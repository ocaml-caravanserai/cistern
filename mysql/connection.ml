(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

module Flow = Weft_flow.Flow
module Reader = Weft_flow.Reader
module Writer = Weft_flow.Writer
module Error = Cistern_core.Error
module Dialect = Cistern_core.Dialect
module Decoder = Cistern_core.Decoder

type info =
  { thread_id: int
  ; connection_id: int64
  ; version: string
  ; version_comment: string
  ; dialect: Dialect.t
  ; hostname: string
  ; port: int
  ; server_id: int64
  ; wait_timeout: int
  ; innodb_rollback_on_timeout: bool
  ; max_allowed_packet: int }

type transport = Tcp_transport | Unix_transport

type t =
  { flow: Flow.t
  ; reader: Reader.t
  ; writer: Writer.t
  ; info: info
  ; mutable closed: bool
  ; mutable broken: bool
  ; busy: bool Atomic.t }

let info t = t.info

(* A failure of the connect, with its message. *)
exception Fail of string

let fail fmt = Printf.ksprintf (fun m -> raise (Fail m)) fmt

let native_name = "mysql_native_password"

let sha2_name = "caching_sha2_password"

let first payload = if payload = "" then -1 else Char.code payload.[0]

let packet_error what = function
  | Packet.Sequence {expected; got} ->
      fail "%s: a packet with the sequence id %d, not %d (sequence error)" what
        got expected
  | Unsolicited _ ->
      fail "%s: an error packet before the command" what
  | Too_long ->
      fail "%s: a packet of 16 MiB or more" what

let read r ~seq what =
  match Packet.read r ~seq with Ok p -> p | Error e -> packet_error what e

(* A server error during the connect: the number, and not the message, which can
   hold a user name. *)
let server_error what payload =
  match Message.err payload with
  | Ok e ->
      fail "%s: server error %d" what e.code
  | Error m ->
      fail "%s: %s" what m

(* An IPv4 address (four dotted groups of digits), an IPv6 address (Config
   checked its syntax), or localhost: no DNS resolve. *)
let ip_literal host =
  host = "localhost" || String.contains host ':'
  || String.for_all (fun c -> Char.Ascii.is_digit c || c = '.') host
     && List.length (String.split_on_char '.' host) = 4

let open_flow config =
  match Config.address config with
  | Config.Socket {path; _} ->
      (Weft_flow.Tcp.connect (Weft.Net.Unix path), Unix_transport)
  | Tcp {host; port} ->
      let connect h =
        Weft_flow.Tcp.connect
          ~keep_alive:
            (true
            [@mutate
              off
                "the sim stores SO_KEEPALIVE, but no test can read the option \
                 of the client socket"] )
          (Weft.Net.Tcp (h, port))
      in
      let rec try_each = function
        | [] ->
            fail "the host name has no address"
        | [h] ->
            connect h
        | h :: rest -> (
          match connect h with
          | flow ->
              flow
          | exception Weft.Net.Error _ ->
              try_each rest )
      in
      let hosts = if ip_literal host then [host] else Weft.Dns.resolve host in
      (try_each hosts, Tcp_transport)

let greeting r =
  let p = read r ~seq:0 "greeting" in
  if first p = 0xFF then server_error "greeting" p ;
  match Message.greeting p with
  | Error m ->
      fail "%s" m
  | Ok g ->
      List.iter
        (fun (c, name) ->
          if not (Message.has g.capabilities c) then
            fail "the server does not have %s" name )
        [ (Message.Protocol_41, "CLIENT_PROTOCOL_41")
        ; (Secure_connection, "CLIENT_SECURE_CONNECTION")
        ; (Plugin_auth, "CLIENT_PLUGIN_AUTH")
        ; (Deprecate_eof, "CLIENT_DEPRECATE_EOF")
        ; (Connect_with_db, "CLIENT_CONNECT_WITH_DB") ] ;
      g

(* The capabilities of DESIGN.md "Handshake". CLIENT_SSL is never set in phase
   1.1. *)
let client_capabilities () =
  List.fold_left
    (fun acc c -> acc lor Message.bit c)
    0
    [ Message.Protocol_41
    ; Secure_connection
    ; Plugin_auth
    ; Deprecate_eof
    ; Found_rows
    ; Transactions
    ; Connect_with_db
    ; Multi_results ]

let scramble plugin ~password ~nonce =
  if plugin = sha2_name then Scramble.sha2 ~password ~nonce
  else Scramble.native ~password ~nonce

(* A plugin name of the server, for a message: as it is if it has 1 to 64
   printable ASCII bytes. Else the first 64 bytes between double quotes, with
   [\xNN] for each other byte, a double quote and a backslash, and "..." if the
   name is longer. Thus a hostile server cannot put control bytes or a long text
   into a log. (Cistern_names.quote keeps the UTF-8 characters and has no
   limit.) *)
let plugin_text name =
  let printable c = c >= ' ' && c <= '~' && c <> '"' && c <> '\\' in
  let n = String.length name in
  if n >= 1 && n <= 64 && String.for_all printable name then name
  else
    let b = Buffer.create 80 in
    Buffer.add_char b '"' ;
    String.iter
      (fun c ->
        if printable c then Buffer.add_char b c
        else Printf.bprintf b "\\x%02x" (Char.code c) )
      (String.sub name 0 (Int.min n 64)) ;
    Buffer.add_char b '"' ;
    if n > 64 then Buffer.add_string b "..." ;
    Buffer.contents b

let gate_message () =
  "the server asks for full authentication; use VERIFY_IDENTITY, \
   ssl-fingerprint, or allow-cleartext=socket"

(* The clear-text gate. The one place that writes the password calls it. It
   reads the transport of the open connection and the address of the
   configuration: only a Unix socket with allow-cleartext=socket. *)
let may_send_cleartext ~transport config =
  match Config.address config with
  | Config.Socket {allow_cleartext; _} ->
      allow_cleartext && transport = Unix_transport
  | Tcp _ ->
      false

let authenticate config ~transport r w (g : Message.greeting) =
  let password = Config.password config in
  let plugin = if g.auth_plugin = sha2_name then sha2_name else native_name in
  Packet.write w ~seq:1 (fun w ->
      Message.write_handshake_response w
        ~capabilities:(client_capabilities () land g.capabilities)
        ~charset:45 ~user:(Config.user config)
        ~auth:(scramble plugin ~password ~nonce:g.scramble)
        ~database:(Config.database config) ~plugin ) ;
  Writer.flush w ;
  let rec loop ~seq ~plugin ~switched ~fast =
    let p = read r ~seq "authentication" in
    let seq = Packet.next seq in
    match first p with
    | 0x00 -> (
      match Message.ok p with
      | Ok _ ->
          ()
      | Error m ->
          fail "authentication: %s" m )
    | 0xFF ->
        server_error "authentication" p
    | 0xFE -> (
        if switched then fail "authentication: a second AuthSwitchRequest" ;
        match Message.auth_switch p with
        | Error m ->
            fail "authentication: %s" m
        | Ok {plugin= name; data} ->
            if name <> native_name && name <> sha2_name then
              fail "unsupported authentication plugin %s" (plugin_text name) ;
            if String.length data <> 21 || data.[20] <> '\000' then
              fail
                "authentication: the AuthSwitchRequest data is not 20 bytes \
                 and a NUL" ;
            let nonce = String.sub data 0 20 in
            Packet.write w ~seq (fun w ->
                Writer.write_string w (scramble name ~password ~nonce) ) ;
            Writer.flush w ;
            loop ~seq:(Packet.next seq) ~plugin:name ~switched:true ~fast:false
        )
    | 0x01 -> (
        if plugin <> sha2_name then
          fail "authentication: AuthMoreData for %s" plugin ;
        match Message.auth_more_data p with
        | Ok "\x03" when not fast ->
            loop ~seq ~plugin ~switched ~fast:true
        | Ok "\x04" when not fast ->
            if not (may_send_cleartext ~transport config) then
              fail "%s" (gate_message ()) ;
            Packet.write w ~seq (fun w ->
                Writer.write_string w password ;
                Writer.write_uint8 w 0 ) ;
            Writer.flush w ;
            loop ~seq:(Packet.next seq) ~plugin ~switched ~fast
        | _ ->
            fail "authentication: AuthMoreData that the client does not know" )
    | _ ->
        fail "authentication: a packet that the client does not know"
  in
  loop ~seq:2 ~plugin ~switched:false ~fast:false

(* One COM_QUERY of the session and its reply. *)
let round_trip r w what sql =
  Packet.write w ~seq:0 (fun w -> Message.write_command w (Query sql)) ;
  Writer.flush w ;
  match Resultset.read r ~seq:1 with
  | Ok (reply, _) ->
      reply
  | Error {cause= Packet e; _} ->
      packet_error what e
  | Error {cause= Malformed m; _} ->
      fail "%s: %s" what m

let session_query () =
  "SELECT CONNECTION_ID(), @@version, @@version_comment, @@hostname, @@port, \
   @@server_id, @@wait_timeout, @@innodb_rollback_on_timeout, \
   @@max_allowed_packet"

let set_query dialect =
  "SET NAMES utf8mb4 COLLATE " ^ Dialect.collation dialect
  ^ ", time_zone = '+00:00', sql_mode = \
     'STRICT_ALL_TABLES,NO_BACKSLASH_ESCAPES,NO_ZERO_IN_DATE,NO_ZERO_DATE,ERROR_FOR_DIVISION_BY_ZERO,NO_ENGINE_SUBSTITUTION', \
     autocommit = 1"

let session r w ~thread_id =
  let info =
    match round_trip r w "round trip 1" (session_query ()) with
    | Resultset.Rows {rows= [row]; _} -> (
        let decoded =
          Decoder.run
            (fun c ->
              let connection_id = Decoder.next c Decoder.int64 in
              let version = Decoder.next c Decoder.string in
              let version_comment = Decoder.next c Decoder.string in
              let hostname = Decoder.next c Decoder.string in
              let port = Decoder.next c Decoder.int in
              let server_id = Decoder.next c Decoder.int64 in
              let wait_timeout = Decoder.next c Decoder.int in
              let innodb_rollback_on_timeout = Decoder.next c Decoder.bool in
              let max_allowed_packet = Decoder.next c Decoder.int in
              ( connection_id
              , version
              , version_comment
              , hostname
              , port
              , server_id
              , wait_timeout
              , innodb_rollback_on_timeout
              , max_allowed_packet ) )
            row
        in
        match decoded with
        | Error m ->
            fail "round trip 1: %s" m
        | Ok
            ( connection_id
            , version
            , version_comment
            , hostname
            , port
            , server_id
            , wait_timeout
            , innodb_rollback_on_timeout
            , max_allowed_packet ) -> (
          match Dialect.of_version ~version ~version_comment with
          | Error m ->
              fail "round trip 1: %s" m
          | Ok dialect ->
              { thread_id
              ; connection_id
              ; version
              ; version_comment
              ; dialect
              ; hostname
              ; port
              ; server_id
              ; wait_timeout
              ; innodb_rollback_on_timeout
              ; max_allowed_packet } ) )
    | Err_packet e ->
        fail "round trip 1: server error %d" e.code
    | _ ->
        fail "round trip 1: the reply is not one row"
  in
  ( match round_trip r w "round trip 2" (set_query info.dialect) with
  | Resultset.Ok_packet _ ->
      ()
  | Err_packet e ->
      fail "round trip 2: server error %d" e.code
  | _ ->
      fail "round trip 2: the reply is not an OK" ) ;
  info

let establish config opened =
  let flow, transport = open_flow config in
  opened := Some flow ;
  let reader = Packet.reader flow and writer = Packet.writer flow in
  let g = greeting reader in
  authenticate config ~transport reader writer g ;
  let info = session reader writer ~thread_id:g.thread_id in
  { flow
  ; reader
  ; writer
  ; info
  ; closed= false
  ; broken= false
  ; busy= Atomic.make false }

(* The text of a network error, with the constructor and the function. *)
let net_error e f = Printexc.to_string (Weft.Net.Error (e, f))

let connect config =
  if Config.needs_tls config then
    Error (Error.Connect "TLS is not supported before phase 1.4")
  else
    let opened = ref None in
    let close_opened () = Option.iter Flow.close !opened in
    let failed m = close_opened () ; Error (Error.Connect m) in
    match
      Weft.Fiber.with_timeout Weft.Clock.Mono (Weft.Span.s 10) (fun () ->
          establish config opened )
    with
    | t ->
        Ok t
    | exception Fail m ->
        failed m
    | exception Weft.Fiber.Timeout ->
        failed "timeout"
    | exception (End_of_file | Flow.Closed) ->
        failed "the server closed the connection"
    | exception Weft.Net.Error (Weft.Net.No_entry, "Dns.resolve") ->
        (* Weft.Dns.Error is the exception Weft.Net.Error. *)
        failed "the host name is not known"
    | exception Weft.Net.Error (e, f) ->
        failed ("network error: " ^ net_error e f)
    | exception e ->
        close_opened () ; raise e

(* One user: a second call at the same time raises. *)
let with_busy t f =
  if not (Atomic.compare_and_set t.busy false true) then
    invalid_arg "Connection: a second call at the same time" ;
  Fun.protect ~finally:(fun () -> Atomic.set t.busy false) f

(* A broken connection: the flow closes, and each later call gives
   Connection_lost before any I/O. After a failed write, the Writer of weft
   raises Invalid_argument for each later write, thus no call must reach it
   again. *)
let break t =
  t.broken <- true ;
  Flow.close t.flow

(* An exception of the flow, the reader or the writer breaks the connection. A
   cancel goes up. Weft.Net.Error comes from an error of the socket that does
   not end the stream, for example ETIMEDOUT after the keep-alive. The tests
   cannot make it, because they do not have the socket of the client. In phase
   1.1, Flow.Timeout cannot come from ping, because only close sets a write
   timeout. *)
let transport_failure t e =
  break t ;
  match e with
  | End_of_file | Flow.Closed | Flow.Timeout | Weft.Net.Error _ ->
      Error Error.Connection_lost
  | e ->
      raise e

let ping t =
  with_busy t (fun () ->
      if t.closed || t.broken then Error Error.Connection_lost
      else
        match
          Packet.write t.writer ~seq:0 (fun w -> Message.write_command w Ping) ;
          Writer.flush t.writer ;
          Packet.read t.reader ~seq:1
        with
        | exception e ->
            transport_failure t e
        | Error _ ->
            break t ; Error Error.Connection_lost
        | Ok p -> (
          match (first p, Message.ok p, Message.err p) with
          | 0x00, Ok _, _ ->
              Ok ()
          | 0xFF, _, Ok e ->
              let error =
                Error.of_server t.info.dialect ~code:e.code ~sqlstate:e.sqlstate
                  ~message:e.message
              in
              if error = Error.Connection_lost then break t ;
              Error error
          | _ ->
              break t ; Error Error.Connection_lost ) )

let close t =
  with_busy t (fun () ->
      if not t.closed then (
        t.closed <- true ;
        (* A broken connection sends nothing: its flow is closed. *)
        if not t.broken then (
          Flow.set_write_timeout t.flow (Some (Weft.Span.s 1)) ;
          match
            Packet.write t.writer ~seq:0 (fun w -> Message.write_command w Quit) ;
            Writer.flush t.writer
          with
          | () ->
              ()
          | exception (Flow.Closed | Flow.Timeout | Weft.Net.Error _) ->
              (* The server closed the connection, or the write waited 1 s. *)
              ()
          | exception e ->
              (* A cancel, or another exception: the flow closes, so that its fd
                 never stays open. *)
              Flow.close t.flow ; raise e ) ;
        Flow.close t.flow ) )
