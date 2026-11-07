(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

module Flow = Weft_flow.Flow
module Reader = Weft_flow.Reader
module Writer = Weft_flow.Writer
module Error = Cistern_core.Error
module Dialect = Cistern_core.Dialect
module Decoder = Cistern_core.Decoder
module Raw = Cistern_core.Raw
module Exec_result = Cistern_core.Exec_result

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
  { config: Config.t
  ; flow: Flow.t
  ; reader: Reader.t
  ; writer: Writer.t
  ; info: info
  ; mutable status: int
  ; mutable last_error: Message.err option
  ; mutable sent: bool
  ; closed: bool Atomic.t
        (** Set by the first abort or close, on any domain. *)
  ; mutable broken: bool
  ; busy: bool Atomic.t }

let info t = t.info

(* A failure of the connect, with its message. *)
exception Fail of string

let fail fmt = Printf.ksprintf (fun m -> raise (Fail m)) fmt

let native_name = "mysql_native_password"

let sha2_name = "caching_sha2_password"

let first payload = if payload = "" then -1 else Char.code payload.[0]

let packet_text what = function
  | Packet.Sequence {expected; got} ->
      Printf.sprintf
        "%s: a packet with the sequence id %d, not %d (sequence error)" what got
        expected
  | Unsolicited _ ->
      what ^ ": an error packet before the command"
  | Too_long ->
      what ^ ": a packet of 16 MiB or more"

let packet_error what e = raise (Fail (packet_text what e))

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

(* An IP literal (Config.ip_literal, the rule that Tls_setup uses for the name
   of the certificate), or localhost: no DNS resolve. *)
let ip_literal host = host = "localhost" || Config.ip_literal host

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

(* A plugin name of the server, for a message (Wire.peer_text). *)
let plugin_text name = Wire.peer_text ~limit:64 name

let gate_message () =
  "the server asks for full authentication; use VERIFY_IDENTITY with ssl-ca, a \
   pinned certificate (ssl-fingerprint), or allow-cleartext=socket"

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
     autocommit = 1, completion_type = 'NO_CHAIN'"

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
  match round_trip r w "round trip 2" (set_query info.dialect) with
  | Resultset.Ok_packet ok ->
      (info, ok.status)
  | Err_packet e ->
      fail "round trip 2: server error %d" e.code
  | _ ->
      fail "round trip 2: the reply is not an OK"

let establish config opened =
  let flow, transport = open_flow config in
  opened := Some flow ;
  let reader = Packet.reader flow and writer = Packet.writer flow in
  let g = greeting reader in
  authenticate config ~transport reader writer g ;
  let info, status = session reader writer ~thread_id:g.thread_id in
  { config
  ; flow
  ; reader
  ; writer
  ; info
  ; status
  ; last_error= None
  ; sent= false
  ; closed= Atomic.make false
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

let status t = t.status

(* An event of the outcome table, in a command. *)
exception Event of Outcome.event

(* Send one command packet. If the flush raises, the server did not get the
   whole packet. A cancel goes up. *)
let send t command =
  match
    Packet.write t.writer ~seq:0 (fun w -> Message.write_command w command) ;
    Writer.flush t.writer
  with
  | () ->
      ()
  | exception (Flow.Closed | Weft.Net.Error _) ->
      raise (Event Outcome.Write_failed)
  | exception Flow.Timeout ->
      raise (Event Outcome.Write_deadline)

(* A read of the reply. A failure of the transport is the end of the stream.
   Weft.Net.Error comes from an error of the socket that does not end the
   stream, for example ETIMEDOUT after the keep-alive: the tests cannot make it,
   because they do not have the socket of the client. Reader.Too_large cannot
   come, because a packet has at most 0xFFFFFE bytes. Flow.Timeout is the read
   deadline of the pool. *)
let reading f =
  match f () with
  | v ->
      v
  | exception (End_of_file | Flow.Closed | Weft.Net.Error _ | Reader.Too_large)
    ->
      raise (Event Outcome.Stream_ended)
  | exception Flow.Timeout ->
      raise (Event Outcome.Read_deadline)

let bad_packet message = raise (Event (Outcome.Bad_packet message))

(* The reply of a command that gives an OK: COM_PING, COM_INIT_DB,
   COM_RESET_CONNECTION, and the SET of the session. *)
let ok_reply t =
  let p =
    match reading (fun () -> Packet.read t.reader ~seq:1) with
    | Ok p ->
        p
    | Error (Unsolicited _) ->
        raise (Event Outcome.Before_command)
    | Error e ->
        bad_packet (packet_text "reply" e)
  in
  match (first p, Message.ok p, Message.err p) with
  | 0x00, Ok ok, _ ->
      t.status <- ok.status
  | 0xFF, _, Ok e ->
      raise (Event (Outcome.Server_error e))
  | _ ->
      bad_packet "reply: the packet is not an OK or an ERR"

(* The replies of a COM_QUERY, while SERVER_MORE_RESULTS_EXISTS is set. An ERR
   in any part is the result, and the parts before it are dropped. *)
let replies t =
  let rec loop seq acc =
    let reply, next =
      match reading (fun () -> Resultset.read t.reader ~seq) with
      | Ok r ->
          r
      | Error {in_result= false; cause= Packet (Unsolicited _)} ->
          raise (Event Outcome.Before_command)
      | Error {cause= Packet e; _} ->
          bad_packet (packet_text "reply" e)
      | Error {cause= Malformed m; _} ->
          bad_packet ("reply: " ^ m)
    in
    let ok =
      match reply with
      | Resultset.Ok_packet ok ->
          ok
      | Rows r ->
          r.ok
      | Err_packet e ->
          raise (Event (Outcome.Server_error e))
      | Local_infile _ ->
          raise (Event Outcome.Local_infile)
    in
    t.status <- ok.status ;
    let acc = (reply, ok) :: acc in
    if Message.has_status ok.status More_results_exists then loop next acc
    else List.rev acc
  in
  loop 1 []

(* One command on a usable connection. [f] gives the result, or raises Event.
   With [always_break], each error breaks the connection. Another exception (a
   cancel) breaks the connection and goes up. *)
let command ?(always_break = false) ?(statement = false) t kind f =
  with_busy t (fun () ->
      t.last_error <- None ;
      if statement then t.sent <- false ;
      if Atomic.get t.closed || t.broken then Error Error.Connection_lost
      else
        match f () with
        | v ->
            v
        | exception Event e ->
            ( match e with
            | Outcome.Server_error err ->
                t.last_error <- Some err
            | _ ->
                () ) ;
            let error, state = Outcome.decide t.info.dialect kind e in
            if always_break || state = Outcome.Broken then break t ;
            Error error
        | exception e ->
            break t ; raise e )

(* The SQL text of a raw statement, if it has literals and fits in one
   packet. *)
let statement t raw =
  match Raw.to_string raw with
  | Error message ->
      Error (Error.Invalid_value {code= None; message})
  | Ok sql ->
      if
        Packet.query_fits ~max_allowed_packet:t.info.max_allowed_packet
          ~sql_length:(String.length sql)
      then Ok sql
      else
        (* The message names the bound that applies. *)
        let message =
          if t.info.max_allowed_packet < 0xFFFFFF then
            "the statement is too large for @@max_allowed_packet"
          else "the statement is too large for one packet (less than 16 MiB)"
        in
        Error (Error.Invalid_value {code= None; message})

let run_query ?(kind = Outcome.Write) t raw f =
  command ~statement:true t kind (fun () ->
      match statement t raw with
      | Error e ->
          Error e
      | Ok sql ->
          send t (Query sql) ;
          t.sent <- true ;
          f (replies t) )

let query ?kind t raw =
  run_query ?kind t raw (fun replies ->
      (* The first result set. *)
      match
        List.find_map
          (function Resultset.Rows r, _ -> Some r | _ -> None)
          replies
      with
      | Some r -> (
          let rows =
            List.fold_left
              (fun acc row ->
                Result.bind acc (fun rows ->
                    Result.map (fun v -> v :: rows) (Column.row r.columns row) ) )
              (Ok []) r.rows
          in
          match rows with
          | Ok rows ->
              Ok
                { Raw.columns=
                    Array.map (fun (c : Message.column) -> c.name) r.columns
                ; rows= List.rev rows }
          | Error m ->
              raise (Event (Outcome.Bad_value m)) )
      | _ ->
          Ok {Raw.columns= [||]; rows= []} )

let exec ?kind t raw =
  run_query ?kind t raw (fun replies ->
      let _, (ok : Message.ok) = List.nth replies (List.length replies - 1) in
      Ok
        { Exec_result.affected_rows= ok.affected_rows
        ; last_insert_id= ok.last_insert_id } )

let ok_command ?always_break t commands =
  command ?always_break t Outcome.Not_write (fun () ->
      List.iter (fun c -> send t c ; ok_reply t) commands ;
      Ok () )

let ping t = ok_command t [Ping]

let init_db t database = ok_command t [Init_db database]

let reset t =
  ok_command ~always_break:true t
    [ Reset_connection
    ; Query (set_query t.info.dialect)
    ; Init_db (Config.database t.config) ]

(* The first claim of a closed flag gives true, each later claim false, also
   when two domains claim at the same time. *)
let claim closed = not (Atomic.exchange closed true)

module Private = struct
  let claim = claim

  let reset_connection t = ok_command t [Reset_connection]

  let last_error t = t.last_error

  let query_text t raw =
    run_query t raw (fun replies ->
        match
          List.find_map
            (function Resultset.Rows r, _ -> Some r.rows | _ -> None)
            replies
        with
        | Some rows ->
            Ok rows
        | None ->
            Ok [] )
end

let sent t = t.sent

let usable t = not (Atomic.get t.closed || t.broken)

let set_deadline t d =
  Flow.set_read_deadline t.flow d ;
  Flow.set_write_deadline t.flow d

(* No busy flag: abort can run while a command waits in another fiber, which
   then raises Flow.Closed. It sends nothing, never waits and never raises. *)
let abort t =
  let first = claim t.closed in
  Flow.close t.flow ; first

let stale t =
  with_busy t (fun () ->
      let buf = Bytes.create 1 in
      let stale =
        Atomic.get t.closed || t.broken
        || Reader.buffered t.reader > 0
        ||
        match Flow.read_now t.flow buf 0 1 with
        | None ->
            false
        | Some _ ->
            true
        | exception _ ->
            true
      in
      if stale then break t ;
      stale )

let close t =
  with_busy t (fun () ->
      if not (claim t.closed) then false
      else (
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
        Flow.close t.flow ;
        true ) )
