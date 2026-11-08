(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* Connection.connect, ping and close against the fake server: the handshake,
   each authentication path and the clear-text gate, the round trips of the
   session, each failure, the address rules, the timeout, and the rules of one
   user. The plan of phase 1.1, sections 3.3, 3.4, 3.9, 3.10 and 4.3. *)

open Windtrap
open Weft
module C = Cistern_mysql.Config
module Conn = Cistern_mysql.Connection
module E = Cistern_core.Error
module F = Fake_server

let password = "secret-pw"

let tcp ?(host = "10.0.0.1") ?(password = password) () =
  Result.get_ok
    (C.make ~kill:false ~tls:C.No_tls
       ~address:(C.Tcp {host; port= 3306})
       ~user:"u" ~password ~database:"db" () )

let sock ?(clear = false) ?(path = "/s") () =
  Result.get_ok
    (C.make ~kill:false
       ~address:(C.Socket {path; allow_cleartext= clear})
       ~user:"u" ~password ~database:"db" () )

let error_text = function
  | E.Connect m ->
      "Connect " ^ m
  | E.Connection_lost ->
      "Connection_lost"
  | e ->
      Format.asprintf "%a" E.pp_summary e

(* The result of a connect, as text. *)
let outcome = function Ok _ -> "connected" | Error e -> error_text e

let contains s sub =
  let n = String.length s and k = String.length sub in
  let rec at i = i + k <= n && (String.sub s i k = sub || at (i + 1)) in
  at 0

(* The bytes of [packets] with their frames. *)
let frames packets =
  String.concat "" (List.map (fun (seq, p) -> F.frame seq p) packets)

(* Each part of 4 bytes of the password. *)
let windows =
  List.init (String.length password - 3) (fun i -> String.sub password i 4)

(* A path that the client refuses: the server got exactly the [expected] packets
   and no other byte up to the end of the stream, and no 4 bytes of the password
   in the stream. *)
let refused expected (received, raw) =
  equal (list (pair int string)) expected received ;
  equal string (frames expected) raw ;
  is_false (List.exists (contains raw) windows)

(* The client has no open fd: its socket is closed. *)
let no_client_fd sim =
  equal ~msg:"the fds of the client" int 0 (F.client_fds sim)

(* Connect, then close. *)
let connect_close config sim =
  let r =
    match Conn.connect config with
    | Ok c ->
        ignore (Conn.close c) ;
        "connected"
    | Error e ->
        error_text e
  in
  no_client_fd sim ; r

let gate =
  "Connect the server asks for full authentication; use VERIFY_IDENTITY with \
   ssl-ca, a pinned certificate (ssl-fingerprint), or allow-cleartext=socket"

let caps_with_mysql = F.caps_mysql

(* The handshake response that the client must send. *)
let response ?(caps = 0x10AA20A) ~plugin ~auth () =
  F.u32 caps ^ F.u32 0x100_0000 ^ "\x2d" ^ String.make 23 '\x00' ^ "u\x00"
  ^ F.lstr auth ^ "db\x00" ^ plugin ^ "\x00"

let native nonce = Cistern_mysql.Scramble.native ~password ~nonce

let sha2 nonce = Cistern_mysql.Scramble.sha2 ~password ~nonce

let rt1 =
  "\x03"
  ^ "SELECT CONNECTION_ID(), @@version, @@version_comment, @@hostname, @@port, \
     @@server_id, @@wait_timeout, @@innodb_rollback_on_timeout, \
     @@max_allowed_packet"

let rt2 collation =
  "\x03SET NAMES utf8mb4 COLLATE " ^ collation
  ^ ", time_zone = '+00:00', sql_mode = \
     'STRICT_ALL_TABLES,NO_BACKSLASH_ESCAPES,NO_ZERO_IN_DATE,NO_ZERO_DATE,ERROR_FOR_DIVISION_BY_ZERO,NO_ENGINE_SUBSTITUTION', \
     autocommit = 1, completion_type = 'NO_CHAIN'"

let mariadb_greeting =
  F.greeting ~version:"11.8.8-MariaDB" ~caps:F.caps_mariadb ~ext:0x3d ()

let mariadb_session =
  F.session ~id:"9" ~version:"11.8.8-MariaDB" ~comment:"MariaDB Server"
    ~max_packet:"16777216" ()

let info_text (i : Conn.info) =
  Printf.sprintf "%d %Ld %s|%s|%s|%s %d %Ld %d %b %d" i.thread_id
    i.connection_id i.version i.version_comment
    (match i.dialect with Mysql -> "mysql" | Mariadb -> "mariadb")
    i.hostname i.port i.server_id i.wait_timeout i.innodb_rollback_on_timeout
    i.max_allowed_packet

let script_auth = [F.Send (0, F.greeting ~plugin:"caching_sha2_password" ())]

(* A script that is the handshake after the authentication packets. *)
let after_auth ~seq values =
  [ F.Send (seq, F.ok ())
  ; F.Recv
  ; F.Sends (1, F.result_set values)
  ; F.Recv
  ; F.Send (1, F.ok ()) ]

let id3 (n, _, _) = n

let id4 (n, _, _, _) = n

let id5 (n, _, _, _, _) = n

let native_response =
  (1, response ~plugin:"mysql_native_password" ~auth:(native F.nonce) ())

let sha2_response =
  (1, response ~plugin:"caching_sha2_password" ~auth:(sha2 F.nonce) ())

let tests =
  group "Connection"
    [ test "Tls_setup.now is the wall clock of weft" (fun () ->
          (* The time of the check of a certificate (phase 1.4). *)
          let t =
            Weft_sim.run (fun sim ->
                Weft_sim.set_posix sim
                  (Weft.Time.posix_of_unix_s 1_750_000_000.) ;
                Cistern_mysql.Tls_setup.now () )
          in
          equal (option string) (Some "2025-06-15T15:06:40Z")
            (Option.map (Ptime.to_rfc3339 ~tz_offset_s:0) t) )
    ; test "native authentication on MariaDB, ping and close" (fun () ->
          let result, received =
            F.run
              ( F.handshake ~greet:mariadb_greeting ~values:mariadb_session ()
              @ [F.Recv; F.Send (1, F.ok ())] )
              (fun _ ->
                match Conn.connect (tcp ()) with
                | Error e ->
                    error_text e
                | Ok c ->
                    let i = info_text (Conn.info c) in
                    let p = Conn.ping c in
                    (* The first close closes. The second does nothing. *)
                    equal bool true (Conn.close c) ;
                    equal bool false (Conn.close c) ;
                    i ^ " ping "
                    ^ Result.fold ~ok:(fun () -> "ok") ~error:error_text p )
          in
          equal string
            "7 9 11.8.8-MariaDB|MariaDB Server|mariadb|host1 3306 1 28800 \
             false 16777216 ping ok"
            result ;
          equal
            (list (pair int string))
            [ ( 1
              , response ~plugin:"mysql_native_password" ~auth:(native F.nonce)
                  () )
            ; (0, rt1)
            ; (0, rt2 "utf8mb4_nopad_bin")
            ; (0, "\x0e")
            ; (0, "\x01") ]
            received )
    ; test "sha2 fast path on MySQL" (fun () ->
          let result, received =
            F.run
              ( script_auth
              @ [F.Recv; F.Send (2, "\x01\x03")]
              @ after_auth ~seq:3 (F.session ()) )
              (fun _ ->
                match Conn.connect (tcp ()) with
                | Ok c ->
                    let i = info_text (Conn.info c) in
                    ignore (Conn.close c) ;
                    i
                | Error e ->
                    error_text e )
          in
          equal string
            "7 42 8.4.11|Source distribution|mysql|host1 3306 1 28800 false \
             67108864"
            result ;
          equal
            (list (pair int string))
            [ ( 1
              , response ~plugin:"caching_sha2_password" ~auth:(sha2 F.nonce) ()
              )
            ; (0, rt1)
            ; (0, rt2 "utf8mb4_0900_bin")
            ; (0, "\x01") ]
            received )
    ; test "sha2 full path on a socket with the opt-in" (fun () ->
          let result, received =
            F.run ~listen:"unix:/s"
              ( script_auth
              @ [F.Recv; F.Send (2, "\x01\x04"); F.Recv]
              @ after_auth ~seq:4 (F.session ()) )
              (connect_close (sock ~clear:true ()))
          in
          equal string "connected" result ;
          equal (pair int string) (3, password ^ "\x00") (List.nth received 1) )
    ; cases "the clear-text gate" ~id:id3
        [ ( "TCP and 01 04"
          , (fun () ->
              F.run_raw
                (script_auth @ [F.Recv; F.Send (2, "\x01\x04"); F.Recv])
                (connect_close (tcp ())) )
          , [sha2_response] )
        ; ( "a socket without the opt-in"
          , (fun () ->
              F.run_raw ~listen:"unix:/s"
                (script_auth @ [F.Recv; F.Send (2, "\x01\x04"); F.Recv])
                (connect_close (sock ())) )
          , [sha2_response] )
        ; ( "TCP, a switch to sha2, then 01 04"
          , (fun () ->
              F.run_raw
                ( [F.Send (0, F.greeting ())]
                @ [ F.Recv
                  ; F.Send
                      ( 2
                      , "\xfecaching_sha2_password\x00" ^ String.make 20 'n'
                        ^ "\x00" )
                  ; F.Recv
                  ; F.Send (4, "\x01\x04")
                  ; F.Recv ] )
                (connect_close (tcp ())) )
          , [native_response; (3, sha2 (String.make 20 'n'))] ) ]
        (fun (_, run, expected) ->
          let result, received, raw = run () in
          equal string gate result ;
          refused expected (received, raw) )
    ; cases "AuthMoreData that the client refuses" ~id:id4
        [ ( "the native plugin"
          , F.greeting ()
          , "\x01\x04"
          , "Connect authentication: AuthMoreData for mysql_native_password" )
        ; ( "01 02"
          , F.greeting ~plugin:"caching_sha2_password" ()
          , "\x01\x02"
          , "Connect authentication: AuthMoreData that the client does not know"
          )
        ; ( "a public key"
          , F.greeting ~plugin:"caching_sha2_password" ()
          , "\x01-----BEGIN PUBLIC KEY-----"
          , "Connect authentication: AuthMoreData that the client does not know"
          ) ]
        (fun (_, greet, more, expected) ->
          let result, received, raw =
            F.run_raw
              [F.Send (0, greet); F.Recv; F.Send (2, more); F.Recv]
              (connect_close (sock ~clear:true ()))
              ~listen:"unix:/s"
          in
          equal string expected result ;
          refused
            [(if greet = F.greeting () then native_response else sha2_response)]
            (received, raw) )
    ; test "01 03 then 01 04" (fun () ->
          let result, received, raw =
            F.run_raw ~listen:"unix:/s"
              ( script_auth
              @ [F.Recv; F.Send (2, "\x01\x03"); F.Send (3, "\x01\x04"); F.Recv]
              )
              (connect_close (sock ~clear:true ()))
          in
          equal string
            "Connect authentication: AuthMoreData that the client does not know"
            result ;
          refused [sha2_response] (received, raw) )
    ; test "a switch to native" (fun () ->
          let nonce2 = "abcdefghijklmnopqrst" in
          let result, received =
            F.run
              ( [ F.Send (0, F.greeting ~plugin:"caching_sha2_password" ())
                ; F.Recv
                ; F.Send (2, "\xfemysql_native_password\x00" ^ nonce2 ^ "\x00")
                ; F.Recv ]
              @ after_auth ~seq:4 (F.session ()) )
              (connect_close (tcp ()))
          in
          equal string "connected" result ;
          equal (pair int string) (3, native nonce2) (List.nth received 1) )
    ; cases "switches that the client refuses" ~id:id3
        [ ( "21 bytes"
          , "\xfemysql_native_password\x00" ^ String.make 21 'n'
          , "Connect authentication: the AuthSwitchRequest data is not 20 \
             bytes and a NUL" )
        ; ( "20 bytes and no NUL"
          , "\xfemysql_native_password\x00" ^ String.make 20 'n'
          , "Connect authentication: the AuthSwitchRequest data is not 20 \
             bytes and a NUL" )
        ; ( "mysql_clear_password"
          , "\xfemysql_clear_password\x00"
          , "Connect unsupported authentication plugin mysql_clear_password" )
        ; ( "dialog"
          , "\xfedialog\x00"
          , "Connect unsupported authentication plugin dialog" )
        ; ( "sha256_password"
          , "\xfesha256_password\x00" ^ String.make 20 'n' ^ "\x00"
          , "Connect unsupported authentication plugin sha256_password" )
        ; ( "unix_socket"
          , "\xfeunix_socket\x00"
          , "Connect unsupported authentication plugin unix_socket" )
        ; ( "the old 1-byte switch"
          , "\xfe"
          , "Connect unsupported authentication plugin mysql_old_password" )
        ; ( "no NUL after the name"
          , "\xfeabc"
          , "Connect authentication: AuthSwitchRequest: the packet is too short"
          )
        ; ( "a name of 64 bytes"
          , "\xfe" ^ String.make 64 'p' ^ "\x00"
          , "Connect unsupported authentication plugin " ^ String.make 64 'p' )
        ; ( "a name of 65 bytes"
          , "\xfe" ^ String.make 65 'p' ^ "\x00"
          , "Connect unsupported authentication plugin \"" ^ String.make 64 'p'
            ^ "\"..." )
        ; ( "an empty name"
          , "\xfe\x00"
          , "Connect unsupported authentication plugin \"\"" )
        ; ( "control bytes, a quote and a backslash"
          , "\xfe\x1b[2Ja\"b\\c\x7f\x00"
          , "Connect unsupported authentication plugin \
             \"\\x1b[2Ja\\x22b\\x5cc\\x7f\"" )
        ; ( "UTF-8 and a space"
          , "\xfeplug\xc3\xa9 x\x00"
          , "Connect unsupported authentication plugin \"plug\\xc3\\xa9 x\"" )
        ; ( "one byte"
          , "\xfe~\x00"
          , "Connect unsupported authentication plugin ~" )
        ; ( "64 bytes with a control byte"
          , "\xfe" ^ String.make 63 'p' ^ "\x01\x00"
          , "Connect unsupported authentication plugin \"" ^ String.make 63 'p'
            ^ "\\x01\"" )
        ; ( "printable ASCII"
          , "\xfe!~\x00"
          , "Connect unsupported authentication plugin !~" ) ]
        (fun (_, switch, expected) ->
          let result, received, raw =
            F.run_raw
              [F.Send (0, F.greeting ()); F.Recv; F.Send (2, switch); F.Recv]
              (connect_close (tcp ()))
          in
          equal string expected result ;
          refused [native_response] (received, raw) )
    ; test "a second switch" (fun () ->
          let switch =
            "\xfemysql_native_password\x00" ^ String.make 20 'n' ^ "\x00"
          in
          let result, received, raw =
            F.run_raw
              [ F.Send (0, F.greeting ())
              ; F.Recv
              ; F.Send (2, switch)
              ; F.Recv
              ; F.Send (4, switch)
              ; F.Recv ]
              (connect_close (tcp ()))
          in
          equal string "Connect authentication: a second AuthSwitchRequest"
            result ;
          refused
            [native_response; (3, native (String.make 20 'n'))]
            (received, raw) )
    ; cases "greetings" ~id:id3
        [ ( "protocol 9"
          , F.greeting ~protocol:9 ()
          , "Connect the greeting is not protocol 10" )
        ; ( "no PROTOCOL_41"
          , F.greeting ~caps:(F.caps_mysql land lnot 0x200) ()
          , "Connect the server does not have CLIENT_PROTOCOL_41" )
        ; ( "no SECURE_CONNECTION"
          , F.greeting ~caps:(F.caps_mysql land lnot 0x8000) ()
          , "Connect the server does not have CLIENT_SECURE_CONNECTION" )
        ; ( "no PLUGIN_AUTH"
          , F.greeting ~caps:(F.caps_mysql land lnot 0x80000) ()
          , "Connect the server does not have CLIENT_PLUGIN_AUTH" )
        ; ( "no DEPRECATE_EOF"
          , F.greeting ~caps:(F.caps_mysql land lnot 0x1000000) ()
          , "Connect the server does not have CLIENT_DEPRECATE_EOF" )
        ; ( "no CONNECT_WITH_DB"
          , F.greeting ~caps:(F.caps_mysql land lnot 0x8) ()
          , "Connect the server does not have CLIENT_CONNECT_WITH_DB" )
        ; ( "an ERR"
          , F.err 1040 "Too many connections"
          , "Connect greeting: server error 1040" )
        ; ( "a bad ERR"
          , "\xff\x10"
          , "Connect greeting: ERR: the packet is too short" ) ]
        (fun (_, greet, expected) ->
          let result, received, raw =
            F.run_raw [F.Send (0, greet); F.Recv] (connect_close (tcp ()))
          in
          equal string expected result ;
          (* The client sends nothing. *)
          refused [] (received, raw) )
    ; test "the capabilities that the server does not have" (fun () ->
          (* No FOUND_ROWS, TRANSACTIONS and MULTI_RESULTS. *)
          let caps = F.caps_mysql land lnot (0x2 lor 0x2000 lor 0x20000) in
          let result, received =
            F.run
              (F.handshake ~greet:(F.greeting ~caps ()) ())
              (connect_close (tcp ()))
          in
          equal string "connected" result ;
          equal (pair int string)
            ( 1
            , response ~caps:0x1088208 ~plugin:"mysql_native_password"
                ~auth:(native F.nonce) () )
            (List.hd received) )
    ; test "an empty password, and a switch" (fun () ->
          let nonce2 = "abcdefghijklmnopqrst" in
          let result, received =
            F.run
              ( [ F.Send (0, F.greeting ())
                ; F.Recv
                ; F.Send (2, "\xfecaching_sha2_password\x00" ^ nonce2 ^ "\x00")
                ; F.Recv ]
              @ after_auth ~seq:4 (F.session ()) )
              (connect_close (tcp ~password:"" ()))
          in
          equal string "connected" result ;
          (* The two auth responses are empty. *)
          equal
            (list (pair int string))
            [ (1, response ~plugin:"mysql_native_password" ~auth:"" ())
            ; (3, "")
            ; (0, rt1)
            ; (0, rt2 "utf8mb4_0900_bin")
            ; (0, "\x01") ]
            received )
    ; test "a greeting with a wrong sequence id" (fun () ->
          let result, _ =
            F.run [F.Send (1, F.greeting ()); F.Recv] (connect_close (tcp ()))
          in
          equal string
            "Connect greeting: a packet with the sequence id 1, not 0 \
             (sequence error)"
            result )
    ; test "an unknown plugin in the greeting, and MariaDB capabilities"
        (fun () ->
          let result, received =
            F.run
              (F.handshake
                 ~greet:
                   (F.greeting ~caps:F.caps_mariadb ~ext:0x3d ~plugin:"ed25519"
                      () )
                 ~values:mariadb_session () )
              (connect_close (tcp ()))
          in
          equal string "connected" result ;
          equal (pair int string)
            ( 1
            , response ~plugin:"mysql_native_password" ~auth:(native F.nonce) ()
            )
            (List.hd received) )
    ; cases "errors of the server" ~id:id3
        [ ( "an empty packet at the authentication"
          , [F.Send (0, F.greeting ()); F.Recv; F.Send (2, ""); F.Recv]
          , "Connect authentication: a packet that the client does not know" )
        ; ( "at the authentication"
          , [ F.Send (0, F.greeting ())
            ; F.Recv
            ; F.Send (2, F.err 1045 "Access denied for user 'u'")
            ; F.Recv ]
          , "Connect authentication: server error 1045" )
        ; ( "a bad OK at the authentication"
          , [F.Send (0, F.greeting ()); F.Recv; F.Send (2, "\x00\x00"); F.Recv]
          , "Connect authentication: OK: the packet is too short" )
        ; ( "an unknown packet at the authentication"
          , [F.Send (0, F.greeting ()); F.Recv; F.Send (2, "\x07"); F.Recv]
          , "Connect authentication: a packet that the client does not know" )
        ; ( "a sequence error at the authentication"
          , [F.Send (0, F.greeting ()); F.Recv; F.Send (5, F.ok ()); F.Recv]
          , "Connect authentication: a packet with the sequence id 5, not 2 \
             (sequence error)" )
        ; ( "at round trip 1"
          , [ F.Send (0, F.greeting ())
            ; F.Recv
            ; F.Send (2, F.ok ())
            ; F.Recv
            ; F.Send (1, F.err 1146 "no table")
            ; F.Recv ]
          , "Connect round trip 1: server error 1146" )
        ; ( "an unsolicited error at round trip 1"
          , [ F.Send (0, F.greeting ())
            ; F.Recv
            ; F.Send (2, F.ok ())
            ; F.Recv
            ; F.Send (0, F.err 4031 "idle")
            ; F.Recv ]
          , "Connect round trip 1: an error packet before the command" )
        ; ( "the server closes at round trip 1"
          , [ F.Send (0, F.greeting ())
            ; F.Recv
            ; F.Send (2, F.ok ())
            ; F.Recv
            ; F.Close ]
          , "Connect the server closed the connection" )
        ; ( "a bad column count at round trip 1"
          , [ F.Send (0, F.greeting ())
            ; F.Recv
            ; F.Send (2, F.ok ())
            ; F.Recv
            ; F.Send (1, "\x01\x00")
            ; F.Recv ]
          , "Connect round trip 1: the column count is not valid" )
        ; ( "an OK at round trip 1"
          , [ F.Send (0, F.greeting ())
            ; F.Recv
            ; F.Send (2, F.ok ())
            ; F.Recv
            ; F.Send (1, F.ok ())
            ; F.Recv ]
          , "Connect round trip 1: the reply is not one row" )
        ; ( "two rows at round trip 1"
          , [ F.Send (0, F.greeting ())
            ; F.Recv
            ; F.Send (2, F.ok ())
            ; F.Recv
            ; F.Sends
                ( 1
                , ("\x09" :: List.init 9 (fun i -> F.column (string_of_int i)))
                  @ [F.row (F.session ()); F.row (F.session ()); F.terminator]
                )
            ; F.Recv ]
          , "Connect round trip 1: the reply is not one row" )
        ; ( "a NULL at round trip 1"
          , F.handshake
              ~values:
                ( F.session ()
                |> List.mapi (fun i v -> if i = 2 then None else v) )
              ()
          , "Connect round trip 1: column 2: NULL in a column that is not \
             optional" )
        ; ( "a version with no numbers"
          , F.handshake ~values:(F.session ~version:"eight" ()) ()
          , "Connect round trip 1: the server version does not start with \
             <major>.<minor>.<patch>" )
        ; ( "a max_allowed_packet that is not a number"
          , F.handshake ~values:(F.session ~max_packet:"big" ()) ()
          , "Connect round trip 1: column 8: expected an integer in the range \
             of int" )
        ; ( "at round trip 2"
          , [ F.Send (0, F.greeting ())
            ; F.Recv
            ; F.Send (2, F.ok ())
            ; F.Recv
            ; F.Sends (1, F.result_set (F.session ()))
            ; F.Recv
            ; F.Send (1, F.err 1231 "bad sql_mode")
            ; F.Recv ]
          , "Connect round trip 2: server error 1231" )
        ; ( "rows at round trip 2"
          , [ F.Send (0, F.greeting ())
            ; F.Recv
            ; F.Send (2, F.ok ())
            ; F.Recv
            ; F.Sends (1, F.result_set (F.session ()))
            ; F.Recv
            ; F.Sends (1, F.result_set (F.session ()))
            ; F.Recv ]
          , "Connect round trip 2: the reply is not an OK" ) ]
        (fun (_, script, expected) ->
          let result, _ = F.run script (connect_close (tcp ())) in
          equal string expected result )
    ; test "a packet of 16 MiB" (fun () ->
          let result, _ =
            F.run
              [ F.Send (0, F.greeting ())
              ; F.Recv
              ; F.Send (2, F.ok ())
              ; F.Recv
              ; F.Raw "\xff\xff\xff\x01"
              ; F.Recv ]
              (connect_close (tcp ()))
          in
          equal string "Connect round trip 1: a packet of 16 MiB or more" result )
    ; cases "the end of the stream and a reset" ~id:id3
        [ ( "before the greeting"
          , [F.Close]
          , "Connect the server closed the connection" )
        ; ( "after the greeting"
          , [F.Send (0, F.greeting ()); F.Recv; F.Close]
          , "Connect the server closed the connection" )
        ; ( "a reset after the greeting"
          , [F.Send (0, F.greeting ()); F.Recv; F.Reset]
          , "Connect the server closed the connection" )
        ; ( "in round trip 1"
          , [ F.Send (0, F.greeting ())
            ; F.Recv
            ; F.Send (2, F.ok ())
            ; F.Recv
            ; F.Sends (1, ["\x09"])
            ; F.Close ]
          , "Connect the server closed the connection" ) ]
        (fun (_, script, expected) ->
          let result, _ = F.run script (connect_close (tcp ())) in
          equal string expected result )
    ; cases "the timeout covers the whole connect" ~id:fst
        [ ("no greeting", [F.Hang])
        ; ("at the authentication", [F.Send (0, F.greeting ()); F.Hang])
        ; ( "at round trip 1"
          , [F.Send (0, F.greeting ()); F.Recv; F.Send (2, F.ok ()); F.Hang] )
        ; ( "at round trip 2"
          , [ F.Send (0, F.greeting ())
            ; F.Recv
            ; F.Send (2, F.ok ())
            ; F.Recv
            ; F.Sends (1, F.result_set (F.session ()))
            ; F.Hang ] ) ]
        (fun (_, script) ->
          let result, _ =
            F.run script (fun sim ->
                let t0 = Time.now Mono in
                let r = connect_close (tcp ()) sim in
                Printf.sprintf "%s %b" r
                  (Span.equal (Time.diff Mono (Time.now Mono) t0) (Span.s 10)) )
          in
          equal string "Connect timeout true" result )
    ; test "a DNS server that does not answer" (fun () ->
          let result, _ =
            F.run
              ~dns:[("slow", Weft_sim.No_answer)]
              []
              (fun sim ->
                let t0 = Time.now Mono in
                let r = connect_close (tcp ~host:"slow" ()) sim in
                Printf.sprintf "%s %b" r
                  (Span.equal (Time.diff Mono (Time.now Mono) t0) (Span.s 10)) )
          in
          equal string "Connect timeout true" result )
    ; cases "addresses" ~id:id5
        [ ( "a name of digits"
          , "srv:3306"
          , [("123", Weft_sim.Addresses ["srv"])]
          , tcp ~host:"123" ()
          , "connected" )
        ; ( "a name of four dotted parts"
          , "srv:3306"
          , [("a.b.c.d", Weft_sim.Addresses ["srv"])]
          , tcp ~host:"a.b.c.d" ()
          , "connected" )
        ; ( "localhost"
          , "localhost:3306"
          , []
          , tcp ~host:"localhost" ()
          , "connected" )
        ; ( "a name"
          , "srv:3306"
          , [("db", Weft_sim.Addresses ["srv"])]
          , tcp ~host:"db" ()
          , "connected" )
        ; ( "a name with two addresses, the first refuses"
          , "srv:3306"
          , [("db", Weft_sim.Addresses ["down"; "srv"])]
          , tcp ~host:"db" ()
          , "connected" )
        ; ("an IPv6 address", "[::1]:3306", [], tcp ~host:"::1" (), "connected")
        ]
        (fun (_, listen, dns, config, expected) ->
          let result, _ =
            F.run ~listen ~dns (F.handshake ()) (connect_close config)
          in
          equal string expected result )
    ; test "two addresses that accept: the first gets the connection" (fun () ->
          (* srv2 accepts no connection: a connect to it gets no greeting. *)
          let result, _ =
            F.run ~listen:"srv1:3306" ~extra:["srv2:3306"]
              ~dns:[("db", Weft_sim.Addresses ["srv1"; "srv2"])]
              (F.handshake ())
              (connect_close (tcp ~host:"db" ()))
          in
          equal string "connected" result )
    ; cases "short reads and writes" ~id:id5
        [ ( "native authentication, ping and close"
          , "10.0.0.1:3306"
          , F.handshake ~greet:mariadb_greeting ~values:mariadb_session ()
            @ [F.Recv; F.Send (1, F.ok ())]
          , tcp ()
          , [ native_response
            ; (0, rt1)
            ; (0, rt2 "utf8mb4_nopad_bin")
            ; (0, "\x0e")
            ; (0, "\x01") ] )
        ; ( "sha2 full path on a socket"
          , "unix:/s"
          , script_auth
            @ [F.Recv; F.Send (2, "\x01\x04"); F.Recv]
            @ after_auth ~seq:4 (F.session ())
          , sock ~clear:true ()
          , [ sha2_response
            ; (3, password ^ "\x00")
            ; (0, rt1)
            ; (0, rt2 "utf8mb4_0900_bin")
            ; (0, "\x01") ] ) ]
        (fun (_, listen, script, config, expected) ->
          let result, received =
            F.run ~short_io:true ~listen script (fun sim ->
                match Conn.connect config with
                | Error e ->
                    error_text e
                | Ok c ->
                    let p = if listen = "unix:/s" then Ok () else Conn.ping c in
                    ignore (Conn.close c) ;
                    no_client_fd sim ;
                    Result.fold ~ok:(fun () -> "ok") ~error:error_text p )
          in
          equal string "ok" result ;
          equal (list (pair int string)) expected received )
    ; cases "addresses that fail" ~id:id3
        [ ( "an unknown name"
          , tcp ~host:"nowhere" ()
          , "Connect the host name is not known" )
        ; ( "a name with no address"
          , tcp ~host:"empty" ()
          , "Connect the host name is not known" )
        ; ( "a refused connect"
          , tcp ~host:"10.0.0.2" ()
          , "Connect network error: Weft.Net.Error(Refused, \"Net.connect\")" )
        ; ( "a socket path of 108 bytes"
          , sock ~path:("/" ^ String.make 107 's') ()
          , "Connect network error: Weft.Net.Error(Other \"ENAMETOOLONG\", \
             \"Net.connect\")" ) ]
        (fun (_, config, expected) ->
          let result, _ =
            F.run
              ~dns:[("empty", Weft_sim.Addresses [])]
              [] (connect_close config)
          in
          equal string expected result )
    ; test "the CA file comes before the DNS resolve and a socket" (fun () ->
          (* Phase 1.4: the CA file is read first, inside the limit of 10 s. *)
          let config =
            Tls_fixture.config ~host:"slow" ~ca:"/nonexistent/ca.pem"
              Verify_identity
          in
          let result, _ =
            F.run
              ~dns:[("slow", Weft_sim.No_answer)]
              []
              (fun sim ->
                let fds = Weft_sim.open_fds sim and t0 = Time.now Mono in
                let r = connect_close config sim in
                Printf.sprintf "%s %b %b" r
                  (Weft_sim.open_fds sim = fds)
                  (Span.equal (Time.diff Mono (Time.now Mono) t0) Span.zero) )
          in
          equal string "Connect the CA file does not exist true true" result )
    ; test "a cancel of the connect closes the socket" (fun () ->
          let result, _ =
            F.run
              [F.Send (0, F.greeting ()); F.Hang]
              (fun sim ->
                let fds = Weft_sim.open_fds sim in
                match
                  Fiber.with_timeout Mono (Span.s 1) (fun () ->
                      Conn.connect (tcp ()) )
                with
                | _ ->
                    "no timeout"
                | exception Fiber.Timeout ->
                    (* The server keeps its side open: one fd more. *)
                    Printf.sprintf "timeout %b" (Weft_sim.open_fds sim = fds + 1) )
          in
          equal string "timeout true" result ) ]

(* A connection after the handshake of [F.handshake], then [steps] of the server
   and [client] on the connection. *)
let with_connection steps client =
  F.run
    (F.handshake () @ steps)
    (fun sim ->
      match Conn.connect (tcp ()) with
      | Error e ->
          "connect: " ^ error_text e
      | Ok c ->
          let r = client sim c in
          no_client_fd sim ; r )

let ping_text c =
  match Conn.ping c with Ok () -> "ok" | Error e -> error_text e

let quits received =
  List.length (List.filter (fun (_, p) -> p = "\x01") received)

let pings received =
  List.length (List.filter (fun (_, p) -> p = "\x0e") received)

let ping_tests =
  group "ping and close"
    [ test "an error that leaves the connection usable" (fun () ->
          let result, received =
            with_connection
              [ F.Recv
              ; F.Send (1, F.err 1317 "interrupted")
              ; F.Recv
              ; F.Send (1, F.ok ()) ]
              (fun _ c ->
                let a = ping_text c in
                let b = ping_text c in
                ignore (Conn.close c) ;
                a ^ " then " ^ b )
          in
          equal string "Driver 1317 28000 then ok" result ;
          equal int 1 (quits received) )
    ; cases "replies that break the connection" ~id:fst
        [ ( "an error that the server ends the session with"
          , [F.Recv; F.Send (1, F.err 1053 "shutdown")] )
        ; ("the end of the stream", [F.Recv; F.Close])
        ; ("a reset", [F.Recv; F.Reset])
        ; ("a packet that is not an OK or an ERR", [F.Recv; F.Send (1, "\x07")])
        ; ("a bad OK", [F.Recv; F.Send (1, "\x00")])
        ; ("a bad ERR", [F.Recv; F.Send (1, "\xff\x01")])
        ; ("a sequence error", [F.Recv; F.Send (2, F.ok ())])
        ; ( "an error before the command (R-SEQ0)"
          , [F.Send (0, F.err 4031 "idle")] ) ]
        (fun (_, steps) ->
          let result, received =
            with_connection steps (fun _ c ->
                let a = ping_text c in
                let b = ping_text c in
                let c3 = ping_text c in
                ignore (Conn.close c) ;
                String.concat " then " [a; b; c3] )
          in
          equal string
            "Connection_lost then Connection_lost then Connection_lost" result ;
          (* The later pings and the close sent nothing. *)
          equal int 1 (pings received) ;
          equal int 0 (quits received) )
    ; test "a failed write breaks the connection" (fun () ->
          let result, received =
            with_connection [F.Reset] (fun _ c ->
                (* The reset comes to the client before the first ping. *)
                Time.sleep Mono (Span.ms 1) ;
                let a = ping_text c in
                let b = ping_text c in
                let c3 = ping_text c in
                ignore (Conn.close c) ;
                String.concat " then " [a; b; c3] )
          in
          equal string
            "Connection_lost then Connection_lost then Connection_lost" result ;
          equal int 0 (pings received) )
    ; test "ping after close" (fun () ->
          let result, received =
            with_connection [] (fun _ c ->
                ignore (Conn.close c) ;
                ping_text c )
          in
          equal string "Connection_lost" result ;
          equal int 0 (pings received) ;
          equal int 1 (quits received) )
    ; test "a second call at the same time" (fun () ->
          let result, _ =
            with_connection
              [F.Recv; F.Send (1, F.ok ())]
              (fun _ c ->
                Fiber.scope (fun s ->
                    let first = Fiber.fork_promise s (fun () -> ping_text c) in
                    Fiber.yield () ;
                    let second =
                      match Conn.ping c with
                      | _ ->
                          "no error"
                      | exception Invalid_argument m ->
                          m
                    in
                    let first = Promise.await first in
                    ignore (Conn.close c) ;
                    first ^ " / " ^ second ) )
          in
          equal string "ok / Connection: a second call at the same time" result )
    ; test "a cancel of a ping breaks the connection" (fun () ->
          let result, received =
            with_connection [F.Recv; F.Hang] (fun _ c ->
                let a =
                  match
                    Fiber.with_timeout Mono (Span.s 1) (fun () -> Conn.ping c)
                  with
                  | _ ->
                      "no timeout"
                  | exception Fiber.Timeout ->
                      "timeout"
                in
                let b = ping_text c in
                ignore (Conn.close c) ;
                a ^ " then " ^ b )
          in
          equal string "timeout then Connection_lost" result ;
          equal int 1 (pings received) ;
          equal int 0 (quits received) )
    ; test "a COM_QUIT that cannot go waits 1 s" (fun () ->
          (* The server reads nothing after the greeting. In the simulator, the
             receive buffer of a connection holds 65536 bytes: the constant
             [capacity] of weft_sim.ml, which weft_sim.mli does not document. If
             weft changes it, this test fails (the pings or the time). The
             client fills the buffer with its packets and with pings to 0 to 4
             bytes from the end, thus the 5 bytes of COM_QUIT cannot go. *)
          let sent =
            String.length
              (frames [native_response; (0, rt1); (0, rt2 "utf8mb4_0900_bin")])
          in
          let n = (65536 - sent) / 5 in
          let result, received =
            F.run
              [ F.Send (0, F.greeting ())
              ; F.Send (2, F.ok ())
              ; F.Sends (1, F.result_set (F.session ()))
              ; F.Send (1, F.ok ())
              ; F.Raw
                  (String.concat ""
                     (List.init n (fun _ -> F.frame 1 (F.ok ()))) )
              ; F.Hang ]
              (fun sim ->
                match Conn.connect (tcp ()) with
                | Error e ->
                    error_text e
                | Ok c ->
                    let pings =
                      List.init n (fun _ -> ping_text c)
                      |> List.for_all (( = ) "ok")
                    in
                    let t0 = Time.now Mono in
                    let r =
                      match
                        Fiber.with_timeout Mono (Span.s 5) (fun () ->
                            ignore (Conn.close c) )
                      with
                      | () ->
                          Printf.sprintf "%b closed after 1 s: %b" pings
                            (Span.equal
                               (Time.diff Mono (Time.now Mono) t0)
                               (Span.s 1) )
                      | exception Fiber.Timeout ->
                          "close waits"
                    in
                    no_client_fd sim ; r )
          in
          equal string "true closed after 1 s: true" result ;
          equal int 0 (List.length received) )
    ; test "a close whose COM_QUIT fails" (fun () ->
          let result, _ =
            with_connection [F.Reset] (fun _ c ->
                Time.sleep Mono (Span.ms 1) ;
                ignore (Conn.close c) ;
                "closed" )
          in
          equal string "closed" result )
    ; test "a close in a cancelled fiber" (fun () ->
          let result, received =
            with_connection [] (fun _ c ->
                match
                  Fiber.with_timeout Mono (Span.ms 1) (fun () ->
                      try Time.sleep Mono (Span.s 5)
                      with Fiber.Cancelled _ as e ->
                        ignore (Conn.close c) ;
                        raise e )
                with
                | () ->
                    "no timeout"
                | exception Fiber.Timeout ->
                    "timeout " ^ ping_text c )
          in
          equal string "timeout Connection_lost" result ;
          equal int 0 (quits received) )
    ; test "a failed connect closes its socket" (fun () ->
          let result, _ =
            F.run
              [ F.Send (0, F.greeting ())
              ; F.Recv
              ; F.Send (2, F.err 1045 "denied")
              ; F.Recv ]
              (fun sim ->
                let fds = Weft_sim.open_fds sim in
                let r = connect_close (tcp ()) sim in
                (* The server keeps its side open: one fd more. *)
                Printf.sprintf "%s %b" r (Weft_sim.open_fds sim = fds + 1) )
          in
          equal string "Connect authentication: server error 1045 true" result )
    ]
