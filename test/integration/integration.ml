(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* The integration suite: Cistern's own client against the server of make db-up
   whose env file CISTERN_DB_ENV names (an absolute path). The plan of phase
   1.1, section 4.4, and the decisions B7 and B9.

   On MySQL, a TCP connection with ssl-mode=DISABLED can use only the fast path
   of caching_sha2_password, thus each TCP test first warms the cache with a
   connect on the socket with allow-cleartext=socket. Then the tests do not
   depend on their order. From phase 1.4, the TCP tests use TLS (on_tls:
   VERIFY_IDENTITY with the test CA), and the tests of the clear-text rule keep
   ssl-mode=DISABLED (on_tcp). *)

open Windtrap
open Support

let query_tests =
  group ("queries on " ^ server)
    [ test "each column type" (fun () ->
          in_weft (fun () ->
              warm () ;
              List.iter
                (fun config ->
                  with_conn config (fun c ->
                      exec_ok c "DROP TABLE IF EXISTS tt" ;
                      exec_ok c create_types ;
                      equal string "1 1" (exec_text (Conn.exec c insert_types)) ;
                      equal string (types_expected ())
                        (rows_text (Conn.query c (Raw.sql select_types))) ;
                      exec_ok c "DROP TABLE tt" ) )
                [on_socket ~clear:true (); on_tls ()] ) )
    ; test "last_insert_id, the found rows and status" (fun () ->
          in_weft (fun () ->
              warm () ;
              with_conn (on_socket ~clear:true ()) (fun c ->
                  exec_ok c "DROP TABLE IF EXISTS ai" ;
                  exec_ok c
                    "CREATE TABLE ai (id INT AUTO_INCREMENT PRIMARY KEY, v INT)" ;
                  let ins = Raw.sql "INSERT INTO ai (v) VALUES (1), (1)" in
                  (* The first value of the last INSERT. *)
                  equal string "2 1" (exec_text (Conn.exec c ins)) ;
                  equal string "2 3" (exec_text (Conn.exec c ins)) ;
                  (* CLIENT_FOUND_ROWS: the rows that the UPDATE found, also
                     with no change. *)
                  equal string "4 0"
                    (exec_text (Conn.exec c (Raw.sql "UPDATE ai SET v = 1"))) ;
                  exec_ok c "START TRANSACTION" ;
                  is_true (Conn.status c land 1 = 1) ;
                  exec_ok c "COMMIT" ;
                  is_true (Conn.status c land 1 = 0) ;
                  equal string "Driver 1146 42S02"
                    (rows_text (Conn.query c (Raw.sql "SELECT * FROM nowhere"))) ;
                  equal string "ok" (unit_text (Conn.ping c)) ;
                  exec_ok c "DROP TABLE ai" ) ) )
    ; test "init_db and reset" (fun () ->
          in_weft (fun () ->
              warm () ;
              with_conn (on_socket ~clear:true ()) (fun c ->
                  let select () =
                    rows_text
                      (Conn.query c
                         (Raw.sql "SELECT DATABASE() AS d, @v IS NULL AS n") )
                  in
                  exec_ok c "SET @v = 1" ;
                  equal string "ok"
                    (unit_text (Conn.init_db c "information_schema")) ;
                  equal string "d,n | Text \"information_schema\", Int 0"
                    (select ()) ;
                  equal string "ok" (unit_text (Conn.reset c)) ;
                  equal string
                    ("d,n | Text \"" ^ database ^ "\", Int 1")
                    (select ()) ;
                  equal string "Driver 1044 42000"
                    (unit_text (Conn.init_db c "nowhere")) ) ) )
    ; test "literals through the server" (fun () ->
          in_weft (fun () ->
              warm () ;
              let time ?(ps = 0L) date clock =
                Option.get
                  (Ptime.add_span
                     (Option.get (Ptime.of_date_time (date, (clock, 0))))
                     (Option.get (Ptime.Span.of_d_ps (0, ps))) )
              in
              let decimal s =
                Value.Decimal (Option.get (Cistern_core.Decimal.of_string s))
              in
              let rows =
                [ ( Value.Float 1.5
                  , Value.Datetime
                      (time ~ps:123_456_700_000L (2024, 2, 29) (23, 59, 59))
                  , decimal "-123456789012345678901234567890.0123456789"
                  , Value.Int Int64.min_int )
                ; ( Value.Float (-0.0)
                  , Value.Datetime (time (1000, 1, 1) (0, 0, 0))
                  , decimal "0.0000000001"
                  , Value.Int Int64.max_int )
                ; ( Value.Float 5e-324
                  , Value.Datetime
                      (time ~ps:999_999_000_000L (9999, 12, 31) (23, 59, 59))
                  , decimal "1.0000000000"
                  , Value.Int 0L )
                ; ( Value.Float Float.max_float
                  , Value.Null
                  , Value.Null
                  , Value.Null ) ]
              in
              with_conn (on_socket ~clear:true ()) (fun c ->
                  exec_ok c "DROP TABLE IF EXISTS lt" ;
                  exec_ok c
                    "CREATE TABLE lt (i INT PRIMARY KEY, f DOUBLE, t \
                     DATETIME(6), d DECIMAL(40,10), b BIGINT)" ;
                  List.iteri
                    (fun i (f, t, d, b) ->
                      equal string "1 0"
                        (exec_text
                           (Conn.exec c
                              Raw.(
                                sql
                                  (Printf.sprintf "INSERT INTO lt VALUES (%d," i)
                                ++ v f ++ sql "," ++ v t ++ sql "," ++ v d
                                ++ sql "," ++ v b ++ sql ")" ) ) ) )
                    rows ;
                  (* The datetime keeps 6 digits (the literal truncates it), and
                     MariaDB stores -0 as +0. *)
                  let zero =
                    if mysql then "Float -0x0p+0" else "Float 0x0p+0"
                  in
                  equal string
                    ( "f,t,d,b | Float 0x1.8p+0, Datetime \
                       2024-02-29T23:59:59.123456-00:00, Decimal \
                       -123456789012345678901234567890.0123456789, Int \
                       -9223372036854775808 / " ^ zero
                    ^ ", Datetime 1000-01-01T00:00:00.000000-00:00, Decimal \
                       0.0000000001, Int 9223372036854775807 / Float \
                       0x0.0000000000001p-1022, Datetime \
                       9999-12-31T23:59:59.999999-00:00, Decimal 1.0000000000, \
                       Int 0 / Float 0x1.fffffffffffffp+1023, Null, Null, Null"
                    )
                    (rows_text
                       (Conn.query c
                          (Raw.sql "SELECT f, t, d, b FROM lt ORDER BY i") ) ) ;
                  (* A SELECT of the literals: the datetime is a string. *)
                  let f, t, d, b = List.hd rows in
                  equal string
                    "f,t,d,b | Float 0x1.8p+0, Text \"2024-02-29 \
                     23:59:59.123456\", Decimal \
                     -123456789012345678901234567890.0123456789, Int \
                     -9223372036854775808"
                    (rows_text
                       (Conn.query c
                          Raw.(
                            sql "SELECT" ++ v f ++ sql "AS f," ++ v t
                            ++ sql "AS t," ++ v d ++ sql "AS d," ++ v b
                            ++ sql "AS b" ) ) ) ;
                  exec_ok c "DROP TABLE lt" ) ) )
    ; test "the size check at @@max_allowed_packet" (fun () ->
          in_weft (fun () ->
              warm () ;
              (* The default of the server: a run that stops in the middle can
                 leave 1 MiB, thus the test does not restore the value that it
                 reads at the start. *)
              let default = if mysql then 67108864 else 16777216 in
              let set n =
                with_conn (root ()) (fun c ->
                    exec_ok c
                      (Printf.sprintf "SET GLOBAL max_allowed_packet = %d" n) )
              in
              set 1048576 ;
              Fun.protect
                ~finally:(fun () -> set default)
                (fun () ->
                  with_conn (on_socket ~clear:true ()) (fun c ->
                      equal int 1048576 (Conn.info c).max_allowed_packet ;
                      (* [stmt n] is "DO '<a...>'" with a payload of [n] bytes:
                         the command byte and the SQL text. *)
                      let stmt n =
                        Raw.sql ("DO '" ^ String.make (n - 6) 'a' ^ "'")
                      in
                      (* A payload of 1048576 bytes: refused before it goes. *)
                      equal string "Invalid_value"
                        (Result.fold
                           ~ok:(fun _ -> "sent")
                           ~error:(fun e ->
                             Format.asprintf "%a" Cistern_core.Error.pp_summary
                               e )
                           (Conn.exec c (stmt 1048576)) ) ;
                      (* One byte less: the server accepts it. *)
                      equal string "0 0"
                        (exec_text (Conn.exec c (stmt 1048575))) ) ) ) ) ]

let tests =
  group
    ("integration on " ^ server)
    [ test "connect on the socket" (fun () ->
          in_weft (fun () ->
              equal string (expected_session ())
                (session (on_socket ~clear:true ())) ;
              equal string "connected" (connect_ping (on_socket ~clear:true ())) ) )
    ; test "connect on TCP with ssl-mode=DISABLED" (fun () ->
          in_weft (fun () ->
              warm () ;
              equal string (expected_session ()) (session (on_tcp ())) ;
              equal string "connected" (connect_ping (on_tcp ())) ) )
    ; test "connect on TCP with TLS (VERIFY_IDENTITY)" (fun () ->
          in_weft (fun () ->
              equal string (expected_session ()) (session (on_tls ())) ;
              equal string "connected" (connect_ping (on_tls ())) ) )
    ; test "the admin route: root on the socket with no password" (fun () ->
          in_weft (fun () ->
              equal string "connected"
                (connect_ping (on_socket ~user:"root" ~password:"" ())) ) )
    ; test "round trip 3: the identity of the test user, and root" (fun () ->
          (* The identity of each connection is the one that a separate query on
             it reads. Root has SUPER: its pool would turn KILL off. *)
          in_weft (fun () ->
              let read c sql =
                match Conn.Private.query_text c (Raw.sql sql) with
                | Ok [row] ->
                    Array.to_list (Array.map (Option.value ~default:"NULL") row)
                | _ ->
                    failwith sql
              in
              let identity_text c =
                match (Conn.info c).identity with
                | Conn.Checked (Mysql_server {uuid; start}) ->
                    [uuid; Int64.to_string start]
                | Conn.Checked
                    (Mariadb_server
                       {server_uid; hostname; port; server_id; start} ) ->
                    [ server_uid
                    ; hostname
                    ; string_of_int port
                    ; Int64.to_string server_id
                    ; Int64.to_string start ]
                | Conn.Unchecked ->
                    ["unchecked"]
                | Conn.Refused r ->
                    [ Format.asprintf "%a" Cistern_core.Event.pp
                        (Cistern_core.Event.Kill_off {reason= r}) ]
              in
              let expected c =
                if mysql then
                  read c
                    "SELECT @@server_uuid, UNIX_TIMESTAMP() - (SELECT \
                     VARIABLE_VALUE FROM performance_schema.global_status \
                     WHERE VARIABLE_NAME = 'Uptime')"
                else
                  read c
                    "SELECT @@server_uid, @@hostname, @@port, @@server_id, \
                     UNIX_TIMESTAMP() - (SELECT VARIABLE_VALUE FROM \
                     information_schema.GLOBAL_STATUS WHERE VARIABLE_NAME = \
                     'UPTIME')"
              in
              warm () ;
              List.iter
                (fun config ->
                  with_conn config (fun c ->
                      equal (list string) (expected c) (identity_text c) ) )
                [on_socket ~clear:true (); on_tcp (); on_tls ()] ;
              with_conn (on_socket ~user:"root" ~password:"" ()) (fun c ->
                  equal (list string) ["kill off privileged"] (identity_text c) ) ) )
    ; test "a wrong password" (fun () ->
          in_weft (fun () ->
              warm () ;
              if mysql then (
                (* The full path sends the wrong password, and the server
                   refuses it. Over TCP, the fast path fails, and the server
                   asks for the full path: the gate refuses. *)
                equal string "Connect authentication: server error 1045"
                  (connect_ping (on_socket ~password:"wrong" ~clear:true ())) ;
                equal string gate (connect_ping (on_tcp ~password:"wrong" ())) ;
                (* Over TLS with VERIFY_IDENTITY, the full path sends the wrong
                   password, and the server refuses it. *)
                flush_privileges () ;
                equal string "Connect authentication: server error 1045"
                  (connect_ping (on_tls ~password:"wrong" ())) )
              else (
                equal string "Connect authentication: server error 1045"
                  (connect_ping (on_socket ~password:"wrong" ())) ;
                equal string "Connect authentication: server error 1045"
                  (connect_ping (on_tcp ~password:"wrong" ())) ;
                equal string "Connect authentication: server error 1045"
                  (connect_ping (on_tls ~password:"wrong" ())) ) ) )
    ; test "an unknown database" (fun () ->
          in_weft (fun () ->
              warm () ;
              (* The test user has no rights on other databases, thus 1044, not
                 1049. *)
              equal string "Connect authentication: server error 1044"
                (connect_ping (on_socket ~database:"nowhere" ~clear:true ())) ;
              equal string "Connect authentication: server error 1044"
                (connect_ping (on_tls ~database:"nowhere" ())) ) )
    ; test "a socket path that does not exist" (fun () ->
          in_weft (fun () ->
              (* A network error of the connect, not a DNS error. *)
              equal string
                "Connect network error: Weft.Net.Error(No_entry, \
                 \"Net.connect\")"
                (connect_ping
                   (config
                      (C.Socket {path= socket ^ ".none"; allow_cleartext= true})
                      C.No_tls ) ) ) )
    ; test "KILL CONNECTION breaks the connection" (fun () ->
          in_weft (fun () ->
              warm () ;
              List.iter
                (fun config ->
                  match Conn.connect config with
                  | Error e ->
                      failwith (error_text e)
                  | Ok c ->
                      kill (Conn.info c).connection_id ;
                      (* Each ping gives Connection_lost: the first one at the
                         end of the stream, the next ones before any I/O. *)
                      let pings =
                        List.init 3 (fun _ ->
                            Result.fold
                              ~ok:(fun () -> "ok")
                              ~error:error_text (Conn.ping c) )
                      in
                      ignore (Conn.close c) ;
                      equal (list string)
                        ["Connection_lost"; "Connection_lost"; "Connection_lost"]
                        pings )
                [on_socket ~clear:true (); on_tls ()] ) ) ]
  ::
  ( if mysql then
      [ group "caching_sha2_password on mysql84"
          [ test "the full path, the gate and the fast path" (fun () ->
                in_weft (fun () ->
                    flush_privileges () ;
                    equal string gate (connect_ping (on_tcp ())) ;
                    equal string gate (connect_ping (on_socket ())) ;
                    equal string "connected"
                      (connect_ping (on_socket ~clear:true ())) ;
                    equal string "connected" (connect_ping (on_tcp ())) ) ) ] ]
    else [] )

(* The group of tests: CISTERN_GROUP is deterministic (the default), timing or
   shutdown (DESIGN.md, "CI jobs"). TLS takes its randomness from the default
   generator (Db.with_db). *)
let () =
  Mirage_crypto_rng_unix.use_default () ;
  let group =
    match Sys.getenv_opt "CISTERN_GROUP" with
    | None | Some "" | Some "deterministic" ->
        (* The Db tests first: they count the sessions of the test user. *)
        Db_tests.deterministic :: Tx_tests.deterministic
        :: Schema_tests.deterministic :: Typed_tests.deterministic
        :: Relation_tests.deterministic :: tests
        @ [ query_tests
          ; Tls_tests.deterministic
          ; Kill_tests.deterministic
          ; Example_tests.deterministic
          ; Claims.deterministic ]
    | Some "timing" ->
        [Db_tests.timing; Tx_tests.timing; Tls_tests.timing; Claims.timing]
    | Some "shutdown" ->
        [Claims.shutdown]
    | Some g ->
        prerr_endline ("integration: unknown CISTERN_GROUP " ^ g) ;
        exit 2
  in
  exit (Windtrap.run "integration" group)
