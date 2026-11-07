(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* The integration suite: Cistern's own client against the server of make db-up
   whose env file CISTERN_DB_ENV names (an absolute path). The plan of phase
   1.1, section 4.4, and the decisions B7 and B9.

   On MySQL, a TCP connection with ssl-mode=DISABLED can use only the fast path
   of caching_sha2_password, thus each TCP test first warms the cache with a
   connect on the socket with allow-cleartext=socket. Then the tests do not
   depend on their order. *)

open Windtrap
module C = Cistern_mysql.Config
module Conn = Cistern_mysql.Connection
module E = Cistern_core.Error

(* The values of the env file: lines KEY='value', and comments. *)
let env =
  let path =
    match Sys.getenv_opt "CISTERN_DB_ENV" with
    | Some p when not (Filename.is_relative p) ->
        p
    | _ ->
        prerr_endline
          "integration: set CISTERN_DB_ENV to the absolute path of \
           .db/<server>/env (make integration SERVER=<server>)" ;
        exit 2
  in
  In_channel.with_open_text path In_channel.input_lines
  |> List.filter_map (fun line ->
      match String.index_opt line '=' with
      | Some i when line <> "" && line.[0] <> '#' ->
          let key = String.sub line 0 i
          and value = String.sub line (i + 1) (String.length line - i - 1) in
          (* The value is between single quotes. *)
          Some (key, String.sub value 1 (String.length value - 2))
      | _ ->
          None )

let get key =
  match List.assoc_opt key env with
  | Some v ->
      v
  | None ->
      failwith ("integration: no " ^ key ^ " in the env file")

let server = get "DB_SERVER"

let mysql = server = "mysql84"

let socket = get "DB_SOCKET"

let port = int_of_string (get "DB_PORT")

let user = get "DB_USER"

let password = get "DB_PASSWORD"

let database = get "DB_DATABASE"

let config ?(user = user) ?(password = password) ?(database = database) address
    tls =
  match C.make ~tls ~address ~user ~password ~database () with
  | Ok c ->
      c
  | Error m ->
      failwith m

let on_socket ?user ?password ?database ?(clear = false) () =
  config ?user ?password ?database
    (C.Socket {path= socket; allow_cleartext= clear})
    C.No_tls

let on_tcp ?user ?password ?database () =
  config ?user ?password ?database (C.Tcp {host= "127.0.0.1"; port}) C.No_tls

(* Each test runs in its own weft world. *)
let in_weft f = Weft.run (module Weft_os) f

let error_text = function
  | E.Connect m ->
      "Connect " ^ m
  | E.Connection_lost ->
      "Connection_lost"
  | e ->
      Format.asprintf "%a" E.pp_summary e

(* Connect, ping, close: the outcome as text. *)
let connect_ping config =
  match Conn.connect config with
  | Error e ->
      error_text e
  | Ok c ->
      let p = Conn.ping c in
      Conn.close c ;
      Result.fold ~ok:(fun () -> "connected") ~error:error_text p

(* The connect of the cache warm-up of MySQL (and a plain connect on
   MariaDB). *)
let warm () = ignore (connect_ping (on_socket ~clear:true ()))

(* The environment of the client of the server, as in test/db/db.sh: no login
   file, and no variable that changes the password, the host, the port or the
   socket. *)
let client_env () =
  let removed =
    [ "MYSQL_PWD"
    ; "MYSQL_HOST"
    ; "MYSQL_TCP_PORT"
    ; "MYSQL_UNIX_PORT"
    ; "MYSQL_HOME"
    ; "MYSQL_TEST_LOGIN_FILE" ]
  in
  let kept =
    List.filter
      (fun kv ->
        match String.index_opt kv '=' with
        | Some i ->
            not (List.mem (String.sub kv 0 i) removed)
        | None ->
            true )
      (Array.to_list (Unix.environment ()))
  in
  Array.of_list ("MYSQL_TEST_LOGIN_FILE=/dev/null" :: kept)

(* A statement as root, with the client of the server, and its output with no
   column names. FLUSH PRIVILEGES and KILL CONNECTION use the client of the
   server, not Cistern's: the tests of the authentication and of KILL do not
   then depend on the client that they test. *)
let admin sql =
  let client = Filename.concat (get "DB_STORE") "bin/mysql" in
  (* The MariaDB client asks for TLS by default, also on the socket. *)
  let tls = if mysql then [] else ["--skip-ssl"] in
  let ((out_channel, _, _) as ic) =
    Unix.open_process_args_full client
      (Array.of_list
         ( [client; "--no-defaults"; "--protocol=SOCKET"]
         @ tls
         @ ["-uroot"; "-S"; socket; "-N"; "-s"; "-e"; sql] ) )
      (client_env ())
  in
  let out = In_channel.input_all out_channel in
  match Unix.close_process_full ic with
  | Unix.WEXITED 0 ->
      String.trim out
  | _ ->
      failwith ("integration: the statement failed: " ^ sql)

(* It empties the authentication cache of MySQL. *)
let flush_privileges () = ignore (admin "FLUSH PRIVILEGES")

(* KILL CONNECTION as root, then wait until the server has no thread with this
   id (at most 5 s). *)
let kill id =
  ignore (admin (Printf.sprintf "KILL CONNECTION %Ld" id)) ;
  let count () =
    admin
      (Printf.sprintf
         "SELECT COUNT(*) FROM information_schema.PROCESSLIST WHERE ID = %Ld" id )
  in
  let rec wait n =
    if count () <> "0" then
      if n = 0 then failwith "integration: the killed thread stays"
      else (
        Weft.Time.sleep Weft.Clock.Mono (Weft.Span.ms 50) ;
        wait (n - 1) )
  in
  wait 100

let gate =
  "Connect the server asks for full authentication; use VERIFY_IDENTITY, \
   ssl-fingerprint, or allow-cleartext=socket"

let dialect_text (i : Conn.info) =
  match i.dialect with Mysql -> "mysql" | Mariadb -> "mariadb"

(* The session values of a connect. *)
let session config =
  match Conn.connect config with
  | Error e ->
      error_text e
  | Ok c ->
      let i = Conn.info c in
      Conn.close c ;
      Printf.sprintf "%s %s %b" (dialect_text i) i.version
        (Int64.logand i.connection_id 0xFFFF_FFFFL = Int64.of_int i.thread_id)

let expected_session () =
  Printf.sprintf "%s %s true"
    (if mysql then "mysql" else "mariadb")
    (get "DB_VERSION")

(* The queries (phase 1.1d). *)

module Raw = Cistern_core.Raw
module Value = Cistern_core.Value

let value_text (v : Value.t) =
  match v with
  | Null ->
      "Null"
  | Int i ->
      Printf.sprintf "Int %Ld" i
  | Float f ->
      Printf.sprintf "Float %h" f
  | Text s ->
      Printf.sprintf "Text %S" s
  | Bytes s ->
      Printf.sprintf "Bytes %S" s
  | Datetime t ->
      "Datetime " ^ Ptime.to_rfc3339 ~frac_s:6 t
  | Decimal d ->
      "Decimal " ^ Cistern_core.Decimal.to_string d

let rows_text = function
  | Ok (r : Raw.result) ->
      String.concat "," (Array.to_list r.columns)
      ^ " | "
      ^ String.concat " / "
          (List.map
             (fun row ->
               String.concat ", " (Array.to_list (Array.map value_text row)) )
             r.rows )
  | Error e ->
      error_text e

let exec_text = function
  | Ok (r : Cistern_core.Exec_result.t) ->
      Printf.sprintf "%Ld %Ld" r.affected_rows r.last_insert_id
  | Error e ->
      error_text e

let unit_text = function Ok () -> "ok" | Error e -> error_text e

(* [with_conn config f]: [f] on a new connection, then close. *)
let with_conn config f =
  match Conn.connect config with
  | Error e ->
      failwith (error_text e)
  | Ok c ->
      Fun.protect ~finally:(fun () -> Conn.close c) (fun () -> f c)

let exec_ok c sql =
  match Conn.exec c (Raw.sql sql) with
  | Ok _ ->
      ()
  | Error e ->
      failwith (sql ^ ": " ^ error_text e)

(* The table of each column type of the sim fixture
   (test/sim/types_fixture.ml). *)
let create_types =
  "CREATE TABLE tt (id BIGINT UNSIGNED AUTO_INCREMENT PRIMARY KEY, ti \
   TINYINT(1), u BIGINT UNSIGNED, d DECIMAL(65,30), b BIT(10), y YEAR, dt \
   DATE, tm TIME(6), dtt DATETIME(6), ts TIMESTAMP NULL, j JSON, g GEOMETRY, \
   en ENUM('a','b'), st SET('x','y'), f FLOAT, vb VARBINARY(10), bl BLOB, tx \
   TEXT, ch CHAR(3), sm SMALLINT, mi MEDIUMINT, it INT, bi BIGINT, vc \
   VARCHAR(10), db DOUBLE)"

let insert_types =
  Raw.(
    sql
      "INSERT INTO tt (ti, u, d, b, y, dt, tm, dtt, ts, j, g, en, st, f, vb, \
       bl, tx, ch, sm, mi, it, bi, vc, db) VALUES (1, 18446744073709551615, \
       -0.5, b'1000000101', 2024, '2024-02-29', '-838:59:59', '2024-01-02 \
       03:04:05.5', '2024-01-02 03:04:05', '{\"a\": [1, 2]}', \
       ST_GeomFromText('POINT(1 2)'), 'b', 'x,y', 3.5,"
    ++ v (Bytes "\x00\xff") ++ sql "," ++ v (Bytes "\x01") ++ sql ","
    ++ v (Text "h\xc3\xa9") ++ sql ", 'ab'," ++ v (Int (-32768L)) ++ sql ","
    ++ v (Int 8388607L) ++ sql "," ++ v (Int (-2147483648L)) ++ sql ","
    ++ v (Int Int64.min_int) ++ sql "," ++ v (Text "v\xc3\xa9") ++ sql ","
    ++ v (Float (-2.5e-6)) ++ sql ")" )

let select_types =
  "SELECT id, ti, u, d, b, y, dt, tm, dtt, ts, j, g, en, st, f, vb, bl, tx, \
   ch, sm, mi, it, bi, vc, db, NULL AS n, 1/3 AS q, -0e0 AS z, \
   JSON_OBJECT('k', 1) AS jo, CAST(-1 AS SIGNED) AS s FROM tt"

let types_expected () =
  "id,ti,u,d,b,y,dt,tm,dtt,ts,j,g,en,st,f,vb,bl,tx,ch,sm,mi,it,bi,vc,db,n,q,z,jo,s \
   | Int 1, Int 1, Decimal 18446744073709551615, Decimal \
   -0.500000000000000000000000000000, Bytes \"\\002\\005\", Int 2024, Text \
   \"2024-02-29\", Text \"-838:59:59.000000\", Datetime \
   2024-01-02T03:04:05.500000-00:00, Datetime \
   2024-01-02T03:04:05.000000-00:00, Text \"{\\\"a\\\": [1, 2]}\", Bytes \
   \"\\000\\000\\000\\000\\001\\001\\000\\000\\000\\000\\000\\000\\000\\000\\000\\240?\\000\\000\\000\\000\\000\\000\\000@\", \
   Text \"b\", Text \"x,y\", Float 0x1.cp+1, Bytes \"\\000\\255\", Bytes \
   \"\\001\", Text \"h\\195\\169\", Text \"ab\", Int -32768, Int 8388607, Int \
   -2147483648, Int -9223372036854775808, Text \"v\\195\\169\", Float \
   -0x1.4f8b588e368f1p-19, Null, Decimal 0.3333, "
  ^ (if mysql then "Float -0x0p+0" else "Float 0x0p+0")
  ^ ", Text \"{\\\"k\\\": 1}\", Int -1"

(* The connection of the admin: root on the socket, with Cistern's client. *)
let root () = on_socket ~user:"root" ~password:"" ()

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
                [on_socket ~clear:true (); on_tcp ()] ) )
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
    ; test "the admin route: root on the socket with no password" (fun () ->
          in_weft (fun () ->
              equal string "connected"
                (connect_ping (on_socket ~user:"root" ~password:"" ())) ) )
    ; test "a wrong password" (fun () ->
          in_weft (fun () ->
              warm () ;
              if mysql then (
                (* The full path sends the wrong password, and the server
                   refuses it. Over TCP, the fast path fails, and the server
                   asks for the full path: the gate refuses. *)
                equal string "Connect authentication: server error 1045"
                  (connect_ping (on_socket ~password:"wrong" ~clear:true ())) ;
                equal string gate (connect_ping (on_tcp ~password:"wrong" ())) )
              else (
                equal string "Connect authentication: server error 1045"
                  (connect_ping (on_socket ~password:"wrong" ())) ;
                equal string "Connect authentication: server error 1045"
                  (connect_ping (on_tcp ~password:"wrong" ())) ) ) )
    ; test "an unknown database" (fun () ->
          in_weft (fun () ->
              warm () ;
              (* The test user has no rights on other databases, thus 1044, not
                 1049. *)
              equal string "Connect authentication: server error 1044"
                (connect_ping (on_socket ~database:"nowhere" ~clear:true ())) ;
              equal string "Connect authentication: server error 1044"
                (connect_ping (on_tcp ~database:"nowhere" ())) ) )
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
                      Conn.close c ;
                      equal (list string)
                        ["Connection_lost"; "Connection_lost"; "Connection_lost"]
                        pings )
                [on_socket ~clear:true (); on_tcp ()] ) ) ]
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

let () = exit (Windtrap.run "integration" (tests @ [query_tests]))
