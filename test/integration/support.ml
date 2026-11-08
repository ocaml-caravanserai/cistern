(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* The helpers of the integration suite and of the claims: the env file of the
   server, the configurations, the admin route, and the texts of the results. *)

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

let ca = get "DB_CA"

let other_ca = get "DB_OTHER_CA"

(* The 32 bytes of the SHA-256 of the server certificate (DB_FINGERPRINT has
   them in hex). *)
let fingerprint =
  let h = get "DB_FINGERPRINT" in
  String.init 32 (fun i ->
      Char.chr (int_of_string ("0x" ^ String.sub h (2 * i) 2)) )

let config ?(user = user) ?(password = password) ?(database = database)
    ?authenticator address tls =
  match C.make ~tls ?authenticator ~address ~user ~password ~database () with
  | Ok c ->
      c
  | Error m ->
      failwith m

let on_socket ?user ?password ?database ?(clear = false) () =
  config ?user ?password ?database
    (C.Socket {path= socket; allow_cleartext= clear})
    C.No_tls

(* TCP with ssl-mode=DISABLED: the claims of phase 1.1, and the tests of the
   clear-text rule. *)
let on_tcp ?user ?password ?database () =
  config ?user ?password ?database (C.Tcp {host= "127.0.0.1"; port}) C.No_tls

(* TCP with TLS (phase 1.4). The default is VERIFY_IDENTITY with the test CA on
   127.0.0.1. *)
let on_tls ?user ?password ?database ?(host = "127.0.0.1")
    ?(mode = C.Verify_identity) ?(ca = Some ca) ?fingerprint ?authenticator () =
  config ?user ?password ?database ?authenticator
    (C.Tcp {host; port})
    (C.Tls {mode; ca; fingerprint})

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
      ignore (Conn.close c) ;
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
  "Connect the server asks for full authentication; use VERIFY_IDENTITY with \
   ssl-ca, a pinned certificate (ssl-fingerprint), or allow-cleartext=socket"

let dialect_text (i : Conn.info) =
  match i.dialect with Mysql -> "mysql" | Mariadb -> "mariadb"

(* The session values of a connect. *)
let session config =
  match Conn.connect config with
  | Error e ->
      error_text e
  | Ok c ->
      let i = Conn.info c in
      ignore (Conn.close c) ;
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
      Fun.protect ~finally:(fun () -> ignore (Conn.close c)) (fun () -> f c)

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
