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
   column names. Cistern's client has no query before phase 1.1d. *)
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

let () = exit (Windtrap.run "integration" tests)
