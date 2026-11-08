(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* The integration tests of KILL (the plan of phase 1.5, 4.6; PR 1.5d), with the
   cases of the probe of the review of PR 1.5c1a. Root reads
   information_schema.PROCESSLIST with the client of the server (Support.admin):
   the tests do not depend on the client that they test. *)

open Windtrap
open Support
module Db = Cistern.Db
module Tx = Cistern.Tx
module Raw = Cistern.Raw
module Ev = Cistern_core.Event
module Span = Weft.Span
module Time = Weft.Time
module Fiber = Weft.Fiber

let q s = Raw.query (Raw.sql s)

let x s = Raw.exec (Raw.sql s)

let mono = Weft.Clock.Mono

let seconds since =
  Int64.to_float (Span.to_ns (Time.diff mono (Time.now mono) since)) /. 1e9

(* The events of KILL of a pool, as text with no duration, in a list that a
   mutex protects (the fibers run on more than one domain). *)
let recorder () =
  let m = Mutex.create () and events = ref [] in
  let on_query e =
    let text =
      match e with
      | Ev.Kill {outcome= `Killed; _} ->
          Some "kill killed"
      | Ev.Kill {outcome= `Gone; _} ->
          Some "kill gone"
      | Ev.Kill_dropped _ | Ev.Kill_off _ ->
          Some (Format.asprintf "%a" Ev.pp e)
      | Ev.Query _ | Ev.Connect _ ->
          None
    in
    Option.iter
      (fun t -> Mutex.protect m (fun () -> events := t :: !events))
      text
  in
  let read () = Mutex.protect m (fun () -> List.rev !events) in
  (on_query, read)

(* Wait until [n] events of KILL came, at most 3 s. *)
let wait_events read n =
  let t0 = Time.now mono in
  while List.length (read ()) < n && seconds t0 < 3. do
    Time.sleep mono (Span.ms 10)
  done

(* The sessions that run the statement [info], as root sees them. *)
let running info =
  admin
    (Printf.sprintf
       "SELECT COUNT(*) FROM information_schema.PROCESSLIST WHERE INFO = '%s'"
       info )

(* Wait until no session runs [info], at most [limit] seconds, else fail. It
   gives the time of the wait. The tests of a KILL use a limit of 1 s and a
   statement of 5 s, so that a KILL that does not end the session fails (review
   A of 1.5d, L1). *)
let wait_gone ?(limit = 5.) info =
  let t0 = Time.now mono in
  while running info <> "0" && seconds t0 < limit do
    Time.sleep mono (Span.ms 10)
  done ;
  if running info <> "0" then failwith ("integration: still running: " ^ info) ;
  seconds t0

let text = function Ok _ -> "ok" | Error e -> error_text e

(* Root ends the statement [info] with KILL CONNECTION, so that a test does not
   wait for its end (review A of 1.5d, L2). *)
let end_by_root info =
  let id =
    admin
      (Printf.sprintf
         "SELECT ID FROM information_schema.PROCESSLIST WHERE INFO = '%s'" info )
  in
  ignore (admin ("KILL CONNECTION " ^ id)) ;
  ignore (wait_gone ~limit:1. info)

(* A configuration on the socket with [kill]. *)
let on_socket_kill ~kill ?(user = user) ?(password = password) () =
  match
    C.make ~kill
      ~address:(C.Socket {path= socket; allow_cleartext= true})
      ~user ~password ~database ()
  with
  | Ok c ->
      c
  | Error m ->
      failwith m

(* A statement of [s] seconds that a read deadline of 300 ms cuts. *)
let cut db s =
  text
    (Db.exec ~timeout:(Span.ms 300) db
       (q (Printf.sprintf "SELECT SLEEP(%d)" s)) )

(* The test users of the privilege check (the plan of 1.5, D6 and 3.4), made and
   dropped by root. *)
let admin_privilege = if mysql then "CONNECTION_ADMIN" else "CONNECTION ADMIN"

let with_users f =
  let drop () =
    ignore (admin "DROP USER IF EXISTS 'ck_priv'@'%', 'ck_role'@'%', 'c@k'@'%'") ;
    ignore (admin "DROP ROLE IF EXISTS ck_r")
  in
  drop () ;
  Fun.protect ~finally:drop (fun () ->
      List.iter
        (fun u ->
          ignore
            (admin (Printf.sprintf "CREATE USER %s IDENTIFIED BY 'ck-pw'" u)) ;
          ignore (admin (Printf.sprintf "GRANT ALL ON %s.* TO %s" database u)) )
        ["'ck_priv'@'%'"; "'ck_role'@'%'"; "'c@k'@'%'"] ;
      ignore
        (admin
           (Printf.sprintf "GRANT %s ON *.* TO 'ck_priv'@'%%'" admin_privilege) ) ;
      ignore (admin "CREATE ROLE ck_r") ;
      ignore (admin (Printf.sprintf "GRANT %s ON *.* TO ck_r" admin_privilege)) ;
      ignore (admin "GRANT ck_r TO 'ck_role'@'%'") ;
      ignore
        (admin
           ( if mysql then "SET DEFAULT ROLE ck_r TO 'ck_role'@'%'"
             else "SET DEFAULT ROLE ck_r FOR 'ck_role'@'%'" ) ) ;
      f () )

let deterministic =
  group ("KILL on " ^ server)
    [ test "a cut statement is killed, on the socket and over TLS" (fun () ->
          in_weft (fun () ->
              warm () ;
              List.iter
                (fun (name, config) ->
                  let on_query, read = recorder () in
                  Db.with_db ~pool_size:2 ~on_query config (fun db ->
                      equal ~msg:name string "Outcome_unknown statement"
                        (cut db 5) ;
                      (* The session goes at once (the probe: 1 to 12 ms). *)
                      ignore (wait_gone ~limit:1. "SELECT SLEEP(5)") ;
                      wait_events read 1 ;
                      equal ~msg:name (list string) ["kill killed"] (read ()) ;
                      (* On MySQL, the control connection, the one session of
                         the test user left, runs TLS with a configuration of
                         TLS. *)
                      if mysql && name = "TLS" then
                        equal ~msg:"the control connection on TLS" string "1"
                          (admin
                             (Printf.sprintf
                                "SELECT COUNT(*) FROM \
                                 performance_schema.status_by_thread s JOIN \
                                 performance_schema.threads t USING \
                                 (THREAD_ID) WHERE t.PROCESSLIST_USER = '%s' \
                                 AND s.VARIABLE_NAME = 'Ssl_cipher' AND \
                                 s.VARIABLE_VALUE <> ''"
                                user ) ) ) )
                [("socket", on_socket ~clear:true ()); ("TLS", on_tls ())] ) )
    ; test "no KILL before the flush, for a server error, and at the margin"
        (fun () ->
          in_weft (fun () ->
              warm () ;
              let on_query, read = recorder () in
              Db.with_db ~pool_size:1 ~on_query (on_socket ~clear:true ())
                (fun db ->
                  Fiber.scope (fun s ->
                      let holder =
                        Fiber.fork_promise s (fun () ->
                            text (Db.exec db (q "SELECT SLEEP(1)")) )
                      in
                      Time.sleep mono (Span.ms 100) ;
                      (* The only connection is busy: the wait passes its limit,
                         and a cancel stops another wait. *)
                      equal string "Timeout"
                        (text
                           (Db.exec ~timeout:(Span.ms 200) db (q "SELECT 2")) ) ;
                      equal string "cancelled"
                        ( match
                            Fiber.with_timeout mono (Span.ms 100) (fun () ->
                                Db.exec db (q "SELECT 3") )
                          with
                        | _ ->
                            "returned"
                        | exception Fiber.Timeout ->
                            "cancelled" ) ;
                      equal string "ok" (Weft.Promise.await holder) ) ;
                  equal string "Timeout"
                    (text
                       (Db.exec ~timeout:(Span.ms 10) db (q "SELECT SLEEP(5)")) ) ;
                  is_true
                    (String.starts_with ~prefix:"Driver 1064"
                       (text (Db.exec db (x "SELEC 1"))) ) ;
                  Time.sleep mono (Span.ms 300) ) ;
              equal (list string) [] (read ()) ) )
    ; test "root: Kill_off privileged, and the statement runs to its end"
        (fun () ->
          in_weft (fun () ->
              let on_query, read = recorder () in
              Db.with_db ~pool_size:2 ~on_query
                (on_socket_kill ~kill:true ~user:"root" ~password:"" ())
                (fun db ->
                  equal string "Outcome_unknown statement" (cut db 2) ;
                  Time.sleep mono (Span.ms 500) ;
                  equal string "1" (running "SELECT SLEEP(2)") ) ;
              equal (list string) ["kill off privileged"] (read ()) ;
              end_by_root "SELECT SLEEP(2)" ) )
    ; test "kill=false: no KILL, and the statement runs to its end" (fun () ->
          in_weft (fun () ->
              warm () ;
              let on_query, read = recorder () in
              Db.with_db ~pool_size:2 ~on_query (on_socket_kill ~kill:false ())
                (fun db ->
                  equal string "Outcome_unknown statement" (cut db 2) ;
                  Time.sleep mono (Span.ms 500) ;
                  equal string "1" (running "SELECT SLEEP(2)") ) ;
              equal (list string) [] (read ()) ;
              end_by_root "SELECT SLEEP(2)" ) )
    ; test "a Tx gets its KILL at its end" (fun () ->
          in_weft (fun () ->
              warm () ;
              let on_query, read = recorder () in
              Db.with_db ~pool_size:2 ~on_query (on_socket ~clear:true ())
                (fun db ->
                  let inner = ref "" and during = ref "" in
                  let r =
                    Db.transaction ~timeout:(Span.s 3) db (fun tx ->
                        inner :=
                          text
                            (Tx.exec ~timeout:(Span.ms 200) tx
                               (q "SELECT SLEEP(4)") ) ;
                        (* The callback goes on: no KILL yet (D13). *)
                        Time.sleep mono (Span.ms 300) ;
                        during := running "SELECT SLEEP(4)" )
                  in
                  equal string "Outcome_unknown statement" !inner ;
                  equal string "1" !during ;
                  equal string "Timeout" (text r) ;
                  ignore (wait_gone ~limit:1. "SELECT SLEEP(4)") ;
                  wait_events read 1 ) ;
              equal (list string) ["kill killed"] (read ()) ) )
    ; test "a cut write of a Tx that waits for a lock: no row, the lock is free"
        (fun () ->
          in_weft (fun () ->
              warm () ;
              with_conn (on_socket ~clear:true ()) (fun c ->
                  exec_ok c "DROP TABLE IF EXISTS ck6" ;
                  exec_ok c
                    "CREATE TABLE ck6 (id INT PRIMARY KEY, v INT) ENGINE=InnoDB" ;
                  exec_ok c "INSERT INTO ck6 VALUES (1, 0)" ) ;
              let on_query, read = recorder () in
              with_conn (on_socket ~clear:true ()) (fun holder ->
                  exec_ok holder "START TRANSACTION" ;
                  exec_ok holder "UPDATE ck6 SET v = 1 WHERE id = 1" ;
                  Db.with_db ~pool_size:2 ~on_query (on_socket ~clear:true ())
                    (fun db ->
                      let r =
                        match
                          Fiber.with_timeout mono (Span.ms 300) (fun () ->
                              Db.transaction db (fun tx ->
                                  ignore
                                    (Tx.exec tx
                                       (x "UPDATE ck6 SET v = 2 WHERE id = 1") ) ) )
                        with
                        | _ ->
                            "returned"
                        | exception Fiber.Timeout ->
                            "cancelled"
                      in
                      equal string "cancelled" r ;
                      ignore (wait_gone "UPDATE ck6 SET v = 2 WHERE id = 1") ;
                      wait_events read 1 ) ;
                  exec_ok holder "ROLLBACK" ) ;
              equal (list string) ["kill killed"] (read ()) ;
              with_conn (on_socket ~clear:true ()) (fun c ->
                  (* No row of the cut write, and the lock is free. *)
                  equal string "Int 0"
                    ( rows_text (Conn.query c (Raw.sql "SELECT v FROM ck6"))
                    |> String.split_on_char '|' |> List.rev |> List.hd
                    |> String.trim ) ;
                  exec_ok c "UPDATE ck6 SET v = 3 WHERE id = 1" ;
                  exec_ok c "DROP TABLE ck6" ) ) )
    ; test "the end of with_db during a KILL: the session goes, one event"
        (fun () ->
          in_weft (fun () ->
              warm () ;
              let on_query, read = recorder () in
              Db.with_db ~pool_size:2 ~on_query (on_socket ~clear:true ())
                (fun db ->
                  (* The body ends at once after the cut: the KILL comes in the
                     join of the killer, or in the drain. *)
                  equal string "Outcome_unknown statement" (cut db 5) ) ;
              equal (list string) ["kill killed"] (read ()) ;
              ignore (wait_gone ~limit:1. "SELECT SLEEP(5)") ) )
    ; test "the privilege check: a direct grant, a role, a user name with @"
        (fun () ->
          in_weft (fun () ->
              with_users (fun () ->
                  List.iter
                    (fun (u, expected) ->
                      let on_query, read = recorder () in
                      Db.with_db ~pool_size:2 ~on_query
                        (on_socket_kill ~kill:true ~user:u ~password:"ck-pw" ())
                        (fun db ->
                          equal ~msg:u string "Outcome_unknown statement"
                            (cut db 5) ;
                          wait_events read 1 ) ;
                      equal ~msg:u (list string) expected (read ()) ;
                      (* With KILL off the statement runs: root ends it. With
                         KILL on it is gone within 1 s. *)
                      if expected = ["kill killed"] then
                        ignore (wait_gone ~limit:1. "SELECT SLEEP(5)")
                      else (
                        equal ~msg:u string "1" (running "SELECT SLEEP(5)") ;
                        end_by_root "SELECT SLEEP(5)" ) )
                    [ ("ck_priv", ["kill off privileged"])
                    ; ("ck_role", ["kill off role"])
                      (* The grantee from the last @ (review A2 of the plan,
                         LN1): KILL stays on. *)
                    ; ("c@k", ["kill killed"]) ] ) ) ) ]
