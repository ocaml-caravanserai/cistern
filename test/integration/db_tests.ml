(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* The integration tests of Db on the real servers (the plan of phase 1.2,
   section 4.4, and the facts V1 to V5 of section 7). The group [deterministic]
   runs in each integration job, before the other groups: its first test counts
   the sessions of the test user, and a later test (a statement that a deadline
   cuts) leaves a session that runs on the server until its end. The group
   [timing] runs only in the nightly db.yml. *)

open Windtrap
open Support
module Db = Cistern.Db
module Raw = Cistern.Raw
module Ev = Cistern_core.Event
module Span = Weft.Span
module Time = Weft.Time
module Fiber = Weft.Fiber

let q s = Raw.query (Raw.sql s)

let config () = on_socket ~clear:true ()

(* The events of a pool, in a list that a mutex protects (the fibers run on more
   than one domain). *)
let recorder () =
  let m = Mutex.create () and events = ref [] in
  let on_query e = Mutex.protect m (fun () -> events := e :: !events) in
  let read () = Mutex.protect m (fun () -> List.rev !events) in
  (on_query, read)

let count_queries events =
  List.length (List.filter (function Ev.Query _ -> true | _ -> false) events)

let count_connects events =
  List.length
    (List.filter (function Ev.Connect _ -> true | _ -> false) events)

(* The first value of the first row, as text. *)
let first = function
  | Ok ({rows= row :: _; _} : Raw.result) ->
      value_text row.(0)
  | Ok _ ->
      "no row"
  | Error e ->
      error_text e

let connection_id db = first (Db.exec db (q "SELECT CONNECTION_ID()"))

let seconds since =
  Int64.to_float
    (Span.to_ns (Time.diff Weft.Clock.Mono (Time.now Weft.Clock.Mono) since))
  /. 1e9

(* The sessions of the test user, as root sees them. [after id]: only those with
   an id larger than [id]. *)
let user_sessions ?(after = "0") () =
  int_of_string
    (admin
       (Printf.sprintf
          "SELECT COUNT(*) FROM information_schema.PROCESSLIST WHERE USER = \
           '%s' AND ID > %s"
          user after ) )

(* The largest session id of the server now. A session that starts later has a
   larger id. *)
let last_session_id () =
  admin "SELECT COALESCE(MAX(ID), 0) FROM information_schema.PROCESSLIST"

(* Wait until [cond ()] holds, at most 5 s. *)
let wait_until what cond =
  let rec go n =
    if not (cond ()) then
      if n = 0 then failwith ("integration: no " ^ what ^ " after 5 s")
      else (
        Time.sleep Weft.Clock.Mono (Span.ms 50) ;
        go (n - 1) )
  in
  go 100

(* The id of a session that is gone from the server. *)
let gone id =
  admin
    (Printf.sprintf
       "SELECT COUNT(*) FROM information_schema.PROCESSLIST WHERE ID = %s" id )
  = "0"

(* [contains s sub]: [sub] is a part of [s]. *)
let contains s sub =
  let n = String.length s and k = String.length sub in
  let rec at i = i + k <= n && (String.sub s i k = sub || at (i + 1)) in
  at 0

let int_text id =
  (* "Int 42" gives "42". *)
  match String.split_on_char ' ' id with
  | [_; n] ->
      n
  | _ ->
      id

let deterministic =
  group ("Db on " ^ server)
    [ test "a pool of 4, 64 fibers on 4 domains, at most 4 sessions (V3)"
        (fun () ->
          Weft.run ~domains:4
            (module Weft_os)
            (fun () ->
              warm () ;
              let on_query, events = recorder () in
              (* Only the sessions that start after this point count: a session
                 that another run or another client left stays out. *)
              let after = last_session_id () in
              let results, sessions =
                Db.with_db ~pool_size:4 ~on_query (config ()) (fun db ->
                    let results =
                      Fiber.scope (fun s ->
                          let ps =
                            List.init 64 (fun i ->
                                Fiber.fork_promise s (fun () ->
                                    first
                                      (Db.exec db
                                         (q (Printf.sprintf "SELECT %d" i)) ) ) )
                          in
                          List.map Weft.Promise.await ps )
                    in
                    (results, user_sessions ~after ()) )
              in
              equal (list string)
                (List.init 64 (fun i -> Printf.sprintf "Int %d" i))
                results ;
              is_true ~msg:"at most 4 sessions of the test user"
                (sessions >= 1 && sessions <= 4) ;
              equal ~msg:"one Query event for each exec" int 64
                (count_queries (events ())) ;
              is_true ~msg:"at most 4 connects" (count_connects (events ()) <= 4) ) )
    ; test "the session reset after a raw SET and USE (V5)" (fun () ->
          in_weft (fun () ->
              warm () ;
              Db.with_db ~pool_size:1 (config ()) (fun db ->
                  let session () =
                    first
                      (Db.exec db
                         (q
                            "SELECT CONCAT_WS(' ', CONNECTION_ID(), \
                             @@sql_mode, @@collation_connection, @@time_zone, \
                             DATABASE(), @v IS NULL)" ) )
                  in
                  let before = session () in
                  List.iter
                    (fun s -> ignore (Db.exec db (q s)))
                    [ "SET sql_mode = ''"
                    ; "SET time_zone = '+05:00'"
                    ; "SET NAMES latin1"
                    ; "SET @v = 7"
                    ; "USE information_schema" ] ;
                  (* The same connection, with the session of Cistern again. *)
                  equal string before (session ()) ) ) )
    ; test "a raw BEGIN: an abort at checkin, and no effect on the next exec"
        (fun () ->
          in_weft (fun () ->
              warm () ;
              Db.with_db ~pool_size:1 (config ()) (fun db ->
                  ignore (Db.exec db (q "DROP TABLE IF EXISTS pb")) ;
                  ignore
                    (Db.exec db
                       (q "CREATE TABLE pb (id INT PRIMARY KEY) ENGINE=InnoDB") ) ;
                  let id1 = int_text (connection_id db) in
                  (* A snapshot starts the InnoDB transaction at once, thus root
                     sees it. *)
                  ignore
                    (Db.exec db
                       (q "START TRANSACTION WITH CONSISTENT SNAPSHOT") ) ;
                  wait_until "end of the old session" (fun () -> gone id1) ;
                  equal ~msg:"no transaction of the old session" string "0"
                    (admin
                       (Printf.sprintf
                          "SELECT COUNT(*) FROM information_schema.INNODB_TRX \
                           WHERE trx_mysql_thread_id = %s"
                          id1 ) ) ;
                  let id2 = int_text (connection_id db) in
                  is_true ~msg:"a new connection" (id1 <> id2) ;
                  ignore (Db.exec db (q "INSERT INTO pb VALUES (1)")) ;
                  (* The INSERT committed at once: root sees the row, and no
                     transaction of the session stays. *)
                  equal ~msg:"root sees the row" string "1"
                    (admin
                       (Printf.sprintf "SELECT COUNT(*) FROM %s.pb" database) ) ;
                  equal ~msg:"no open transaction" string "0"
                    (admin
                       (Printf.sprintf
                          "SELECT COUNT(*) FROM information_schema.INNODB_TRX \
                           WHERE trx_mysql_thread_id = %s"
                          id2 ) ) ;
                  ignore (Db.exec db (q "DROP TABLE pb")) ) ) )
    ; test "a killed idle connection: a new connection, no error (V1)"
        (fun () ->
          in_weft (fun () ->
              warm () ;
              Db.with_db ~pool_size:1 (config ()) (fun db ->
                  let id1 = int_text (connection_id db) in
                  kill (Int64.of_string id1) ;
                  (* The counters are global to the process. This holds, because
                     the suite runs one test at a time, and only this pool runs
                     now. *)
                  Cistern.Private.Pool.Private.reset_counters () ;
                  let r = first (Db.exec db (q "SELECT 5")) in
                  (* The stale check finds the end of the stream at the
                     checkout, before the reset of the dirty entry. *)
                  equal ~msg:"the stale check" int 1
                    (Cistern.Private.Pool.Private.counters ()).stale_at_checkout ;
                  let id2 = int_text (connection_id db) in
                  equal string "Int 5" r ;
                  is_true ~msg:"a new connection" (id1 <> id2) ) ) )
    ; test "a read deadline: Outcome_unknown, and the pool stays usable"
        (fun () ->
          in_weft (fun () ->
              warm () ;
              Db.with_db ~pool_size:1 (config ()) (fun db ->
                  let t0 = Time.now Weft.Clock.Mono in
                  let a =
                    first
                      (Db.exec ~timeout:(Span.ms 200) db (q "SELECT SLEEP(2)"))
                  in
                  let t = seconds t0 in
                  let b = first (Db.exec db (q "SELECT 3")) in
                  equal string "Outcome_unknown statement" a ;
                  is_true ~msg:"at the deadline" (t >= 0.19 && t < 1.) ;
                  equal string "Int 3" b ) ) )
    ; test "a wrong password: Connect at the deadline" (fun () ->
          in_weft (fun () ->
              warm () ;
              let on_query, events = recorder () in
              let r, t =
                Db.with_db ~on_query
                  (on_socket ~clear:true ~password:"wrong" ()) (fun db ->
                    let t0 = Time.now Weft.Clock.Mono in
                    let r =
                      first (Db.exec ~timeout:(Span.ms 800) db (q "SELECT 1"))
                    in
                    (r, seconds t0) )
              in
              is_true ~msg:("a Connect error: " ^ r)
                (String.starts_with ~prefix:"Connect " r) ;
              is_true ~msg:"the error number 1045" (contains r "1045") ;
              is_true ~msg:"at the deadline" (t >= 0.79 && t < 2.) ;
              is_true ~msg:"Connect events with the error"
                (List.exists
                   (function
                     | Ev.Connect {error= Some _; _} -> true | _ -> false )
                   (events ()) ) ) )
    ; test "the end of with_db with execs in flight" (fun () ->
          in_weft (fun () ->
              warm () ;
              let first_id = last_session_id () in
              (* The forks can run on other domains (Weft_os runs up to 4): the
                 values that they give are under a mutex or in an Atomic. *)
              let m = Mutex.create () in
              let holders = ref [] and waiter = Atomic.make "" in
              let after = ref "" in
              Fiber.scope (fun s ->
                  let last = ref None in
                  Db.with_db ~pool_size:2 (config ()) (fun db ->
                      last := Some db ;
                      for _ = 1 to 2 do
                        Fiber.fork s (fun () ->
                            let r = first (Db.exec db (q "SELECT SLEEP(1)")) in
                            Mutex.protect m (fun () -> holders := r :: !holders) )
                      done ;
                      Time.sleep Weft.Clock.Mono (Span.ms 200) ;
                      Fiber.fork s (fun () ->
                          Atomic.set waiter (first (Db.exec db (q "SELECT 1"))) ) ;
                      Time.sleep Weft.Clock.Mono (Span.ms 100) ) ;
                  after :=
                    match Db.exec (Option.get !last) (q "SELECT 2") with
                    | _ ->
                        "no error"
                    | exception Invalid_argument m ->
                        m ) ;
              (* The holders end their statements. Their connections come back
                 to a closed pool, and the pool aborts them. *)
              equal (list string) ["Int 0"; "Int 0"]
                (Mutex.protect m (fun () -> !holders)) ;
              equal string "Connection_lost" (Atomic.get waiter) ;
              equal string "Db.t used after with_db" !after ;
              wait_until "end of the sessions of the test user" (fun () ->
                  user_sessions ~after:first_id () = 0 ) ) ) ]

(* The timing tests (nightly). *)
let timing =
  group ("Db timing on " ^ server)
    [ test "the idle limit from a global wait_timeout of 6 s (V1, V2)" (fun () ->
          in_weft (fun () ->
              warm () ;
              (* The default of the server comes back after the test, also if an
                 earlier run stopped with the value of this test. *)
              Fun.protect
                ~finally:(fun () ->
                  ignore (admin "SET GLOBAL wait_timeout = DEFAULT") )
                (fun () ->
                  ignore (admin "SET GLOBAL wait_timeout = 6") ;
                  (* A new session reads 6 s: an idle limit of 1 s. *)
                  Db.with_db ~pool_size:1 (config ()) (fun db ->
                      let id1 = connection_id db in
                      Time.sleep Weft.Clock.Mono (Span.s 2) ;
                      let r = first (Db.exec db (q "SELECT 7")) in
                      let id2 = connection_id db in
                      equal string "Int 7" r ;
                      is_true ~msg:"a new connection" (id1 <> id2) ) ) ) )
    ; test "max_lifetime of 1 s" (fun () ->
          in_weft (fun () ->
              warm () ;
              Db.with_db ~pool_size:1 ~max_lifetime:(Span.s 1) (config ())
                (fun db ->
                  let id1 = connection_id db in
                  Time.sleep Weft.Clock.Mono (Span.ms 1500) ;
                  let id2 = connection_id db in
                  is_true ~msg:"a new connection" (id1 <> id2) ) ) ) ]
