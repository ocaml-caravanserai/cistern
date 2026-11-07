(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* The integration tests of Db.transaction on the real servers (the plan of
   phase 1.3, section 4.3, and the facts V4, V8, V10 and V11 of section 7). The
   group [deterministic] runs with make integration. A wait on a server state
   polls PROCESSLIST, not a clock. The group [timing] runs only in the nightly
   db.yml. *)

open Windtrap
open Support
module Db = Cistern.Db
module Tx = Cistern.Tx
module Raw = Cistern.Raw
module Ev = Cistern_core.Event
module Span = Weft.Span
module Time = Weft.Time
module Fiber = Weft.Fiber
module Ivar = Weft.Ivar

let mono = Weft.Clock.Mono

let q s = Raw.query (Raw.sql s)

let x s = Raw.exec (Raw.sql s)

let ok_text = function Ok _ -> "ok" | Error e -> error_text e

(* The result of a transaction that gives a string. *)
let tx_text = function Ok v -> "ok " ^ v | Error e -> error_text e

let insert tx table id =
  ignore
    (Tx.exec_exn tx
       (x (Printf.sprintf "INSERT INTO %s (id) VALUES (%d)" table id)) )

(* The ids of [table], as root sees them: "1,2", or "" for no row. *)
let ids table =
  admin
    (Printf.sprintf
       "SELECT COALESCE(GROUP_CONCAT(id ORDER BY id), '') FROM %s.%s" database
       table )

(* The count of the Query events with the redacted SQL [sql]. *)
let events_of events sql =
  List.length
    (List.filter
       (function Ev.Query {sql= s; _} -> s = sql | _ -> false)
       (events ()) )

(* [with_table name f]: a pool of [pool_size] with an event recorder, and a new
   table [name] (id, v), then [f db events]. The table is dropped at the end. *)
let with_table ?(pool_size = 2) name f =
  in_weft (fun () ->
      warm () ;
      let on_query, events = Db_tests.recorder () in
      Db.with_db ~pool_size ~on_query (Db_tests.config ()) (fun db ->
          let run s = ignore (Db.exec_exn db (x s)) in
          run ("DROP TABLE IF EXISTS " ^ name) ;
          run
            ( "CREATE TABLE " ^ name
            ^ " (id INT PRIMARY KEY, v INT NOT NULL DEFAULT 0) ENGINE=InnoDB" ) ;
          Fun.protect
            ~finally:(fun () -> run ("DROP TABLE IF EXISTS " ^ name))
            (fun () -> f db events) ) )

(* The sessions of the test user that run [info] now. *)
let running info =
  int_of_string
    (admin
       (Printf.sprintf
          "SELECT COUNT(*) FROM information_schema.PROCESSLIST WHERE USER = \
           '%s' AND INFO = '%s'"
          user info ) )

let connection_id tx = Db_tests.first (Tx.exec tx (q "SELECT CONNECTION_ID()"))

(* The values that one fiber of a test gives to another, in an Ivar: [Ok v], or
   [Error m] when the fiber raised. Thus a broken library fails the test, and
   does not hang it. *)

(* [await what iv]: the value of [iv], at most 5 s. *)
let await what iv =
  match
    Weft.sync
      (Weft.choose
         [ Weft.wrap (Ivar.read iv) Option.some
         ; Weft.wrap (Time.after mono (Span.s 5)) (fun () -> None) ] )
  with
  | Some (Ok v) ->
      v
  | Some (Error m) ->
      failwith
        (Printf.sprintf "integration: %s: the other fiber raised %s" what m)
  | None ->
      failwith ("integration: no " ^ what ^ " after 5 s")

(* [fail_of iv]: the fill of [iv] with an error, if it is empty. *)
let fail_of iv m = ignore (Ivar.try_fill iv (Error m))

(* [filling fails f]: [f ()]. If it raises, each function of [fails] gets the
   exception as text, then the exception goes up. *)
let filling fails f =
  match f () with
  | v ->
      v
  | exception e ->
      let bt = Printexc.get_raw_backtrace () in
      List.iter (fun fail -> fail (Printexc.to_string e)) fails ;
      Printexc.raise_with_backtrace e bt

(* The deadlock of two Tx (two fibers). Each side updates its row, waits until
   the two rows are locked, then updates the row of the other side. Side 1
   starts its second UPDATE first, and side 2 starts its own 300 ms later, when
   the UPDATE of side 1 waits for the lock. [scope] wraps the second UPDATE (for
   a savepoint). For each side: the result of the second UPDATE, the result of a
   later Tx.exec, and the result of the transaction. *)
let deadlock db ~scope =
  let locked = [|Ivar.create (); Ivar.create ()|] in
  Fiber.scope (fun s ->
      let side me =
        Fiber.fork_promise s (fun () ->
            filling [fail_of locked.(me - 1)]
            @@ fun () ->
            let other = 3 - me in
            let second = ref "" and later = ref "" in
            let r =
              Db.transaction db (fun tx ->
                  let update id =
                    Tx.exec tx
                      (x
                         (Printf.sprintf "UPDATE dl SET v = %d WHERE id = %d" me
                            id ) )
                  in
                  ignore (Result.get_ok (update me)) ;
                  Ivar.fill locked.(me - 1) (Ok ()) ;
                  await "the lock of the other side" locked.(other - 1) ;
                  if me = 2 then Time.sleep mono (Span.ms 300) ;
                  second := scope tx (fun () -> ok_text (update other)) ;
                  later :=
                    ok_text (Tx.exec tx (q (Printf.sprintf "SELECT %d" me))) ;
                  "" )
            in
            (!second, !later, tx_text r) )
      in
      let p1 = side 1 and p2 = side 2 in
      [Weft.Promise.await p1; Weft.Promise.await p2] )

(* The checks of [deadlock]: one victim, its later Tx.exec gives the first error
   with no I/O, the other side commits, and the two rows have the value of the
   other side. *)
let check_deadlock events results =
  let victims =
    List.filter (fun (second, _, _) -> second = "Retryable deadlock") results
  in
  equal ~msg:"one victim" int 1 (List.length victims) ;
  let winner =
    List.concat
      (List.mapi
         (fun i (second, later, r) ->
           if second = "Retryable deadlock" then (
             equal ~msg:"the later Tx.exec" string "Retryable deadlock" later ;
             equal ~msg:"no I/O for the later Tx.exec" int 0
               (events_of events (Printf.sprintf "SELECT %d" (i + 1))) ;
             equal ~msg:"the transaction of the victim" string
               "Retryable deadlock" r ;
             [] )
           else (
             equal ~msg:"the second UPDATE of the winner" string "ok" second ;
             equal ~msg:"the later Tx.exec of the winner" string "ok" later ;
             equal ~msg:"the transaction of the winner" string "ok " r ;
             [i + 1] ) )
         results )
  in
  let w = List.hd winner in
  equal ~msg:"no write of the victim stays" string
    (Printf.sprintf "%d,%d" w w)
    (admin
       (Printf.sprintf "SELECT GROUP_CONCAT(v ORDER BY id) FROM %s.dl" database) )

let setup_deadlock db =
  ignore (Db.exec_exn db (x "INSERT INTO dl (id) VALUES (1), (2)"))

(* Root holds FLUSH TABLES WITH READ LOCK on its own connection, while the
   COMMIT of a Tx waits (the fact R4). [during c id] runs when the COMMIT waits,
   with the root connection and the connection id of the Tx. [body] wraps the
   transaction, and [timeout] is its timeout. *)
let with_waiting_commit ?(timeout = Span.s 30) ?(body = fun f -> f ()) db during
    =
  let inserted = Ivar.create () and lock = Ivar.create () in
  Fiber.scope (fun s ->
      let p =
        Fiber.fork_promise s (fun () ->
            filling [fail_of inserted]
            @@ fun () ->
            body (fun () ->
                tx_text
                  (Db.transaction ~timeout db (fun tx ->
                       insert tx "fw" 1 ;
                       Ivar.fill inserted (Ok (connection_id tx)) ;
                       await "the FLUSH TABLES of root" lock ;
                       "v" ) ) ) )
      in
      ( filling [fail_of lock]
      @@ fun () ->
      with_conn (root ()) (fun c ->
          let id = await "the INSERT of the Tx" inserted in
          exec_ok c "FLUSH TABLES WITH READ LOCK" ;
          Fun.protect
            ~finally:(fun () -> exec_ok c "UNLOCK TABLES")
            (fun () ->
              Ivar.fill lock (Ok ()) ;
              Db_tests.wait_until "a COMMIT that waits" (fun () ->
                  running "COMMIT" = 1 ) ;
              during c (Db_tests.int_text id) ) ) ) ;
      Weft.Promise.await p )

let deterministic =
  group
    ("Db.transaction on " ^ server)
    [ test "COMMIT of two writes, ROLLBACK after Cistern_error" (fun () ->
          with_table "cr" (fun db events ->
              let r =
                Db.transaction db (fun tx ->
                    insert tx "cr" 1 ; insert tx "cr" 2 ; "v" )
              in
              equal ~msg:"the commit" string "ok v" (tx_text r) ;
              equal ~msg:"root sees the two rows" string "1,2" (ids "cr") ;
              let r =
                Db.transaction db (fun tx ->
                    insert tx "cr" 3 ; insert tx "cr" 1 ; "v" )
              in
              equal ~msg:"the error of the callback" string
                "Constraint_violation Unique" (tx_text r) ;
              equal ~msg:"one ROLLBACK" int 1 (events_of events "ROLLBACK") ;
              equal ~msg:"no write of the second Tx" string "1,2" (ids "cr") ) )
    ; test "a deadlock of two Tx: one victim, no write of it stays" (fun () ->
          with_table "dl" (fun db events ->
              setup_deadlock db ;
              check_deadlock events (deadlock db ~scope:(fun _ f -> f ())) ) )
    ; test "a deadlock in a savepoint: the first error, no savepoint command"
        (fun () ->
          with_table "dl" (fun db events ->
              setup_deadlock db ;
              (* The scope handles the error of the UPDATE and returns. *)
              let scope tx f =
                let inner = ref "" in
                let r =
                  Tx.savepoint tx (fun _ ->
                      inner := f () ;
                      "" )
                in
                if !inner = "Retryable deadlock" then
                  equal ~msg:"the savepoint of the victim" string
                    "Retryable deadlock" (tx_text r) ;
                !inner
              in
              check_deadlock events (deadlock db ~scope) ;
              equal ~msg:"only the RELEASE of the winner" int 1
                (events_of events "RELEASE SAVEPOINT cistern_sp_1") ;
              equal ~msg:"no ROLLBACK TO" int 0
                (events_of events "ROLLBACK TO SAVEPOINT cistern_sp_1") ) )
    ; test "TRUNCATE and CREATE TABLE in a Tx: Implicit_commit (V4)" (fun () ->
          with_table "ic" (fun db _ ->
              ignore (Db.exec_exn db (x "DROP TABLE IF EXISTS ic2")) ;
              ignore (Db.exec_exn db (x "CREATE TABLE ic2 (id INT)")) ;
              List.iteri
                (fun i ddl ->
                  let first = ref "" and later = ref "" in
                  let r =
                    Db.transaction db (fun tx ->
                        insert tx "ic" (i + 1) ;
                        first := ok_text (Tx.exec tx (x ddl)) ;
                        later := ok_text (Tx.exec tx (q "SELECT 1")) ;
                        "v" )
                  in
                  equal ~msg:ddl string "Implicit_commit" !first ;
                  equal ~msg:"the later Tx.exec" string "Implicit_commit" !later ;
                  equal ~msg:"the transaction" string "Implicit_commit"
                    (tx_text r) )
                ["TRUNCATE TABLE ic2"; "CREATE TABLE ic3 (id INT)"] ;
              equal ~msg:"root sees the writes before the DDL" string "1,2"
                (ids "ic") ;
              ignore (Db.exec_exn db (x "DROP TABLE ic2, ic3")) ) )
    ; test "a failed DDL in a Tx: Implicit_commit, the write committed (V10)"
        (fun () ->
          with_table "fd" (fun db events ->
              let first = ref "" and sp = ref "" and ran = ref false in
              let r =
                Db.transaction db (fun tx ->
                    insert tx "fd" 1 ;
                    first := ok_text (Tx.exec tx (x "CREATE TABLE fd (id INT)")) ;
                    (* A savepoint after the first error: the error, with no
                       I/O, and the scope does not run. *)
                    sp :=
                      tx_text
                        (Tx.savepoint tx (fun _ ->
                             ran := true ;
                             "" ) ) ;
                    "v" )
              in
              equal ~msg:"CREATE TABLE of a table that exists" string
                "Implicit_commit" !first ;
              equal ~msg:"the savepoint after it" string "Implicit_commit" !sp ;
              is_false ~msg:"the scope does not run" !ran ;
              equal ~msg:"no SAVEPOINT" int 0
                (events_of events "SAVEPOINT cistern_sp_1") ;
              equal ~msg:"the transaction" string "Implicit_commit" (tx_text r) ;
              equal ~msg:"root sees the write" string "1" (ids "fd") ) )
    ; test "1062 and 1064 in a Tx: the Tx stays usable" (fun () ->
          with_table "su" (fun db _ ->
              let dup = ref "" and syntax = ref "" in
              let r =
                Db.transaction db (fun tx ->
                    insert tx "su" 1 ;
                    dup :=
                      ok_text (Tx.exec tx (x "INSERT INTO su (id) VALUES (1)")) ;
                    insert tx "su" 2 ;
                    (* An unknown word: the server gives 1064, and the ping
                       shows IN_TRANS set. *)
                    syntax := ok_text (Tx.exec tx (x "SELEC 1")) ;
                    insert tx "su" 3 ;
                    "v" )
              in
              equal ~msg:"1062" string "Constraint_violation Unique" !dup ;
              is_true ~msg:("1064: " ^ !syntax)
                (String.starts_with ~prefix:"Driver 1064" !syntax) ;
              equal ~msg:"the commit" string "ok v" (tx_text r) ;
              equal ~msg:"root sees the other writes" string "1,2,3" (ids "su") ) )
    ; test "savepoints: RELEASE after a value, ROLLBACK TO after Cistern_error"
        (fun () ->
          with_table "sp" (fun db events ->
              let a = ref "" and b = ref "" in
              let r =
                Db.transaction db (fun tx ->
                    insert tx "sp" 1 ;
                    a :=
                      tx_text
                        (Tx.savepoint tx (fun tx -> insert tx "sp" 2 ; "a")) ;
                    b :=
                      tx_text
                        (Tx.savepoint tx (fun tx ->
                             insert tx "sp" 3 ; insert tx "sp" 1 ; "b" ) ) ;
                    insert tx "sp" 4 ;
                    "v" )
              in
              equal ~msg:"the first savepoint" string "ok a" !a ;
              equal ~msg:"the second savepoint" string
                "Constraint_violation Unique" !b ;
              equal ~msg:"the commit" string "ok v" (tx_text r) ;
              equal ~msg:"RELEASE" int 1
                (events_of events "RELEASE SAVEPOINT cistern_sp_1") ;
              equal ~msg:"ROLLBACK TO" int 1
                (events_of events "ROLLBACK TO SAVEPOINT cistern_sp_2") ;
              equal ~msg:"the write of the second savepoint is gone" string
                "1,2,4" (ids "sp") ) )
    ; test "the first-word check and the fiber check: no I/O" (fun () ->
          with_table "fc" (fun db events ->
              let refused = ref [] in
              let r =
                Db.transaction db (fun tx ->
                    List.iter
                      (fun s ->
                        match Tx.exec tx (x s) with
                        | _ ->
                            refused := (s ^ ": no exception") :: !refused
                        | exception Invalid_argument _ ->
                            () )
                      [ "START TRANSACTION"
                      ; "SET autocommit = 0"
                      ; "/* */ COMMIT"
                      ; "EXECUTE s" ] ;
                    ( match Db.exec db (q "SELECT 5") with
                    | _ ->
                        refused := "Db.exec: no exception" :: !refused
                    | exception Invalid_argument _ ->
                        () ) ;
                    insert tx "fc" 1 ; "v" )
              in
              equal (list string) [] !refused ;
              List.iter
                (fun s ->
                  equal ~msg:("no event of " ^ s) int 0 (events_of events s) )
                ["SET autocommit = 0"; "/* */ COMMIT"; "EXECUTE s"; "SELECT 5"] ;
              (* The START TRANSACTION of Db.transaction gives one event. *)
              equal ~msg:"one START TRANSACTION" int 1
                (events_of events "START TRANSACTION") ;
              equal ~msg:"the commit" string "ok v" (tx_text r) ;
              equal ~msg:"root sees the write" string "1" (ids "fc") ) )
    ; test "a read deadline of Tx.exec: Outcome_unknown, then Timeout (D5)"
        (fun () ->
          with_table "rd" (fun db _ ->
              let sleep = ref "" in
              let t0 = Time.now mono in
              let r =
                Db.transaction db (fun tx ->
                    insert tx "rd" 1 ;
                    sleep :=
                      Db_tests.first
                        (Tx.exec ~timeout:(Span.ms 200) tx (q "SELECT SLEEP(2)")) ;
                    "v" )
              in
              let t = Db_tests.seconds t0 in
              equal ~msg:"Tx.exec" string "Outcome_unknown statement" !sleep ;
              equal ~msg:"the transaction" string "Timeout" (tx_text r) ;
              is_true ~msg:"at the deadline" (t >= 0.19 && t < 1.) ;
              equal ~msg:"no write stays" string "" (ids "rd") ) )
    ; test "a Tx past its deadline: Timeout, ROLLBACK, no write stays"
        (fun () ->
          (* A wait in the callback: the timer of the Tx cancels it at the
             deadline. A wait in a shield: the callback returns after the
             deadline, and the time check gives Timeout. The test takes the time
             at the end of the callback: on MySQL 8.4, the ROLLBACK of an INSERT
             into a new table can take up to about 1 s on the server. *)
          List.iter
            (fun (name, wait, low, high) ->
              with_table "pd" (fun db events ->
                  let t0 = Time.now mono in
                  let t = ref 0. in
                  let r =
                    Db.transaction ~timeout:(Span.ms 300) db (fun tx ->
                        insert tx "pd" 1 ;
                        Fun.protect
                          ~finally:(fun () -> t := Db_tests.seconds t0)
                          (fun () ->
                            wait (fun () -> Time.sleep mono (Span.ms 400)) ) ;
                        "v" )
                  in
                  let t = !t in
                  equal ~msg:name string "Timeout" (tx_text r) ;
                  is_true
                    ~msg:
                      (Printf.sprintf "%s: the end of the callback at %.3f s"
                         name t )
                    (t >= low && t < high) ;
                  equal ~msg:"one ROLLBACK" int 1 (events_of events "ROLLBACK") ;
                  equal ~msg:"no write stays" string "" (ids "pd") ) )
            [ ("a wait", (fun f -> f ()), 0.29, 0.39)
            ; ("a wait in a shield", Fiber.shield, 0.39, 0.6) ] )
    ; test "KILL QUERY of a waiting COMMIT: Outcome_unknown commit (V8)"
        (fun () ->
          with_table ~pool_size:1 "fw" (fun db _ ->
              let r =
                with_waiting_commit db (fun _ id ->
                    ignore (admin ("KILL QUERY " ^ id)) ;
                    Db_tests.wait_until "the end of the COMMIT" (fun () ->
                        running "COMMIT" = 0 ) )
              in
              equal ~msg:"the transaction" string "Outcome_unknown commit" r ;
              equal ~msg:"the COMMIT rolled back" string "" (ids "fw") ) )
    ; test "a cancel while COMMIT waits: the result of COMMIT (the shield)"
        (fun () ->
          with_table ~pool_size:1 "fw" (fun db _ ->
              let t0 = Time.now mono in
              let seen = ref 0. in
              let r =
                with_waiting_commit db
                  ~body:(fun f ->
                    match Fiber.with_timeout mono (Span.s 1) f with
                    | r ->
                        r
                    | exception Fiber.Timeout ->
                        "cancelled" )
                  (fun _ _ ->
                    seen := Db_tests.seconds t0 ;
                    (* The timer of 1 s fires while the COMMIT waits. *)
                    Time.sleep mono (Span.ms 1300) )
              in
              is_true ~msg:"the COMMIT waited before the timer" (!seen < 0.9) ;
              equal ~msg:"the transaction" string "ok v" r ;
              equal ~msg:"root sees the write" string "1" (ids "fw") ) )
    ; test "a global completion_type CHAIN: the COMMIT does not chain (V11)"
        (fun () ->
          Fun.protect
            ~finally:(fun () ->
              ignore (admin "SET GLOBAL completion_type = DEFAULT") )
            (fun () ->
              ignore (admin "SET GLOBAL completion_type = 'CHAIN'") ;
              with_table ~pool_size:1 "ch" (fun db _ ->
                  let id n =
                    Db.transaction db (fun tx ->
                        insert tx "ch" n ; connection_id tx )
                  in
                  let a = tx_text (id 1) in
                  let b = tx_text (id 2) in
                  (* An OK of COMMIT with IN_TRANS set aborts the connection at
                     checkin: the second Tx then has another id. *)
                  equal ~msg:"the same connection" string a b ;
                  equal ~msg:"root sees the two writes" string "1,2" (ids "ch") ) ) )
    ]

(* The timing tests (nightly). *)
let timing =
  group
    ("Db.transaction timing on " ^ server)
    [ test "a lock wait cut by the deadline of the Tx: Timeout (D5)" (fun () ->
          with_table "lw" (fun db _ ->
              ignore (Db.exec_exn db (x "INSERT INTO lw (id) VALUES (1)")) ;
              let locked = Ivar.create () and release = Ivar.create () in
              Fiber.scope (fun s ->
                  let holder =
                    Fiber.fork_promise s (fun () ->
                        filling [fail_of locked]
                        @@ fun () ->
                        tx_text
                          (Db.transaction db (fun tx ->
                               ignore
                                 (Tx.exec_exn tx
                                    (x "UPDATE lw SET v = 1 WHERE id = 1") ) ;
                               Ivar.fill locked (Ok ()) ;
                               await "the end of the waiter" release ;
                               "holder" ) ) )
                  in
                  let wait = ref "" in
                  let t0 = ref (Time.now mono) in
                  let r =
                    (* The holder ends after the waiter, also if the waiter
                       raises. *)
                    Fun.protect
                      ~finally:(fun () -> ignore (Ivar.try_fill release (Ok ())))
                      (fun () ->
                        await "the lock of the holder" locked ;
                        t0 := Time.now mono ;
                        Db.transaction ~timeout:(Span.ms 500) db (fun tx ->
                            wait :=
                              ok_text
                                (Tx.exec tx
                                   (x "UPDATE lw SET v = 2 WHERE id = 1") ) ;
                            "waiter" ) )
                  in
                  let t = Db_tests.seconds !t0 in
                  equal ~msg:"the UPDATE that waits" string
                    "Outcome_unknown statement" !wait ;
                  equal ~msg:"the transaction" string "Timeout" (tx_text r) ;
                  is_true ~msg:"at the deadline" (t >= 0.49 && t < 1.5) ;
                  equal ~msg:"the holder" string "ok holder"
                    (Weft.Promise.await holder) ) ;
              equal ~msg:"the value of the holder" string "1"
                (admin (Printf.sprintf "SELECT v FROM %s.lw" database)) ) )
    ; test "a COMMIT after the deadline of the Tx: at least 5 s for it"
        (fun () ->
          with_table ~pool_size:1 "fw" (fun db _ ->
              let r =
                (* The deadline of the Tx passes while the COMMIT waits. The
                   flow deadline of the COMMIT is at least 5 s. *)
                with_waiting_commit ~timeout:(Span.s 1) db (fun _ _ ->
                    Time.sleep mono (Span.ms 2500) )
              in
              equal ~msg:"the transaction" string "ok v" r ;
              equal ~msg:"root sees the write" string "1" (ids "fw") ) ) ]
