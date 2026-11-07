(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* Db.transaction and Tx against the fake server of many connections, with its
   state of a transaction (the plan of phase 1.3, sections 3.3 to 3.9). Each
   test runs in with_pool of Test_pool, which requires no socket and no slot
   after with_db. *)

open Windtrap
open Weft
module Db = Cistern.Db
module Tx = Cistern.Tx
module Raw = Cistern.Raw
module Pool = Cistern.Private.Pool
module E = Cistern_core.Error
module D = Fake_db
module T = Test_pool

let q = T.q

let x s = Raw.exec (Raw.sql s)

(* The text of a result. *)
let text = function Ok _ -> "Ok" | Error e -> T.error_text e

(* The SQL commands that the fake server got, the oldest first, from [from] on.
   A COM_PING shows as "ping". *)
let log ~from (db : D.t) =
  List.rev db.log
  |> List.filteri (fun i _ -> i >= from)
  |> List.map (fun s -> if s = "" then "ping" else s)

(* The text of the outcome of Db.transaction, also of its exceptions. *)
let outcome show f =
  match f () with
  | v ->
      show v
  | exception Invalid_argument m ->
      "Invalid_argument " ^ m
  | exception Failure m ->
      "Failure " ^ m
  | exception Fiber.Timeout ->
      "Fiber.Timeout"

(* [run_tx ?size ?timeout ?setup f]: a pool and one exec (a connection), then
   Db.transaction with [f db tx]. [f] writes the texts of its steps into
   [inner]. It gives the text of the result, the inner texts, the commands of
   the fake server after the warm-up, the count of the connects, and the events
   of the transaction. *)
let run_tx ?(size = 1) ?(timeout = Span.s 30) ?(setup = ignore) f =
  let inner = ref [] in
  let note s = inner := s :: !inner in
  let (r, cmds, connects), events =
    T.with_pool ~size ~setup (fun _ db pool ->
        (* A statement that is not raw: the entry stays clean, thus the checkout
           of the Tx sends no reset. *)
        ignore (Db.exec pool (T.clean ())) ;
        let from = List.length db.log in
        let r =
          outcome text (fun () -> Db.transaction ~timeout pool (f db note))
        in
        (r, log ~from db, db.connects) )
  in
  ( r
  , String.concat " / " (List.rev !inner)
  , cmds
  , connects
  , List.filter (fun e -> e <> "query \"CUSTOM\" done 0") (T.queries events) )

let step note r = note (text r)

(* The events of COMMIT and ROLLBACK. *)
let end_events events =
  List.filter
    (fun e ->
      String.starts_with ~prefix:"query \"COMMIT\"" e
      || String.starts_with ~prefix:"query \"ROLLBACK\"" e )
    events

let tests =
  group "Db.transaction and Tx"
    [ test "a commit, its commands and its events" (fun () ->
          let r, _, cmds, _, events =
            run_tx (fun _ _ tx ->
                ignore (Tx.exec_exn tx (x "UPDATE a")) ;
                List.length (Tx.exec_exn tx (q "SELECT 5")).rows )
          in
          equal string "Ok" r ;
          equal (list string)
            ["START TRANSACTION"; "UPDATE a"; "SELECT 5"; "COMMIT"]
            cmds ;
          equal (list string)
            [ "query \"START TRANSACTION\" done 0"
            ; "query \"UPDATE a\" done 1"
            ; "query \"SELECT 5\" done 1"
            ; "query \"COMMIT\" done 0" ]
            events )
    ; cases "the errors of a statement (3.7)"
        ~id:(fun (n, _, _, _, _, _) -> n)
        [ ( "an implicit commit (an OK with IN_TRANS clear)"
          , false
          , "CREATE t"
          , "Implicit_commit / Implicit_commit"
          , "Implicit_commit"
          , ["START TRANSACTION"; "CREATE t"; "ROLLBACK"] )
        ; ( "1062: statement-only, no ping"
          , false
          , "FAIL 1062"
          , "Constraint_violation Unique / Ok"
          , "Ok"
          , ["START TRANSACTION"; "FAIL 1062"; "UPDATE b"; "COMMIT"] )
        ; ( "1146: a ping, IN_TRANS set, the Tx stays"
          , false
          , "FAIL 1146"
          , "Driver 1146 28000 / Ok"
          , "Ok"
          , ["START TRANSACTION"; "FAIL 1146"; "ping"; "UPDATE b"; "COMMIT"] )
        ; ( "a failed DDL: a ping, IN_TRANS clear, Implicit_commit"
          , false
          , "DDLFAIL"
          , "Implicit_commit / Implicit_commit"
          , "Implicit_commit"
          , ["START TRANSACTION"; "DDLFAIL"; "ping"; "ROLLBACK"] )
        ; ( "a deadlock: the Tx fails, no ping"
          , false
          , "DEADLOCK"
          , "Retryable deadlock / Retryable deadlock"
          , "Retryable deadlock"
          , ["START TRANSACTION"; "DEADLOCK"; "ROLLBACK"] )
        ; ( "1205 with no rollback: a ping, the Tx stays"
          , false
          , "LOCKWAIT"
          , "Retryable lock timeout / Ok"
          , "Ok"
          , ["START TRANSACTION"; "LOCKWAIT"; "ping"; "UPDATE b"; "COMMIT"] )
        ; ( "1205 with innodb_rollback_on_timeout: the Tx fails"
          , true
          , "LOCKWAIT"
          , "Retryable lock timeout / Retryable lock timeout"
          , "Retryable lock timeout"
          , ["START TRANSACTION"; "LOCKWAIT"; "ROLLBACK"] )
        ; ( "LOCAL INFILE: a broken connection, no ping, no ROLLBACK"
          , false
          , "INFILE"
          , "Decode / Decode"
          , "Decode"
          , ["START TRANSACTION"; "INFILE"] ) ]
        (fun ( _
             , rollback_on_timeout
             , sql
             , expected_inner
             , expected
             , expected_cmds )
           ->
          let r, inner, cmds, _, events =
            run_tx
              ~setup:(fun db ->
                db.faults.rollback_on_timeout <- rollback_on_timeout )
              (fun _ note tx ->
                step note (Tx.exec tx (x sql)) ;
                (* A failed Tx gives its first error, with no I/O. *)
                step note (Tx.exec tx (x "UPDATE b")) )
          in
          equal string expected_inner inner ;
          equal string expected r ;
          equal (list string) expected_cmds cmds ;
          (* The event of the statement has the error that Tx.exec gave. *)
          let first =
            List.hd (String.split_on_char '/' expected_inner) |> String.trim
          in
          is_true ~msg:"the event of the statement"
            (List.mem (Printf.sprintf "query %S failed %s" sql first) events) )
    ; test "Tx.savepoint after a first error: the error, no I/O" (fun () ->
          let ran = ref false in
          let r, inner, cmds, _, events =
            run_tx (fun _ note tx ->
                step note (Tx.exec tx (x "DEADLOCK")) ;
                step note (Tx.savepoint tx (fun _ -> ran := true)) )
          in
          equal string "Retryable deadlock / Retryable deadlock" inner ;
          equal string "Retryable deadlock" r ;
          is_false ~msg:"the scope does not run" !ran ;
          equal (list string) ["START TRANSACTION"; "DEADLOCK"; "ROLLBACK"] cmds ;
          is_false
            (List.exists
               (String.starts_with ~prefix:"query \"SAVEPOINT")
               events ) )
    ; test "the margin of a statement timeout fails only the statement"
        (fun () ->
          let r, inner, cmds, _, events =
            run_tx (fun _ note tx ->
                step note (Tx.exec ~timeout:(Span.ms 30) tx (x "UPDATE a")) ;
                step note (Tx.exec tx (x "UPDATE b")) )
          in
          equal string "Timeout / Ok" inner ;
          equal string "Ok" r ;
          equal (list string) ["START TRANSACTION"; "UPDATE b"; "COMMIT"] cmds ;
          is_true (List.mem "query \"UPDATE a\" failed Timeout" events) )
    ; test "the margin of the transaction deadline fails the Tx" (fun () ->
          let r, inner, cmds, _, _ =
            run_tx ~timeout:(Span.ms 300) (fun _ note tx ->
                Time.sleep Mono (Span.ms 260) ;
                step note (Tx.exec tx (x "UPDATE a")) )
          in
          equal string "Timeout" inner ;
          equal string "Timeout" r ;
          equal (list string) ["START TRANSACTION"; "ROLLBACK"] cmds )
    ; cases "a read deadline in a statement (D5)"
        ~id:(fun (n, _, _) -> n)
        [ ("a DML statement: Timeout", "UPDATE a", "Timeout")
        ; ( "a statement that is not DML: Outcome_unknown stays"
          , "TRUNCATE t"
          , "Outcome_unknown statement" ) ]
        (fun (_, sql, expected) ->
          let r, inner, cmds, connects, _ =
            run_tx (fun db note tx ->
                db.faults.latency_ms <- 1000 ;
                (* The statement deadline comes before the transaction deadline
                   (not at the same instant). *)
                step note (Tx.exec ~timeout:(Span.ms 200) tx (x sql)) ;
                db.faults.latency_ms <- 0 )
          in
          equal string "Outcome_unknown statement" inner ;
          equal string expected r ;
          (* The broken connection gets no ROLLBACK. The fake server logs a
             command after its latency, thus the cut statement is not in the
             log. *)
          equal (list string) ["START TRANSACTION"] cmds ;
          equal int 1 connects )
    ; cases "a cancel in a statement stores its error (C-H1, D7)"
        ~id:(fun (n, _, _) -> n)
        [ ("a DML statement: Connection_lost", "UPDATE a", "Connection_lost")
        ; ( "a statement that is not DML: Outcome_unknown"
          , "TRUNCATE t"
          , "Outcome_unknown statement" ) ]
        (fun (_, sql, expected) ->
          let r, inner, cmds, _, events =
            run_tx (fun db note tx ->
                db.faults.latency_ms <- 100 ;
                ( match
                    Fiber.with_timeout Mono (Span.ms 10) (fun () ->
                        Tx.exec tx (x sql) )
                  with
                | r ->
                    step note r
                | exception Fiber.Timeout ->
                    note "cut" ) ;
                db.faults.latency_ms <- 0 ;
                step note (Tx.exec tx (x "UPDATE b")) )
          in
          equal string ("cut / " ^ expected) inner ;
          equal string expected r ;
          (* No COMMIT and no ROLLBACK: the connection was aborted. *)
          equal (list string) ["START TRANSACTION"] cmds ;
          equal int 1
            (List.length
               (List.filter
                  (String.ends_with ~suffix:"cancelled sent=true")
                  events ) ) )
    ; cases "the first-word check" ~id:Fun.id
        [ "COMMIT"
        ; "start transaction"
        ; "SET autocommit = 0"
        ; "/* c */ COMMIT"
        ; " EXECUTE s"
        ; "(SELECT 1)" ] (fun sql ->
          let r, _, cmds, _, _ = run_tx (fun _ _ tx -> Tx.exec tx (x sql)) in
          equal string
            "Invalid_argument Tx.exec: a raw statement that a Tx refuses \
             (DESIGN.md, \"Transactions\")"
            r ;
          equal (list string) ["START TRANSACTION"; "ROLLBACK"] cmds )
    ; test "a Tx from another fiber, and after its end" (fun () ->
          let r, _ =
            T.with_pool ~size:2 (fun _ _ pool ->
                let saved = ref None and child = ref "" in
                let r =
                  Db.transaction pool (fun tx ->
                      saved := Some tx ;
                      Fiber.scope (fun s ->
                          Fiber.fork s (fun () ->
                              match Tx.exec tx (x "UPDATE a") with
                              | _ ->
                                  child := "no error"
                              | exception Invalid_argument m ->
                                  child := m ) ) )
                in
                let after =
                  match Tx.exec (Option.get !saved) (x "UPDATE a") with
                  | _ ->
                      "no error"
                  | exception Invalid_argument m ->
                      m
                in
                String.concat " / " [text r; !child; after] )
          in
          equal string
            "Ok / Tx.exec from another fiber than the fiber of the Tx / \
             Tx.exec after the end of the transaction"
            r )
    ; test "Db.exec and Db.transaction in the fiber of a Tx" (fun () ->
          let (r, inner), _ =
            T.with_pool ~size:2 (fun _ _ pool ->
                let inner = ref [] in
                let r =
                  Db.transaction pool (fun _ ->
                      ( match Db.exec pool (q "SELECT 1") with
                      | _ ->
                          inner := "no error" :: !inner
                      | exception Invalid_argument m ->
                          inner := m :: !inner ) ;
                      ( match Db.transaction pool (fun _ -> ()) with
                      | _ ->
                          inner := "no error" :: !inner
                      | exception Invalid_argument m ->
                          inner := m :: !inner ) ;
                      (* A child fiber gets a second connection. *)
                      Fiber.scope (fun s ->
                          Fiber.fork s (fun () ->
                              inner :=
                                T.result_text (Db.exec pool (q "SELECT 7"))
                                :: !inner ) ) )
                in
                (text r, String.concat " / " (List.rev !inner)) )
          in
          equal string "Ok" r ;
          equal string
            "Db.exec in the fiber of a Tx / Db.transaction in the fiber of a \
             Tx / 7"
            inner )
    ; test "the set of Tx fibers is empty after each path" (fun () ->
          let r, events =
            T.with_pool ~size:1
              ~setup:(fun db -> db.faults.refuse <- true)
              (fun _ db pool ->
                let p = Db.Private.pool pool in
                (* A checkout that fails. *)
                let a =
                  text (Db.transaction ~timeout:(Span.ms 100) pool ignore)
                in
                let after_a = Pool.tx_fibers p in
                (* A cancel in the wait. *)
                let b =
                  match
                    Fiber.with_timeout Mono (Span.ms 50) (fun () ->
                        Db.transaction pool ignore )
                  with
                  | _ ->
                      "no cancel"
                  | exception Fiber.Timeout ->
                      "cancelled"
                in
                let after_b = Pool.tx_fibers p in
                db.faults.refuse <- false ;
                (* An exception of the callback, then Db.exec in the same
                   fiber. *)
                let c =
                  outcome text (fun () ->
                      Db.transaction pool (fun _ -> failwith "bug") )
                in
                let d = T.result_text (Db.exec pool (q "SELECT 3")) in
                Printf.sprintf "%s %d %s %d %s %s %d" a after_a b after_b c d
                  (Pool.tx_fibers p) )
          in
          equal string
            "Connect the server closed the connection 0 cancelled 0 Failure \
             bug 3 0"
            r ;
          List.iter
            (fun e -> is_true ~msg:e (List.mem e events))
            [ "query \"START TRANSACTION\" failed Connect the server closed \
               the connection"
            ; "query \"START TRANSACTION\" cancelled sent=false" ] )
    ; test "Db.transaction in a fiber that is already cancelled" (fun () ->
          let r, events =
            T.with_pool ~size:1 (fun _ db pool ->
                ignore (Db.exec pool (q "SELECT 1")) ;
                let r =
                  match
                    Fiber.with_timeout Mono (Span.ms 1) (fun () ->
                        ( try Time.sleep Mono (Span.s 1)
                          with Fiber.Cancelled _ -> () ) ;
                        Db.transaction pool ignore )
                  with
                  | _ ->
                      "no cancel"
                  | exception Fiber.Timeout ->
                      "cancelled"
                in
                ignore (Db.exec pool (q "SELECT 2")) ;
                Printf.sprintf "%s %d" r db.connects )
          in
          (* No command, no event, and the idle connection stays good. *)
          equal string "cancelled 1" r ;
          is_false
            (List.exists
               (String.starts_with ~prefix:"query \"START TRANSACTION\"")
               events ) )
    ; cases "the end of the callback: the priority rule (3.6.1)"
        ~id:(fun (n, _, _, _, _) -> n)
        [ ( "1 a cancel of the caller: no COMMIT"
          , Span.s 30
          , (fun _ _ -> Time.sleep Mono (Span.s 1))
          , "cancelled"
          , ["START TRANSACTION"] )
        ; ( "2 a user exception: ROLLBACK, then it goes up"
          , Span.s 30
          , (fun _ tx ->
              ignore (Tx.exec tx (x "UPDATE a")) ;
              failwith "bug" )
          , "Failure bug"
          , ["START TRANSACTION"; "UPDATE a"; "ROLLBACK"] )
        ; ( "2 a user exception after an abort: no ROLLBACK"
          , Span.s 30
          , (fun db tx ->
              db.D.faults.latency_ms <- 100 ;
              ( try
                  ignore
                    (Fiber.with_timeout Mono (Span.ms 10) (fun () ->
                         Tx.exec tx (x "UPDATE a") ) )
                with Fiber.Timeout -> () ) ;
              db.D.faults.latency_ms <- 0 ;
              failwith "bug" )
          , "Failure bug"
          , ["START TRANSACTION"] )
        ; ( "2 a Fiber.Timeout before the deadline is a user exception"
          , Span.s 30
          , (fun _ _ -> raise Fiber.Timeout)
          , "Fiber.Timeout"
          , ["START TRANSACTION"; "ROLLBACK"] )
        ; ( "3 a failed DDL, then the deadline: Implicit_commit"
          , Span.ms 500
          , (fun _ tx ->
              ignore (Tx.exec tx (x "DDLFAIL")) ;
              Time.sleep Mono (Span.s 1) )
          , "Implicit_commit"
          , ["START TRANSACTION"; "DDLFAIL"; "ping"; "ROLLBACK"] )
        ; ( "3 a failed DDL, then a Cistern_error: Implicit_commit"
          , Span.s 30
          , (fun _ tx ->
              ignore (Tx.exec tx (x "DDLFAIL")) ;
              raise (E.Cistern_error E.Not_found) )
          , "Implicit_commit"
          , ["START TRANSACTION"; "DDLFAIL"; "ping"; "ROLLBACK"] )
        ; ( "4 a Cistern_error after a deadlock: the Cistern_error"
          , Span.s 30
          , (fun _ tx ->
              ignore (Tx.exec tx (x "DEADLOCK")) ;
              raise (E.Cistern_error E.Not_found) )
          , "Not_found"
          , ["START TRANSACTION"; "DEADLOCK"; "ROLLBACK"] )
        ; ( "4 a Cistern_error: ROLLBACK"
          , Span.s 30
          , (fun _ _ -> raise (E.Cistern_error E.Not_found))
          , "Not_found"
          , ["START TRANSACTION"; "ROLLBACK"] )
        ; ( "5 a value after the deadline: ROLLBACK and Timeout"
          , Span.ms 500
          , (fun _ _ ->
              (* The callback catches the cancel of the deadline. *)
              try Time.sleep Mono (Span.s 1) with Fiber.Cancelled _ -> () )
          , "Timeout"
          , ["START TRANSACTION"; "ROLLBACK"] )
        ; ( "5 the Fiber.Timeout of the deadline: ROLLBACK and Timeout"
          , Span.ms 500
          , (fun _ _ -> Time.sleep Mono (Span.s 1))
          , "Timeout"
          , ["START TRANSACTION"; "ROLLBACK"] )
        ; ( "6 a value after a deadlock: ROLLBACK and the first error"
          , Span.s 30
          , (fun _ tx -> ignore (Tx.exec tx (x "DEADLOCK")))
          , "Retryable deadlock"
          , ["START TRANSACTION"; "DEADLOCK"; "ROLLBACK"] ) ]
        (fun (_, timeout, f, expected, expected_cmds) ->
          let (r, cmds), events =
            T.with_pool ~size:1 (fun _ db pool ->
                ignore (Db.exec pool (T.clean ())) ;
                let from = List.length db.log in
                let r =
                  outcome text (fun () ->
                      (* Only row 1 is cut from outside. *)
                      if expected = "cancelled" then
                        match
                          Fiber.with_timeout Mono (Span.ms 200) (fun () ->
                              Db.transaction ~timeout pool (f db) )
                        with
                        | r ->
                            r
                        | exception Fiber.Timeout ->
                            raise (Failure "cancelled")
                      else Db.transaction ~timeout pool (f db) )
                in
                let r = if r = "Failure cancelled" then "cancelled" else r in
                (r, log ~from db) )
          in
          equal string expected r ;
          equal (list string) expected_cmds cmds ;
          (* Each ROLLBACK of the fake server succeeds: one event "done". *)
          equal ~msg:"the events of the end" (list string)
            (List.filter_map
               (function
                 | "ROLLBACK" -> Some "query \"ROLLBACK\" done 0" | _ -> None )
               expected_cmds )
            (end_events events) )
    ; cases "the replies to COMMIT (3.8)"
        ~id:(fun (n, _, _, _) -> n)
        [ ("an OK", None, "Ok", 1)
        ; ( "an OK with IN_TRANS: the checkin aborts"
          , Some D.Commit_in_trans
          , "Ok"
          , 2 )
        ; ("1213", Some (D.Commit_err 1213), "Retryable rolled back", 1)
        ; ("1205", Some (D.Commit_err 1205), "Retryable rolled back", 1)
        ; ("1180", Some (D.Commit_err 1180), "Retryable rolled back", 1)
        ; ("3101", Some (D.Commit_err 3101), "Retryable rolled back", 1)
        ; ("1317", Some (D.Commit_err 1317), "Outcome_unknown commit", 2)
        ; ( "1053 then the end"
          , Some (D.Commit_err_close 1053)
          , "Outcome_unknown commit"
          , 2 )
        ; ( "the end with no reply"
          , Some D.Commit_close
          , "Outcome_unknown commit"
          , 2 )
        ; ( "no reply in 5 s"
          , Some (D.Commit_delay 5500)
          , "Outcome_unknown commit"
          , 2 )
        ; ("a slow reply before 5 s", Some (D.Commit_delay 4000), "Ok", 1)
        ; ("4031 with the id 0", Some D.Commit_seq0, "Connection_lost", 2)
        ; ( "another error: ROLLBACK"
          , Some (D.Commit_err 1146)
          , "Driver 1146 28000"
          , 1 ) ]
        (fun (_, fault, expected, expected_connects) ->
          let (r, cmds, connects), events =
            T.with_pool ~size:1 (fun _ db pool ->
                ignore (Db.exec pool (T.clean ())) ;
                let from = List.length db.log in
                db.faults.commit <- fault ;
                (* The transaction deadline is near: COMMIT still has 5 s. *)
                let r =
                  text
                    (Db.transaction ~timeout:(Span.ms 200) pool (fun tx ->
                         ignore (Tx.exec tx (x "UPDATE a")) ) )
                in
                db.faults.commit <- None ;
                let cmds = log ~from db in
                ignore (Db.exec pool (T.clean ())) ;
                (r, cmds, db.connects) )
          in
          equal string expected r ;
          (* The next exec opens a new connection when the checkin aborted. *)
          equal int expected_connects connects ;
          if fault = Some (D.Commit_err 1146) then
            equal (list string)
              ["START TRANSACTION"; "UPDATE a"; "COMMIT"; "ROLLBACK"]
              cmds ;
          (* The event of COMMIT has its result. Only "another error" has a
             ROLLBACK. *)
          equal ~msg:"the events of the end" (list string)
            ( ( if expected = "Ok" then "query \"COMMIT\" done 0"
                else "query \"COMMIT\" failed " ^ expected )
            ::
            ( if fault = Some (D.Commit_err 1146) then
                ["query \"ROLLBACK\" done 0"]
              else [] ) )
            (end_events events) )
    ; test "a cancel during COMMIT: the shield keeps its result" (fun () ->
          let (r, cmds), _ =
            T.with_pool ~size:1 (fun _ db pool ->
                ignore (Db.exec pool (T.clean ())) ;
                let from = List.length db.log in
                db.faults.commit <- Some (D.Commit_delay 100) ;
                let r =
                  match
                    Fiber.with_timeout Mono (Span.ms 50) (fun () ->
                        Db.transaction pool (fun tx ->
                            ignore (Tx.exec tx (x "UPDATE a")) ) )
                  with
                  | r ->
                      text r
                  | exception Fiber.Timeout ->
                      "Fiber.Timeout"
                in
                (r, log ~from db) )
          in
          (* The cancel comes in the wait of COMMIT: the commit happens, and its
             result comes back. *)
          equal string "Ok" r ;
          equal (list string) ["START TRANSACTION"; "UPDATE a"; "COMMIT"] cmds )
    ; cases "savepoints (3.9)"
        ~id:(fun (n, _, _, _) -> n)
        [ ( "a value: RELEASE"
          , (fun _ tx ->
              ignore (Tx.exec_exn tx (x "UPDATE a")) ;
              3 )
          , "Ok 3 / Ok"
          , [ "START TRANSACTION"
            ; "SAVEPOINT cistern_sp_1"
            ; "UPDATE a"
            ; "RELEASE SAVEPOINT cistern_sp_1"
            ; "COMMIT" ] )
        ; ( "a Cistern_error: ROLLBACK TO, the Tx stays"
          , (fun _ _ -> raise (E.Cistern_error E.Not_found))
          , "Not_found / Ok"
          , [ "START TRANSACTION"
            ; "SAVEPOINT cistern_sp_1"
            ; "ROLLBACK TO SAVEPOINT cistern_sp_1"
            ; "COMMIT" ] )
        ; ( "an exception: ROLLBACK TO, then it goes up"
          , (fun _ _ -> failwith "bug")
          , "Failure bug / Ok"
          , [ "START TRANSACTION"
            ; "SAVEPOINT cistern_sp_1"
            ; "ROLLBACK TO SAVEPOINT cistern_sp_1"
            ; "COMMIT" ] )
        ; ( "a handled error: RELEASE"
          , (fun _ tx ->
              ignore (Tx.exec tx (x "FAIL 1062")) ;
              4 )
          , "Ok 4 / Ok"
          , [ "START TRANSACTION"
            ; "SAVEPOINT cistern_sp_1"
            ; "FAIL 1062"
            ; "RELEASE SAVEPOINT cistern_sp_1"
            ; "COMMIT" ] )
        ; ( "a deadlock in the scope: no command, the first error"
          , (fun _ tx ->
              ignore (Tx.exec tx (x "DEADLOCK")) ;
              5 )
          , "Retryable deadlock / Retryable deadlock"
          , [ "START TRANSACTION"
            ; "SAVEPOINT cistern_sp_1"
            ; "DEADLOCK"
            ; "ROLLBACK" ] )
        ; ( "a failed DDL, then a Cistern_error: Implicit_commit"
          , (fun _ tx ->
              ignore (Tx.exec tx (x "DDLFAIL")) ;
              raise (E.Cistern_error E.Not_found) )
          , "Implicit_commit / Implicit_commit"
          , [ "START TRANSACTION"
            ; "SAVEPOINT cistern_sp_1"
            ; "DDLFAIL"
            ; "ping"
            ; "ROLLBACK" ] )
        ; ( "a deadlock, then a Cistern_error: the Cistern_error"
          , (fun _ tx ->
              ignore (Tx.exec tx (x "DEADLOCK")) ;
              raise (E.Cistern_error E.Not_found) )
          , "Not_found / Retryable deadlock"
          , [ "START TRANSACTION"
            ; "SAVEPOINT cistern_sp_1"
            ; "DEADLOCK"
            ; "ROLLBACK" ] )
        ; ( "1305 at RELEASE fails the Tx"
          , (fun _ tx ->
              ignore (Tx.exec tx (x "FORGET")) ;
              6 )
          , "Driver 1305 28000 / Driver 1305 28000"
          , [ "START TRANSACTION"
            ; "SAVEPOINT cistern_sp_1"
            ; "FORGET"
            ; "RELEASE SAVEPOINT cistern_sp_1"
            ; "ROLLBACK" ] )
        ; ( "a cancel of the scope: no command, Cancelled"
          , (fun _ _ ->
              Time.sleep Mono (Span.s 1) ;
              7 )
          , "cancelled / Ok"
          , ["START TRANSACTION"; "SAVEPOINT cistern_sp_1"; "COMMIT"] ) ]
        (fun (name, g, expected, expected_cmds) ->
          let cut = String.ends_with ~suffix:"Cancelled" name in
          let (r, cmds), _ =
            T.with_pool ~size:1 (fun _ db pool ->
                ignore (Db.exec pool (T.clean ())) ;
                let from = List.length db.log in
                let inner = ref "" in
                let r =
                  Db.transaction pool (fun tx ->
                      inner :=
                        outcome
                          (function
                            | Ok v ->
                                "Ok " ^ string_of_int v
                            | Error e ->
                                T.error_text e )
                          (fun () ->
                            if cut then
                              match
                                Fiber.with_timeout Mono (Span.ms 10) (fun () ->
                                    Tx.savepoint tx (g db) )
                              with
                              | r ->
                                  r
                              | exception Fiber.Timeout ->
                                  raise (Failure "cancelled")
                            else Tx.savepoint tx (g db) ) ;
                      if !inner = "Failure cancelled" then inner := "cancelled" )
                in
                (!inner ^ " / " ^ text r, log ~from db) )
          in
          equal string expected r ;
          equal (list string) expected_cmds cmds )
    ; test "the default timeout is 30 s" (fun () ->
          let r, _ =
            T.with_pool ~size:1 (fun _ _ pool ->
                let t0 = Time.now Mono in
                let r =
                  text
                    (Db.transaction pool (fun _ -> Time.sleep Mono (Span.s 40)))
                in
                Printf.sprintf "%s %.3f" r (T.seconds t0) )
          in
          equal string "Timeout 30.000" r )
    ; test "a cancel in the ping after an error (D7)" (fun () ->
          let r, inner, cmds, _, events =
            run_tx (fun db note tx ->
                (* The latency comes before each reply: the ping waits from 100
                   ms to 200 ms, and the cut comes at 150 ms. *)
                db.faults.latency_ms <- 100 ;
                ( match
                    Fiber.with_timeout Mono (Span.ms 150) (fun () ->
                        Tx.exec tx (x "FAIL 1146") )
                  with
                | r ->
                    step note r
                | exception Fiber.Timeout ->
                    note "cut" ) ;
                db.faults.latency_ms <- 0 )
          in
          equal string "cut" inner ;
          equal string "Connection_lost" r ;
          equal (list string) ["START TRANSACTION"; "FAIL 1146"] cmds ;
          is_true (List.mem "query \"FAIL 1146\" cancelled sent=true" events) ;
          (* The Tx failed at the cancel: no COMMIT. *)
          is_false
            (List.exists (String.starts_with ~prefix:"query \"COMMIT\"") events) )
    ; test "a savepoint after the margin of the transaction deadline" (fun () ->
          let r, inner, cmds, _, events =
            run_tx ~timeout:(Span.ms 300) (fun _ note tx ->
                Time.sleep Mono (Span.ms 260) ;
                note (text (Tx.savepoint tx (fun _ -> ()))) )
          in
          equal string "Timeout" inner ;
          equal string "Timeout" r ;
          equal (list string) ["START TRANSACTION"; "ROLLBACK"] cmds ;
          is_true
            (List.mem "query \"SAVEPOINT cistern_sp_1\" failed Timeout" events) )
    ; test "a savepoint command sets its own flow deadline" (fun () ->
          let r, inner, _, _, _ =
            run_tx (fun db note tx ->
                (* The deadline of this statement is 100 ms: it stays on the
                   flow after the statement. *)
                step note (Tx.exec ~timeout:(Span.ms 100) tx (x "UPDATE a")) ;
                Time.sleep Mono (Span.ms 150) ;
                db.faults.latency_ms <- 300 ;
                note (text (Tx.savepoint tx (fun _ -> ()))) ;
                db.faults.latency_ms <- 0 )
          in
          equal string "Ok / Ok" inner ;
          equal string "Ok" r )
    ; test "the events of a savepoint, of COMMIT and of ROLLBACK" (fun () ->
          let _, _, _, _, events =
            run_tx (fun _ _ tx ->
                ignore (Tx.savepoint tx (fun _ -> ())) ;
                ignore
                  (Tx.savepoint tx (fun _ ->
                       raise (E.Cistern_error E.Not_found) ) ) )
          in
          equal (list string)
            [ "query \"START TRANSACTION\" done 0"
            ; "query \"SAVEPOINT cistern_sp_1\" done 0"
            ; "query \"RELEASE SAVEPOINT cistern_sp_1\" done 0"
            ; "query \"SAVEPOINT cistern_sp_2\" done 0"
            ; "query \"ROLLBACK TO SAVEPOINT cistern_sp_2\" done 0"
            ; "query \"COMMIT\" done 0" ]
            events ;
          let _, _, _, _, events =
            run_tx (fun _ _ tx -> ignore (Tx.exec tx (x "DEADLOCK")))
          in
          equal (list string)
            [ "query \"START TRANSACTION\" done 0"
            ; "query \"DEADLOCK\" failed Retryable deadlock"
            ; "query \"ROLLBACK\" done 0" ]
            events )
    ; test "an error of SAVEPOINT: a ping decides" (fun () ->
          let r, inner, cmds, _, _ =
            run_tx (fun db note tx ->
                db.faults.savepoint_err <- Some 1146 ;
                note (text (Tx.savepoint tx (fun _ -> ()))) ;
                db.faults.savepoint_err <- None )
          in
          equal string "Driver 1146 28000" inner ;
          equal string "Ok" r ;
          equal (list string)
            ["START TRANSACTION"; "SAVEPOINT cistern_sp_1"; "ping"; "COMMIT"]
            cmds )
    ; test "a cancel in a savepoint command" (fun () ->
          let r, inner, cmds, _, events =
            run_tx (fun db note tx ->
                db.faults.latency_ms <- 100 ;
                ( match
                    Fiber.with_timeout Mono (Span.ms 10) (fun () ->
                        Tx.savepoint tx (fun _ -> ()) )
                  with
                | r ->
                    note (text r)
                | exception Fiber.Timeout ->
                    note "cut" ) ;
                db.faults.latency_ms <- 0 )
          in
          equal string "cut" inner ;
          equal string "Connection_lost" r ;
          equal (list string) ["START TRANSACTION"] cmds ;
          is_true
            (List.mem "query \"SAVEPOINT cistern_sp_1\" cancelled sent=true"
               events ) ;
          is_false
            (List.exists (String.starts_with ~prefix:"query \"COMMIT\"") events) )
    ; test "a value with no literal in a Tx: statement-only" (fun () ->
          let r, inner, cmds, _, events =
            run_tx (fun _ note tx ->
                step note (Tx.exec tx (T.bad_literal ())) ;
                step note (Tx.exec tx (x "UPDATE a")) )
          in
          equal string "Invalid_value / Ok" inner ;
          equal string "Ok" r ;
          equal (list string) ["START TRANSACTION"; "UPDATE a"; "COMMIT"] cmds ;
          is_true (List.mem "query \"SELECT ? \" failed Invalid_value" events) )
    ; test "a statement timeout equal to the transaction deadline" (fun () ->
          let r, inner, _, _, _ =
            run_tx ~timeout:(Span.s 1) (fun _ note tx ->
                Time.sleep Mono (Span.ms 960) ;
                (* The same deadline: the margin failure fails the Tx. *)
                step note (Tx.exec ~timeout:(Span.ms 40) tx (x "UPDATE a")) )
          in
          equal string "Timeout" inner ;
          equal string "Timeout" r )
    ; test "the dirty mark after a Tx" (fun () ->
          let r, _ =
            T.with_pool ~size:1 (fun _ db pool ->
                ignore (Db.exec pool (T.clean ())) ;
                ignore (Db.transaction pool (fun tx -> Tx.exec tx (T.clean ()))) ;
                ignore (Db.exec pool (T.clean ())) ;
                let a = db.resets in
                ignore
                  (Db.transaction pool (fun tx -> Tx.exec tx (x "UPDATE a"))) ;
                ignore (Db.exec pool (T.clean ())) ;
                Printf.sprintf "%d %d" a db.resets )
          in
          (* A Tx with only a clean statement keeps the entry clean. A raw
             statement makes it dirty: one reset at the next checkout. *)
          equal string "0 1" r )
    ; test "a clean statement keeps the rule D5 for DML" (fun () ->
          let r, _, _, _, _ =
            run_tx (fun db _ tx ->
                ignore (Tx.exec tx (T.clean ())) ;
                db.faults.latency_ms <- 1000 ;
                ignore (Tx.exec ~timeout:(Span.ms 200) tx (x "UPDATE a")) ;
                db.faults.latency_ms <- 0 )
          in
          equal string "Timeout" r )
    ; test "a savepoint from another fiber, and after the end" (fun () ->
          let r, _ =
            T.with_pool ~size:2 (fun _ _ pool ->
                let saved = ref None and child = ref "" in
                ignore
                  (Db.transaction pool (fun tx ->
                       saved := Some tx ;
                       Fiber.scope (fun s ->
                           Fiber.fork s (fun () ->
                               match Tx.savepoint tx (fun _ -> ()) with
                               | _ ->
                                   child := "no error"
                               | exception Invalid_argument m ->
                                   child := m ) ) ) ) ;
                let after =
                  match Tx.savepoint (Option.get !saved) (fun _ -> ()) with
                  | _ ->
                      "no error"
                  | exception Invalid_argument m ->
                      m
                in
                !child ^ " / " ^ after )
          in
          equal string
            "Tx.savepoint from another fiber than the fiber of the Tx / \
             Tx.savepoint after the end of the transaction"
            r )
    ; test "a failed ROLLBACK TO fails the Tx" (fun () ->
          let r, inner, cmds, _, _ =
            run_tx (fun _ note tx ->
                note
                  (text
                     (Tx.savepoint tx (fun tx ->
                          ignore (Tx.exec tx (x "FORGET")) ;
                          raise (E.Cistern_error E.Not_found) ) ) ) )
          in
          equal string "Not_found" inner ;
          equal string "Driver 1305 28000" r ;
          equal (list string)
            [ "START TRANSACTION"
            ; "SAVEPOINT cistern_sp_1"
            ; "FORGET"
            ; "ROLLBACK TO SAVEPOINT cistern_sp_1"
            ; "ROLLBACK" ]
            cmds )
    ; test "COMMIT waits at most 30 s" (fun () ->
          let r, _ =
            T.with_pool ~size:1 (fun _ db pool ->
                db.faults.commit <- Some (D.Commit_delay 30500) ;
                let r =
                  text
                    (Db.transaction ~timeout:(Span.s 60) pool (fun tx ->
                         ignore (Tx.exec tx (x "UPDATE a")) ) )
                in
                db.faults.commit <- None ;
                r )
          in
          equal string "Outcome_unknown commit" r )
    ; cases "a failed ROLLBACK aborts the connection, and keeps the result"
        ~id:(fun (n, _, _) -> n)
        [ ("after a deadlock", "DEADLOCK", "Retryable deadlock")
          (* The OK of CREATE has IN_TRANS clear: only the rule of the failed
             ROLLBACK aborts the connection, not the flag at the checkin. *)
        ; ("after an implicit commit", "CREATE", "Implicit_commit") ]
        (fun (_, word, expected) ->
          let (r, cmds, connects), events =
            T.with_pool ~size:1 (fun _ db pool ->
                ignore (Db.exec pool (T.clean ())) ;
                let from = List.length db.log in
                let r =
                  text
                    (Db.transaction pool (fun tx ->
                         db.faults.rollback_err <- Some 1146 ;
                         ignore (Tx.exec tx (x word)) ) )
                in
                db.faults.rollback_err <- None ;
                let cmds = log ~from db in
                (* The connection was aborted: the next exec opens another. *)
                ignore (Db.exec pool (T.clean ())) ;
                (r, cmds, db.connects) )
          in
          equal string expected r ;
          equal (list string) ["START TRANSACTION"; word; "ROLLBACK"] cmds ;
          equal ~msg:"the next exec opens a connection" int 2 connects ;
          equal (list string)
            ["query \"ROLLBACK\" failed Driver 1146 28000"]
            (end_events events) )
    ; test "another error of COMMIT: its event, then ROLLBACK" (fun () ->
          let _, _, _, _, events =
            run_tx (fun db _ tx ->
                db.faults.commit <- Some (D.Commit_err 1146) ;
                ignore (Tx.exec tx (x "UPDATE a")) )
          in
          equal (list string)
            [ "query \"START TRANSACTION\" done 0"
            ; "query \"UPDATE a\" done 1"
            ; "query \"COMMIT\" failed Driver 1146 28000"
            ; "query \"ROLLBACK\" done 0" ]
            events )
    ; test "START TRANSACTION with IN_TRANS clear: Connection_lost" (fun () ->
          let r, _ =
            T.with_pool ~size:1
              ~setup:(fun db -> db.faults.begin_no_trans <- true)
              (fun _ db pool ->
                let r = text (Db.transaction pool (fun _ -> ())) in
                db.faults.begin_no_trans <- false ;
                ignore (Db.exec pool (q "SELECT 1")) ;
                Printf.sprintf "%s %d" r db.connects )
          in
          (* The connection was aborted: the next exec opens another. *)
          equal string "Connection_lost 2" r )
    ; test
        "the steps before the function: the margin, a reset past the deadline"
        (fun () ->
          let (a, b), events =
            T.with_pool ~size:1 (fun _ db pool ->
                let a =
                  text (Db.transaction ~timeout:(Span.ms 40) pool ignore)
                in
                ignore (Db.exec pool (q "SELECT 1")) ;
                db.faults.reset_ms <- 1000 ;
                let b =
                  text (Db.transaction ~timeout:(Span.ms 200) pool ignore)
                in
                db.faults.reset_ms <- 0 ;
                (a, b) )
          in
          equal string "Timeout" a ;
          equal string "Timeout" b ;
          equal int 2
            (List.length
               (List.filter
                  (( = ) "query \"START TRANSACTION\" failed Timeout")
                  events ) ) )
    ; test "an exception of the finish of a statement: the event first (Q8)"
        (fun () ->
          let r, inner, cmds, _, events =
            run_tx (fun _ note tx ->
                ( match
                    Tx.exec tx
                      (Cistern.Stmt.map
                         (fun _ -> failwith "finish")
                         (x "UPDATE a") )
                  with
                | _ ->
                    note "no exception"
                | exception Failure m ->
                    note m ) ;
                step note (Tx.exec tx (x "UPDATE b")) )
          in
          (* The exception goes up, and the Tx stays usable. *)
          equal string "finish / Ok" inner ;
          equal string "Ok" r ;
          equal (list string)
            ["START TRANSACTION"; "UPDATE a"; "UPDATE b"; "COMMIT"]
            cmds ;
          equal (list string)
            [ "query \"START TRANSACTION\" done 0"
            ; "query \"UPDATE a\" done 1"
            ; "query \"UPDATE b\" done 1"
            ; "query \"COMMIT\" done 0" ]
            events )
    ; test "a ping that fails after an error: the Tx fails, no COMMIT (D6)"
        (fun () ->
          let (r, inner, cmds, connects), events =
            T.with_pool ~size:1 (fun _ db pool ->
                ignore (Db.exec pool (T.clean ())) ;
                let from = List.length db.log in
                let inner = ref "" in
                let r =
                  text
                    (Db.transaction pool (fun tx ->
                         db.faults.ping_close <- true ;
                         inner := text (Tx.exec tx (x "FAIL 1146")) ;
                         db.faults.ping_close <- false ;
                         inner :=
                           !inner ^ " / " ^ text (Tx.exec tx (x "UPDATE a")) )
                    )
                in
                let cmds = log ~from db in
                ignore (Db.exec pool (T.clean ())) ;
                (r, !inner, cmds, db.connects) )
          in
          equal string "Driver 1146 28000 / Driver 1146 28000" inner ;
          equal string "Driver 1146 28000" r ;
          (* The connection broke at the ping: no COMMIT and no ROLLBACK. *)
          equal (list string) ["START TRANSACTION"; "FAIL 1146"; "ping"] cmds ;
          equal int 2 connects ;
          equal (list string) [] (end_events events) )
    ; test "a cancel after the first command of a statement: sent=true"
        (fun () ->
          let r, inner, cmds, _, events =
            run_tx (fun _ note tx ->
                let run ctx =
                  ignore (Cistern.Private.Exec.exec ctx (Raw.sql "SELECT 1")) ;
                  (try Time.sleep Mono (Span.s 1) with Fiber.Cancelled _ -> ()) ;
                  (* The fiber is cancelled: this command sends nothing. *)
                  ignore (Cistern.Private.Exec.exec ctx (Raw.sql "SELECT 2")) ;
                  Ok ()
                in
                match
                  Fiber.with_timeout Mono (Span.ms 10) (fun () ->
                      Tx.exec tx (T.custom run) )
                with
                | _ ->
                    note "no timeout"
                | exception Fiber.Timeout ->
                    note "cut" )
          in
          equal string "cut" inner ;
          (* CUSTOM is not DML, and its first command went: the outcome is not
             known. *)
          equal string "Outcome_unknown statement" r ;
          equal (list string) ["START TRANSACTION"; "SELECT 1"] cmds ;
          is_true (List.mem "query \"CUSTOM\" cancelled sent=true" events) )
    ; test "an ERR of START TRANSACTION: its event, the connection stays"
        (fun () ->
          let r, events =
            T.with_pool ~size:1
              ~setup:(fun db -> db.faults.begin_err <- Some 1146)
              (fun _ db pool ->
                let r = text (Db.transaction pool (fun _ -> ())) in
                db.faults.begin_err <- None ;
                ignore (Db.exec pool (q "SELECT 1")) ;
                Printf.sprintf "%s %d" r db.connects )
          in
          equal string "Driver 1146 28000 1" r ;
          is_true
            (List.mem "query \"START TRANSACTION\" failed Driver 1146 28000"
               events ) )
    ; test "the callback gets the time left after a checkout that waits"
        (fun () ->
          let r, _ =
            T.with_pool ~size:1 (fun _ _ pool ->
                Fiber.scope (fun s ->
                    (* The only connection is busy for 200 ms. *)
                    Fiber.fork s (fun () ->
                        ignore (Db.exec pool (q "SLEEP 200")) ) ;
                    Time.sleep Mono (Span.ms 1) ;
                    let t0 = Time.now Mono in
                    let woke = ref false and at = ref 0. in
                    let r =
                      text
                        (Db.transaction ~timeout:(Span.ms 500) pool (fun _ ->
                             Fun.protect
                               ~finally:(fun () -> at := T.seconds t0)
                               (fun () ->
                                 Time.sleep Mono (Span.ms 400) ;
                                 woke := true ) ) )
                    in
                    Printf.sprintf "%s %b %.3f" r !woke !at ) )
          in
          (* The timer of the callback fires at the deadline, 500 ms after the
             start, not 500 ms after the start of the callback. *)
          equal string "Timeout false 0.500" r )
    ; test "a cancel at the end aborts the connection, also with IN_TRANS clear"
        (fun () ->
          let r, _ =
            T.with_pool ~size:1 (fun _ db pool ->
                ignore (Db.exec pool (T.clean ())) ;
                let r =
                  match
                    Fiber.with_timeout Mono (Span.ms 200) (fun () ->
                        Db.transaction pool (fun tx ->
                            (* The OK of CREATE has IN_TRANS clear. *)
                            ignore (Tx.exec tx (x "CREATE")) ;
                            Time.sleep Mono (Span.s 1) ) )
                  with
                  | _ ->
                      "no cancel"
                  | exception Fiber.Timeout ->
                      "cancelled"
                in
                ignore (Db.exec pool (T.clean ())) ;
                Printf.sprintf "%s %d" r db.connects )
          in
          equal string "cancelled 2" r )
    ; test "the fake server: the savepoints after RELEASE and after a deadlock"
        (fun () ->
          let r, _ =
            T.with_pool ~size:1 (fun _ _ pool ->
                let out = ref [] in
                let run ctx =
                  List.iter
                    (fun s ->
                      out :=
                        ( s ^ ": "
                        ^ text (Cistern.Private.Exec.exec ctx (Raw.sql s)) )
                        :: !out )
                    [ "START TRANSACTION"
                    ; "SAVEPOINT a"
                    ; "SAVEPOINT b"
                    ; "RELEASE SAVEPOINT a"
                    ; "ROLLBACK TO SAVEPOINT b"
                    ; "ROLLBACK TO SAVEPOINT a"
                    ; "SAVEPOINT c"
                    ; "DEADLOCK"
                    ; "ROLLBACK TO SAVEPOINT c" ] ;
                  Ok ()
                in
                ignore (Db.exec pool (T.custom run)) ;
                List.rev !out )
          in
          equal (list string)
            [ "START TRANSACTION: Ok"
            ; "SAVEPOINT a: Ok"
            ; "SAVEPOINT b: Ok"
            ; "RELEASE SAVEPOINT a: Ok"
            ; "ROLLBACK TO SAVEPOINT b: Driver 1305 28000"
            ; "ROLLBACK TO SAVEPOINT a: Driver 1305 28000"
            ; "SAVEPOINT c: Ok"
            ; "DEADLOCK: Retryable deadlock"
            ; "ROLLBACK TO SAVEPOINT c: Driver 1305 28000" ]
            r )
    ; test "a cancel by the timer of the Tx in a statement: Timeout (D7)"
        (fun () ->
          let r, inner, _, _, events =
            run_tx ~timeout:(Span.ms 500) (fun db note tx ->
                db.faults.latency_ms <- 1000 ;
                ( match Tx.exec tx (x "UPDATE a") with
                | r ->
                    step note r
                | exception Fiber.Cancelled _ ->
                    note "cancelled" ) ;
                db.faults.latency_ms <- 0 )
          in
          (* In the simulator, the timer of the Tx fires before the flow
             deadline of the statement, at the same instant. *)
          equal string "cancelled" inner ;
          equal string "Timeout" r ;
          is_true (List.mem "query \"UPDATE a\" cancelled sent=true" events) )
    ; test "the COMMIT and ROLLBACK events come after the checkin" (fun () ->
          let seen = ref [] and pr = ref None in
          D.run (fun _ db ->
              Db.with_db ~pool_size:1
                ~on_query:(function
                  | Cistern_core.Event.Query
                      {sql= ("COMMIT" | "ROLLBACK") as sql; _} ->
                      let _, in_use, _, _, _ =
                        Pool.Private.state (Option.get !pr)
                      in
                      seen := Printf.sprintf "%s %d" sql in_use :: !seen
                  | _ ->
                      () )
                (D.config ())
                (fun pool ->
                  pr := Some (Db.Private.pool pool) ;
                  ignore
                    (Db.transaction pool (fun tx -> Tx.exec tx (x "UPDATE a"))) ;
                  ignore
                    (Db.transaction pool (fun _ ->
                         raise (E.Cistern_error E.Not_found) ) ) ;
                  (* Another error of COMMIT: the COMMIT event and the ROLLBACK
                     event. *)
                  db.D.faults.commit <- Some (D.Commit_err 1146) ;
                  ignore
                    (Db.transaction pool (fun tx -> Tx.exec tx (x "UPDATE a"))) ;
                  db.D.faults.commit <- None ) ) ;
          (* A hook never sees the slot of its own transaction in use. *)
          equal (list string)
            ["COMMIT 0"; "ROLLBACK 0"; "COMMIT 0"; "ROLLBACK 0"]
            (List.rev !seen) )
    ; test "a cancel during a COMMIT that rolls back: its result and its event"
        (fun () ->
          let (r, cmds, connects), events =
            T.with_pool ~size:1 (fun _ db pool ->
                ignore (Db.exec pool (T.clean ())) ;
                let from = List.length db.log in
                db.faults.commit <- Some (D.Commit_delay_err (100, 1213)) ;
                let r =
                  match
                    Fiber.with_timeout Mono (Span.ms 50) (fun () ->
                        Db.transaction pool (fun tx ->
                            ignore (Tx.exec tx (x "UPDATE a")) ) )
                  with
                  | r ->
                      text r
                  | exception Fiber.Timeout ->
                      "Fiber.Timeout"
                in
                db.faults.commit <- None ;
                let cmds = log ~from db in
                ignore (Db.exec pool (T.clean ())) ;
                (r, cmds, db.connects) )
          in
          (* The cancel comes in the wait of COMMIT. The ping after the ERR runs
             in the shield too, and the connection stays. *)
          equal string "Retryable rolled back" r ;
          equal (list string)
            ["START TRANSACTION"; "UPDATE a"; "COMMIT"; "ping"]
            cmds ;
          equal int 1 connects ;
          equal (list string)
            ["query \"COMMIT\" failed Retryable rolled back"]
            (end_events events) )
    ; test "Db.transaction in a cancelled fiber on a closed pool: the misuse"
        (fun () ->
          let r = ref "" in
          D.run (fun _ _ ->
              let last = ref None in
              Db.with_db ~pool_size:1 (D.config ()) (fun pool ->
                  last := Some pool ) ;
              r :=
                match
                  Fiber.with_timeout Mono (Span.ms 1) (fun () ->
                      ( try Time.sleep Mono (Span.s 1)
                        with Fiber.Cancelled _ -> () ) ;
                      Db.transaction (Option.get !last) ignore )
                with
                | _ ->
                    "no error"
                | exception Invalid_argument m ->
                    m
                | exception Fiber.Timeout ->
                    "cancelled" ) ;
          (* The closed pool comes first, as in Db.exec. *)
          equal string "Db.t used after with_db" !r ) ]

(* The storm of Db.transaction and Db.exec (the plan of phase 1.3, 4.2, and the
   assertions of review C2). [fibers] fibers on 4 virtual domains and a pool of
   8, in three waves. Half of them run Db.exec, half run Db.transaction with 1
   to 3 statements, a wait between them, and a deadlock of the fake server in
   about 1 statement of 10. One fiber of 4 gets a cancel. Each statement has a
   text of its own, so that the events can be matched with the commands of the
   fake server. *)
let tx_permitted =
  [ "ok"
  ; "cancelled"
  ; "Timeout"
  ; "Pool_exhausted"
  ; "Retryable deadlock"
  ; "Connection_lost"
  ; "Outcome_unknown statement"
  ; "Retryable rolled back"
  ; "Outcome_unknown commit" ]

(* The faults of COMMIT in the storm: most of the time none. A fault stays for 5
   ms. *)
let commit_faults =
  [| None
   ; None
   ; None
   ; None
   ; Some (D.Commit_err 1213)
   ; Some (D.Commit_err 1205)
   ; Some (D.Commit_delay_err (30, 1213))
   ; Some (D.Commit_delay 30) |]

let tx_storm ~fibers ~seed ~seam =
  Pool.Private.after_op :=
    if seam then fun () -> Fiber.shield (fun () -> Time.sleep Mono (Span.ms 1))
    else ignore ;
  Fun.protect
    ~finally:(fun () -> Pool.Private.after_op := ignore)
    (fun () ->
      let (outcomes, commits, set, cmds), events =
        T.with_pool ~size:8 ~seed ~domains:4 ~checkout_timeout:(Span.ms 20)
          (fun _ db pool ->
            let outcomes = ref [] and commits = ref 0 in
            let ended = Atomic.make 0 in
            Fiber.scope (fun s ->
                (* The fiber of the faults of COMMIT, until the last fiber of
                   the storm ends. *)
                Fiber.fork s (fun () ->
                    let rs = Random.State.make [|seed; 0|] in
                    while Atomic.get ended < fibers do
                      Time.sleep Mono (Span.ms (10 + Random.State.int rs 20)) ;
                      db.faults.commit <-
                        commit_faults.(Random.State.int rs
                                         (Array.length commit_faults)) ;
                      Time.sleep Mono (Span.ms 5) ;
                      db.faults.commit <- None
                    done ) ;
                for i = 1 to fibers do
                  Fiber.fork s (fun () ->
                      let rs = Random.State.make [|seed; i|] in
                      let wave = [|0; 60; 200|].(Random.State.int rs 3) in
                      Time.sleep Mono (Span.ms (wave + Random.State.int rs 30)) ;
                      let run () =
                        if Random.State.bool rs then
                          Result.map ignore
                            (Db.exec pool (x (Printf.sprintf "UPDATE e%d" i)))
                        else
                          let n = 1 + Random.State.int rs 3 in
                          let timeout =
                            Span.ms (60 + Random.State.int rs 100)
                          in
                          let r =
                            Db.transaction ~timeout pool (fun tx ->
                                for k = 1 to n do
                                  let word =
                                    if Random.State.int rs 10 = 0 then
                                      "DEADLOCK"
                                    else "UPDATE"
                                  in
                                  let statement () =
                                    ignore
                                      (Tx.exec tx
                                         (x
                                            (Printf.sprintf "%s t%d_%d" word i k) ) )
                                  in
                                  (* One statement of 3 in a savepoint, and half
                                     of these scopes end with a Cistern_error
                                     (ROLLBACK TO). *)
                                  ( match Random.State.int rs 6 with
                                  | 0 ->
                                      ignore
                                        (Tx.savepoint tx (fun _ -> statement ()))
                                  | 1 ->
                                      ignore
                                        (Tx.savepoint tx (fun _ ->
                                             statement () ;
                                             raise (E.Cistern_error E.Not_found) )
                                        )
                                  | _ ->
                                      statement () ) ;
                                  Time.sleep Mono
                                    (Span.ms (Random.State.int rs 6))
                                done )
                          in
                          if Result.is_ok r then incr commits ;
                          r
                      in
                      let o =
                        match
                          if Random.State.int rs 4 = 0 then
                            Fiber.with_timeout Mono
                              (Span.ms (1 + Random.State.int rs 60))
                              run
                          else run ()
                        with
                        | Ok () ->
                            "ok"
                        | Error e ->
                            T.error_text e
                        | exception Fiber.Timeout ->
                            "cancelled"
                        | exception Invalid_argument m ->
                            "Invalid_argument " ^ m
                        | exception e ->
                            "exception " ^ Printexc.to_string e
                      in
                      outcomes := o :: !outcomes ;
                      Atomic.incr ended )
                done ) ;
            Pool.Private.after_op := ignore ;
            Time.sleep Mono (Span.ms 100) ;
            ( !outcomes
            , !commits
            , Pool.tx_fibers (Db.Private.pool pool)
            , List.rev db.log ) )
      in
      (outcomes, commits, set, cmds, T.queries events) )

let count p l = List.length (List.filter p l)

let check_tx_storm ~fibers (outcomes, commits, set, cmds, events) =
  equal ~msg:"one outcome for each fiber" int fibers (List.length outcomes) ;
  List.iter
    (fun o ->
      is_true ~msg:("a permitted outcome: " ^ o) (List.mem o tx_permitted) )
    outcomes ;
  equal ~msg:"no fiber in the set of Tx fibers" int 0 set ;
  is_true
    ~msg:(Printf.sprintf "at least 50 commits (%d)" commits)
    (commits >= 50) ;
  (* One event for each command that the fake server got: each statement has a
     text of its own, and COMMIT and ROLLBACK are counted. *)
  let unique s =
    String.starts_with ~prefix:"UPDATE t" s
    || String.starts_with ~prefix:"UPDATE e" s
    || String.starts_with ~prefix:"DEADLOCK t" s
  in
  let event_sql e =
    (* "query \"<sql>\" <outcome>" *)
    match String.index_from_opt e 7 '"' with
    | Some j ->
        String.sub e 7 (j - 7)
    | None ->
        e
  in
  let sqls = List.map event_sql events in
  List.iter
    (fun c ->
      if unique c then
        equal
          ~msg:("one event for the command " ^ c)
          int 1
          (count (( = ) c) sqls) )
    cmds ;
  List.iter
    (fun c ->
      equal ~msg:("the events of " ^ c) int
        (count (( = ) c) cmds)
        (count (( = ) c) sqls) )
    ["COMMIT"; "ROLLBACK"] ;
  (* A savepoint command that the margin stopped, or that a cancel cut before it
     went, did not reach the fake server. *)
  let went e =
    not
      ( String.ends_with ~suffix:"\" failed Timeout" e
      || String.ends_with ~suffix:"\" cancelled sent=false" e )
  in
  List.iter
    (fun c ->
      equal ~msg:("the events of " ^ c) int
        (count (( = ) c) cmds)
        (count (fun e -> event_sql e = c && went e) events) )
    (List.concat_map
       (fun n ->
         [ "SAVEPOINT cistern_sp_" ^ n
         ; "RELEASE SAVEPOINT cistern_sp_" ^ n
         ; "ROLLBACK TO SAVEPOINT cistern_sp_" ^ n ] )
       ["1"; "2"; "3"] ) ;
  equal ~msg:"the events of START TRANSACTION that went" int
    (count (( = ) "START TRANSACTION") cmds)
    (count
       (fun e ->
         e = "query \"START TRANSACTION\" done 0"
         || e = "query \"START TRANSACTION\" cancelled sent=true" )
       events )

(* The storm over [seeds] seeds. The first 2 seeds run with no seam. With the
   faults of COMMIT and the savepoints, the fewest commits of a seed were 60 of
   200 and 78 of 1000 (the record of PR 1.3b2): the bound is 50. *)
let tx_storm_test ~fibers ~seeds () =
  for seed = 0 to seeds - 1 do
    check_tx_storm ~fibers (tx_storm ~fibers ~seed ~seam:(seed >= 2))
  done

let storm_tests =
  group "Db.transaction and Db.exec, the storm"
    [test "200 fibers, 8 seeds, 4 domains" (tx_storm_test ~fibers:200 ~seeds:8)]

(* The slow storm, outside the gate: make sim-slow (CISTERN_SLOW). *)
let slow_tests =
  group "Db.transaction and Db.exec, the storm, slow"
    [ tag "slow"
        (test "1000 fibers, 20 seeds, 4 domains"
           (tx_storm_test ~fibers:1000 ~seeds:20) ) ]
