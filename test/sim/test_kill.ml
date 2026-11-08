(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* The kill queue and the killer (the plan of phase 1.5, D1, D4, D4a, D5 to D9,
   D14, and the reviews C2 M-a, M-b, L-a, L-b and 1.5b N1, N2; PR 1.5c1a).
   Connection.in_flight against the scripts of Fake_server, then the pool with
   kill=true on Fake_db. In the pool tests, the main fiber waits 1 ms first, so
   that the killer opens its control connection as the connection 1. *)

open Windtrap
open Weft
module C = Cistern_mysql.Config
module Conn = Cistern_mysql.Connection
module E = Cistern_core.Error
module F = Fake_server
module D = Fake_db
module Db = Cistern.Db
module Tx = Cistern.Tx
module Pool = Cistern.Private.Pool
module T = Test_pool

let config () =
  Result.get_ok
    (C.make ~kill:false ~tls:C.No_tls
       ~address:(C.Tcp {host= "10.0.0.1"; port= 3306})
       ~user:"u" ~password:"pw" ~database:"db" () )

let error_text = function
  | E.Connect m ->
      "Connect " ^ m
  | e ->
      Format.asprintf "%a" E.pp_summary e

(* One exec of "X" after the handshake, with the reply steps [reply], then
   in_flight. [cancel_ms] cancels the exec. *)
let in_flight ?deadline_ms ?cancel_ms reply =
  let result, _ =
    F.run
      (F.handshake () @ (F.Recv :: reply))
      (fun _ ->
        match Conn.connect (config ()) with
        | Error e ->
            error_text e
        | Ok c ->
            Option.iter
              (fun ms ->
                Conn.set_deadline c (Time.add Mono (Time.now Mono) (Span.ms ms)) )
              deadline_ms ;
            let run () = Conn.exec c (Cistern_core.Raw.sql "X") in
            let r =
              match cancel_ms with
              | None -> (
                match run () with Ok _ -> "ok" | Error e -> error_text e )
              | Some ms -> (
                match Fiber.with_timeout Mono (Span.ms ms) run with
                | _ ->
                    "not cancelled"
                | exception Fiber.Timeout ->
                    "cancelled" )
            in
            let f = Conn.in_flight c in
            ignore (Conn.abort c) ;
            Printf.sprintf "%s %b" r f )
  in
  result

let in_flight_rows =
  [ ("an OK", [F.Send (1, F.ok ())], None, None, "ok false")
  ; ( "a server error"
    , [F.Send (1, F.err 1062 "dup")]
    , None
    , None
    , "Constraint_violation Unique false" )
  ; ( "an ERR with the id 0 (R-SEQ0)"
    , [F.Send (0, F.err 4031 "idle"); F.Close]
    , None
    , None
    , "Connection_lost false" )
  ; ( "the end of the stream"
    , [F.Close]
    , None
    , None
    , "Outcome_unknown statement true" )
  ; ( "a read deadline"
    , [F.Hang]
    , Some 50
    , None
    , "Outcome_unknown statement true" )
  ; ("a cancel", [F.Hang], None, Some 50, "cancelled true") ]

(* The events of KILL with no duration, and the connects. *)
let kill_events events =
  List.filter_map
    (fun e ->
      match String.split_on_char ' ' e with
      | "kill" :: ("dropped" | "off") :: _ ->
          Some e
      | "kill" :: _ :: rest ->
          Some (String.concat " " ("kill" :: rest))
      | "connect" :: _ :: rest ->
          Some (String.concat " " ("connect" :: rest))
      | _ ->
          None )
    events

let q = T.q

(* An exec of [sql] that a cancel stops after [ms] ms. *)
let cancelled ?(ms = 10) pool sql =
  match
    Fiber.with_timeout Mono (Span.ms ms) (fun () -> Db.exec pool (q sql))
  with
  | Ok _ ->
      "ok"
  | Error e ->
      error_text e
  | exception Fiber.Timeout ->
      "cancelled"

let wait ms = Time.sleep Mono (Span.ms ms)

let pool_of p = Db.Private.pool p

(* A pool with kill=true: the main fiber waits 1 ms, so that the killer has its
   control connection (the connection 1). *)
let with_kill ?(size = 1) ?checkout_timeout ?setup ?tls f =
  T.with_pool ~size ?checkout_timeout ?setup ?tls ~kill:true (fun sim db pool ->
      wait 1 ; f sim db pool )

(* A pool of 1: an exec that holds the connection, a waiter from 2 ms, then a
   cancel of the exec at 10 ms. [before] runs at 5 ms. It gives the outcome of
   the cancel, of the waiter, and if the waiter got its connection in less than
   1 s. *)
let waiter_after_cancel ?(before = ignore) pool =
  let a = ref "" and w = ref "" and fast = ref false in
  Fiber.scope (fun s ->
      Fiber.fork s (fun () -> a := cancelled pool "SLEEP 5000") ;
      wait 2 ;
      Fiber.fork s (fun () ->
          let t0 = Time.now Mono in
          w := T.result_text (Db.exec ~timeout:(Span.s 40) pool (q "SELECT 9")) ;
          fast :=
            Span.compare (Time.diff Mono (Time.now Mono) t0) (Span.s 1) < 0 ) ;
      wait 3 ;
      before () ) ;
  [!a; !w; string_of_bool !fast]

(* The time of the pool when its control connection is open, polled each 1
   ms. *)
let until_control p =
  while Pool.Private.control p = 0 do
    wait 1
  done ;
  Pool.now p

(* The events of KILL. *)
let only_kills events =
  List.filter
    (fun e -> String.starts_with ~prefix:"kill" e)
    (kill_events events)

(* The time from the end of the body to the end of with_db, in seconds (review C
   of 1.5c1a, L2). A pool of 1 with kill=true: a connection, then a cancel at
   about 11 ms; the body ends at 300 ms. [after_cancel] runs after the cancel,
   and [at_end] in a fiber of an outer scope at the end of the body. [events]
   gets the events of the pool, the newest first. With [size], the body cancels
   that number of statements. *)
let join_case ?(size = 1) ?(events = ref []) ?(setup = ignore)
    ?(after_cancel = ignore) ?(at_end = ignore) () =
  D.run (fun _ db ->
      setup db ;
      let body_end = ref (Time.now Mono) and since = ref 0. in
      Fiber.scope (fun s ->
          Db.with_db ~pool_size:size
            ~on_query:(fun e -> events := T.event_text e :: !events)
            (D.config ~kill:true ())
            (fun pool ->
              wait 1 ;
              ignore (Db.exec pool (q "SELECT 1")) ;
              Fiber.scope (fun c ->
                  for _ = 1 to size do
                    Fiber.fork c (fun () ->
                        ignore (cancelled pool "SLEEP 100000") )
                  done ) ;
              after_cancel db ;
              wait 289 ;
              Fiber.fork s (fun () -> at_end db) ;
              body_end := Time.now Mono ) ;
          since :=
            Int64.to_float
              (Span.to_ns (Time.diff Mono (Time.now Mono) !body_end))
            /. 1e9 ) ;
      !since )

(* The end of with_db with an entry in the queue (the plan of 1.5, D10 and
   D10a). A pool of [size] with kill=true. [outer] runs in a fiber of an outer
   scope from 1 ms, with the pool. The body: an exec (a connection of the pool),
   then the faults refuse and kill_close=1, then a cancel at about 11 ms: the
   first try fails and the control connection cannot open again. The body ends
   at about 311 ms. [around] runs with_db. It gives the time of the end of
   with_db in seconds, the queue, the Killing slots and the control connections
   after with_db, the events, and the text of [outer]. *)
let drain_case ?(size = 1) ?(outer = fun _ _ -> "") ?(around = fun f -> f ()) ()
    =
  D.run (fun _ db ->
      let evs = ref [] and pr = ref None and o = ref "" and ended = ref 0. in
      let t0 = Time.now Mono in
      Fiber.scope (fun s ->
          around (fun () ->
              Db.with_db ~pool_size:size
                ~on_query:(fun e -> evs := T.event_text e :: !evs)
                (D.config ~kill:true ())
                (fun pool ->
                  pr := Some (pool_of pool) ;
                  wait 1 ;
                  Fiber.fork s (fun () -> o := outer db pool) ;
                  ignore (Db.exec pool (q "SELECT 1")) ;
                  db.D.faults.refuse <- true ;
                  db.D.faults.kill_close <- 1 ;
                  ignore (cancelled pool "SLEEP 5000") ;
                  wait 300 ) ;
              ended :=
                Int64.to_float (Span.to_ns (Time.diff Mono (Time.now Mono) t0))
                /. 1e9 ) ) ;
      let p = Option.get !pr in
      ( !ended
      , [ Pool.Private.queue p
        ; List.init (Pool.Private.killing p) Int64.of_int
        ; List.init (Pool.Private.control p) Int64.of_int ]
      , List.rev !evs
      , !o ) )

let tests =
  group "KILL"
    [ cases "Connection.in_flight"
        ~id:(fun (n, _, _, _, _) -> n)
        in_flight_rows
        (fun (_, reply, deadline_ms, cancel_ms, expected) ->
          equal string expected (in_flight ?deadline_ms ?cancel_ms reply) )
    ; test "Connection.in_flight: after the connect, and a broken connection"
        (fun () ->
          let result, _ =
            F.run
              (F.handshake () @ [F.Recv; F.Close])
              (fun _ ->
                match Conn.connect (config ()) with
                | Error e ->
                    error_text e
                | Ok c ->
                    let fresh = Conn.in_flight c in
                    let x = Cistern_core.Raw.sql "X" in
                    let first = Conn.exec c x in
                    let lost = Conn.in_flight c in
                    (* No I/O on a broken connection: the flag of the earlier
                       command stays (review A of 1.5c1a, N1). *)
                    let second = Conn.exec c x in
                    let after = Conn.in_flight c in
                    ignore (Conn.abort c) ;
                    String.concat " "
                      [ string_of_bool fresh
                      ; Result.fold ~ok:(fun _ -> "ok") ~error:error_text first
                      ; string_of_bool lost
                      ; Result.fold ~ok:(fun _ -> "ok") ~error:error_text second
                      ; string_of_bool after ] )
          in
          equal string
            "false Outcome_unknown statement true Connection_lost true" result )
    ; test "Connection.in_flight: a reset whose second write fails" (fun () ->
          let result, _ =
            F.run
              (F.handshake () @ [F.Recv; F.Send (1, F.ok ()); F.Reset])
              (fun _ ->
                match Conn.connect (config ()) with
                | Error e ->
                    error_text e
                | Ok c ->
                    (* The server resets the connection after the OK of
                       COM_RESET_CONNECTION: the first command ended. *)
                    Time.sleep Mono (Span.ms 0) ;
                    let r = Conn.reset c in
                    let f = Conn.in_flight c in
                    ignore (Conn.abort c) ;
                    Result.fold ~ok:(fun () -> "ok") ~error:error_text r
                    ^ " " ^ string_of_bool f )
          in
          equal string "Connection_lost false" result )
    ; test "a cancel after the flush: KILL" (fun () ->
          let r, events =
            with_kill
              ~setup:(fun db -> db.D.faults.kill_ms <- 20)
              (fun _ db pool ->
                let p = pool_of pool in
                let o = cancelled pool "SLEEP 5000" in
                (* The slot stays Killing until the KILL. *)
                let killing = Pool.Private.killing p in
                let t0 = Time.now Mono in
                let next = T.result_text (Db.exec pool (q "SELECT 3")) in
                let waited = Time.diff Mono (Time.now Mono) t0 in
                [ o
                ; string_of_int killing
                ; next
                ; string_of_bool (Span.compare waited (Span.ms 15) >= 0)
                ; string_of_int db.D.killed
                ; string_of_int (Pool.Private.control p) ] )
          in
          equal (list string) ["cancelled"; "1"; "3"; "true"; "1"; "1"] r ;
          (* The control connection, the connection of the cancel, and a new
             connection for SELECT 3. *)
          equal (list string)
            ["connect ok"; "connect ok"; "kill 2 killed"; "connect ok"]
            (kill_events events) )
    ; test "no KILL: a cancel in the wait, an error, an ended statement"
        (fun () ->
          let r, events =
            with_kill (fun _ db pool ->
                let first = ref "" in
                Fiber.scope (fun s ->
                    Fiber.fork s (fun () ->
                        first := T.result_text (Db.exec pool (q "SELECT 4")) ;
                        ignore (Db.exec pool (q "SLEEP 50")) ) ;
                    wait 5 ;
                    (* The only connection is busy: this exec waits. *)
                    let waited = cancelled pool "SELECT 5" in
                    let err = T.result_text (Db.exec pool (q "FAIL 1062")) in
                    [!first; waited; err; string_of_int db.D.killed] ) )
          in
          equal (list string)
            ["4"; "cancelled"; "Constraint_violation Unique"; "0"]
            r ;
          equal (list string) []
            (List.filter
               (fun e -> String.starts_with ~prefix:"kill" e)
               (kill_events events) ) )
    ; test "a read deadline (Outcome_unknown): KILL" (fun () ->
          let r, events =
            with_kill (fun _ db pool ->
                let o =
                  T.result_text
                    (Db.exec ~timeout:(Span.ms 100) pool (q "SLEEP 5000"))
                in
                wait 20 ;
                [o; string_of_int db.D.killed] )
          in
          equal (list string) ["Outcome_unknown statement"; "1"] r ;
          equal (list string) ["kill 2 killed"]
            (List.filter
               (fun e -> String.starts_with ~prefix:"kill" e)
               (kill_events events) ) )
    ; test "a Tx: a cancel in a statement, and a COMMIT past its deadline"
        (fun () ->
          let r, events =
            with_kill (fun _ db pool ->
                let a =
                  match
                    Fiber.with_timeout Mono (Span.ms 10) (fun () ->
                        Db.transaction pool (fun tx ->
                            ignore (Tx.exec tx (q "SLEEP 5000")) ) )
                  with
                  | _ ->
                      "not cancelled"
                  | exception Fiber.Timeout ->
                      "cancelled"
                in
                wait 20 ;
                db.D.faults.commit <- Some (D.Commit_delay 7000) ;
                let b =
                  match
                    Db.transaction ~timeout:(Span.ms 100) pool (fun tx ->
                        ignore (Tx.exec tx (q "INSERT x")) )
                  with
                  | Ok () ->
                      "ok"
                  | Error e ->
                      error_text e
                in
                wait 20 ;
                [a; b; string_of_int db.D.killed] )
          in
          equal (list string) ["cancelled"; "Outcome_unknown commit"; "2"] r ;
          equal (list string)
            ["kill 2 killed"; "kill 3 killed"]
            (List.filter
               (fun e -> String.starts_with ~prefix:"kill" e)
               (kill_events events) ) )
    ; test "no KILL for an ERR before the command (R-SEQ0, N2)" (fun () ->
          let r, events =
            with_kill (fun _ db pool ->
                db.D.faults.commit <- Some D.Commit_seq0 ;
                let o =
                  match
                    Db.transaction pool (fun tx ->
                        ignore (Tx.exec tx (q "INSERT x")) )
                  with
                  | Ok () ->
                      "ok"
                  | Error e ->
                      error_text e
                in
                wait 20 ;
                [ o
                ; string_of_int db.D.killed
                ; string_of_int (Pool.Private.killing (pool_of pool)) ] )
          in
          (* The server did not read COMMIT: nothing runs, and no KILL. *)
          equal (list string) ["Connection_lost"; "0"; "0"] r ;
          equal (list string) []
            (List.filter
               (fun e -> String.starts_with ~prefix:"kill" e)
               (kill_events events) ) )
    ; test "no KILL for an idle transaction at the checkin, and a failed reset"
        (fun () ->
          let r, events =
            with_kill ~size:1 (fun _ db pool ->
                (* A raw BEGIN: IN_TRANS at the checkin, the abort. *)
                let a = T.result_text (Db.exec pool (q "BEGIN")) in
                (* A raw statement makes the entry dirty, then its reset fails:
                   the discard, then a new connection. *)
                let b = T.result_text (Db.exec pool (q "SELECT 1")) in
                db.D.faults.fail_reset <- Some 1105 ;
                let c = T.result_text (Db.exec pool (q "SELECT 2")) in
                wait 20 ;
                [a; b; c; string_of_int db.D.killed] )
          in
          equal (list string) [""; "1"; "2"; "0"] r ;
          equal (list string) [] (only_kills events) )
    ; test "a Kill_off during a try, before the membership check: no KILL"
        (fun () ->
          let r, events =
            Fun.protect
              ~finally:(fun () -> Pool.Private.before_try := ignore)
              (fun () ->
                with_kill ~size:2 (fun _ db pool ->
                    (* The killer waits 50 ms after it read the head. *)
                    (Pool.Private.before_try :=
                       fun () -> Fiber.shield (fun () -> wait 50) ) ;
                    let a = cancelled pool "SLEEP 5000" in
                    (* A new connection of the pool with a refused identity
                       turns KILL off in that window. *)
                    db.D.faults.identity <- D.Identity_privileged ;
                    let b = T.result_text (Db.exec pool (q "SELECT 3")) in
                    wait 100 ;
                    [a; b; string_of_int db.D.killed] ) )
          in
          equal (list string) ["cancelled"; "3"; "0"] r ;
          equal (list string)
            ["kill dropped 2 off"; "kill off privileged"]
            (List.sort compare (only_kills events)) )
    ; test "the join of the scope while a try runs ends within 2 s" (fun () ->
          let since =
            join_case
              ~setup:(fun db -> db.D.faults.kill_ms <- 10_000)
              ~at_end:(fun db -> db.D.faults.kill_ms <- 0)
              ()
          in
          is_true
            ~msg:(Printf.sprintf "%.3f s after the body" since)
            (since >= 1.5 && since < 2.2) )
    ; test "the join of the scope while the killer reconnects ends at once"
        (fun () ->
          let since =
            join_case
              ~setup:(fun db -> db.D.faults.kill_close <- 1)
              ~after_cancel:(fun db -> db.D.faults.hang_greeting <- true)
              ~at_end:(fun db -> db.D.faults.hang_greeting <- false)
              ()
          in
          is_true
            ~msg:(Printf.sprintf "%.3f s after the body" since)
            (since < 0.2) )
    ; test "the drain uses the state of the killer" (fun () ->
          (* The first try fails in the killer, and its reopen is refused: the
             killer waits for its retry at about 311 ms. The drain must wait for
             that time (not connect at once), and it gives the entry only its
             last try, which fails too (review C of 1.5c1b, L1). *)
          let events = ref [] in
          let since =
            join_case ~events
              ~setup:(fun db -> db.D.faults.kill_close <- 2)
              ~after_cancel:(fun db -> db.D.faults.refuse <- true)
              ~at_end:(fun db -> db.D.faults.refuse <- false)
              ()
          in
          is_true
            ~msg:(Printf.sprintf "%.3f s after the body" since)
            (since >= 0.005 && since < 0.1) ;
          equal (list string) ["kill dropped 2 failed"]
            (only_kills (List.rev !events)) )
    ; test "a try at the limit of the drain: the end at most 2 s later"
        (fun () ->
          (* Each KILL reply is late (3 s): each try ends at its limit of 2 s.
             The killer holds the join until about 2 s; then the drain gives the
             5 entries their tries of 2 s each, and the try that runs at the
             limit of the drain (about 17 s) ends at about 18 s (review C of
             1.5c1b, L2). *)
          let events = ref [] in
          let since =
            join_case ~size:5 ~events
              ~setup:(fun db -> db.D.faults.kill_ms <- 3000)
              ()
          in
          is_true
            ~msg:(Printf.sprintf "%.3f s after the body" since)
            (since >= 16.5 && since < 19.0) ;
          let kills = only_kills (List.rev !events) in
          equal int 5 (List.length kills) ;
          is_true ~msg:(String.concat "; " kills)
            (String.ends_with ~suffix:"drain_timeout" (List.nth kills 4)) )
    ; test "the control connection: a failed connect, then the backoff"
        (fun () ->
          let r, events =
            with_kill
              ~setup:(fun db -> db.D.faults.refuse <- true)
              (fun _ db pool ->
                wait 50 ;
                db.D.faults.refuse <- false ;
                let o = cancelled pool "SLEEP 5000" in
                wait 200 ;
                [o; string_of_int db.D.killed] )
          in
          equal (list string) ["cancelled"; "1"] r ;
          equal (list string)
            [ "connect Connect the server closed the connection"
            ; "connect ok"
            ; "connect ok"
            ; "kill 2 killed" ]
            (kill_events events) )
    ; test "the control connection past its idle limit" (fun () ->
          Pool.Private.reset_counters () ;
          let r, _ =
            with_kill
              ~setup:(fun db -> db.D.faults.wait_timeout <- 6)
              (fun _ db pool ->
                (* The idle limit is 1 s. *)
                wait 1500 ;
                let o = cancelled pool "SLEEP 5000" in
                wait 20 ;
                [o; string_of_int db.D.killed] )
          in
          equal (list string) ["cancelled"; "1"] r ;
          equal int 1 (Pool.Private.counters ()).control_reopened )
    ; test "a 4031 on the control connection: stale" (fun () ->
          Pool.Private.reset_counters () ;
          let r, _ =
            with_kill
              ~setup:(fun db -> db.D.faults.idle_4031_ms <- Some 25)
              (fun _ db pool ->
                wait 50 ;
                let o = cancelled pool "SLEEP 5000" in
                wait 20 ;
                [o; string_of_int db.D.killed] )
          in
          equal (list string) ["cancelled"; "1"] r ;
          equal int 1 (Pool.Private.counters ()).control_reopened )
    ; test "the control connection breaks in KILL: one retry" (fun () ->
          Pool.Private.reset_counters () ;
          let r, events =
            with_kill
              ~setup:(fun db -> db.D.faults.kill_close <- 1)
              (fun _ db pool ->
                let o = cancelled pool "SLEEP 5000" in
                wait 50 ;
                [o; string_of_int db.D.killed] )
          in
          equal (list string) ["cancelled"; "1"] r ;
          equal int 1 (Pool.Private.counters ()).kill_retry ;
          (* The killer aborted the control connection after the failure (D9):
             the stale check did not do it. *)
          equal int 0 (Pool.Private.counters ()).control_reopened ;
          equal (list string) ["kill 2 killed"]
            (List.filter
               (fun e -> String.starts_with ~prefix:"kill" e)
               (kill_events events) ) )
    ; test "two failures: Kill_dropped, and the slot is free" (fun () ->
          Pool.Private.reset_counters () ;
          let r, events =
            with_kill
              ~setup:(fun db -> db.D.faults.kill_close <- 2)
              (fun _ db pool ->
                let o = cancelled pool "SLEEP 5000" in
                wait 50 ;
                let next = T.result_text (Db.exec pool (q "SELECT 6")) in
                [o; next; string_of_int db.D.killed] )
          in
          equal (list string) ["cancelled"; "6"; "0"] r ;
          equal int 1 (Pool.Private.counters ()).kill_dropped ;
          equal (list string) ["kill dropped 2 failed"]
            (List.filter
               (fun e -> String.starts_with ~prefix:"kill" e)
               (kill_events events) ) )
    ; test "the limit of 2 s of one try" (fun () ->
          let r, events =
            with_kill
              ~setup:(fun db -> db.D.faults.kill_ms <- 3000)
              (fun _ db pool ->
                let t0 = Time.now Mono in
                let o = cancelled pool "SLEEP 9000" in
                let next = T.result_text (Db.exec pool (q "SELECT 7")) in
                let s = Span.to_ns (Time.diff Mono (Time.now Mono) t0) in
                [ o
                ; next
                ; string_of_bool (s >= 4_000_000_000L && s < 4_500_000_000L)
                ; string_of_int db.D.killed ] )
          in
          (* The server ends the first KILL after 3 s, also after the client
             gave up: the session is gone, and the second try is past its limit
             too. *)
          equal (list string) ["cancelled"; "7"; "true"; "1"] r ;
          equal (list string) ["kill dropped 2 failed"]
            (List.filter
               (fun e -> String.starts_with ~prefix:"kill" e)
               (kill_events events) ) )
    ; test "1095: KILL goes off for the pool, and the queue is dropped"
        (fun () ->
          let r, events =
            with_kill ~size:2
              ~setup:(fun db ->
                db.D.faults.kill_err <- Some 1095 ;
                db.D.faults.kill_ms <- 50 )
              (fun _ _ pool ->
                let p = pool_of pool in
                let a = ref "" and b = ref "" in
                Fiber.scope (fun s ->
                    Fiber.fork s (fun () -> a := cancelled pool "SLEEP 5000") ;
                    Fiber.fork s (fun () ->
                        b := cancelled ~ms:20 pool "SLEEP 5000" ) ) ;
                wait 100 ;
                (* KILL is off: a later cancel frees its slot at once. *)
                let c = cancelled pool "SLEEP 5000" in
                [ !a
                ; !b
                ; c
                ; string_of_bool (Pool.Private.kill_off p)
                ; string_of_int (Pool.Private.killing p)
                ; string_of_int (Pool.Private.control p) ] )
          in
          equal (list string)
            ["cancelled"; "cancelled"; "cancelled"; "true"; "0"; "0"]
            r ;
          equal (list string)
            [ "kill dropped 2 not_owner"
            ; "kill off not_owner"
            ; "kill dropped 3 off" ]
            (List.filter
               (fun e -> String.starts_with ~prefix:"kill" e)
               (kill_events events) ) )
    ; test "another server, and the id of the control connection (N1)"
        (fun () ->
          let r, events =
            with_kill ~size:2 (fun _ db pool ->
                db.D.faults.server_uuid <- "another" ;
                let a = cancelled pool "SLEEP 5000" in
                wait 20 ;
                db.D.faults.server_uuid <-
                  "5c607771-db0a-11f1-86e8-e86538021fcd" ;
                (* The next connection has the id of the control connection. *)
                db.D.faults.id_of <- (fun _ -> 1) ;
                let b = cancelled pool "SLEEP 5000" in
                wait 20 ;
                [a; b; string_of_int db.D.killed] )
          in
          equal (list string) ["cancelled"; "cancelled"; "0"] r ;
          equal (list string)
            ["kill dropped 2 other_server"; "kill dropped 1 other_server"]
            (List.filter
               (fun e -> String.starts_with ~prefix:"kill" e)
               (kill_events events) ) )
    ; test "a duplicate in the queue: its slot is free at once" (fun () ->
          Pool.Private.reset_counters () ;
          let r, events =
            with_kill ~size:2
              ~setup:(fun db -> db.D.faults.kill_ms <- 100)
              (fun _ db pool ->
                db.D.faults.id_of <- (fun n -> if n = 1 then 1 else 77) ;
                let w = ref "" and fast = ref false in
                Fiber.scope (fun s ->
                    Fiber.fork s (fun () ->
                        ignore (cancelled pool "SLEEP 5000") ) ;
                    Fiber.fork s (fun () ->
                        ignore (cancelled ~ms:30 pool "SLEEP 5000") ) ;
                    wait 15 ;
                    (* A waiter: the free slot of the duplicate comes at 30 ms,
                       the KILL ends at about 110 ms. *)
                    Fiber.fork s (fun () ->
                        let t0 = Time.now Mono in
                        w := T.result_text (Db.exec pool (q "SELECT 9")) ;
                        fast :=
                          Span.compare
                            (Time.diff Mono (Time.now Mono) t0)
                            (Span.ms 60)
                          < 0 ) ) ;
                wait 200 ;
                [!w; string_of_bool !fast; string_of_int db.D.killed] )
          in
          equal (list string) ["9"; "true"; "1"] r ;
          equal (list string)
            ["kill dropped 77 duplicate"; "kill 77 killed"]
            (List.filter
               (fun e -> String.starts_with ~prefix:"kill" e)
               (kill_events events) ) ;
          (* The two kill requests: also the duplicate (phase 1.5c2). *)
          equal int 2 (Pool.Private.counters ()).kill_requests )
    ; test "the drain past its limit of 15 s" (fun () ->
          let ended, after, events, outer =
            drain_case ~size:2
              ~outer:(fun _ pool ->
                (* A cancel after the drain: its slot is free at once. *)
                cancelled ~ms:16_000 pool "SLEEP 100000" )
              ()
          in
          equal string "cancelled" outer ;
          (* The body ends at about 311 ms: the limit of the drain ends it at
             about 15.3 s. *)
          is_true
            ~msg:(Printf.sprintf "the end at %.3f s" ended)
            (ended >= 15.0 && ended < 16.0) ;
          (* After with_db: no entry, no Killing slot, no control connection. *)
          equal (list (list int64)) [[]; []; []] after ;
          equal (list string)
            ["kill dropped 2 drain_timeout"; "kill dropped 3 closed"]
            (only_kills events) )
    ; test "the drain sends the entries that remain" (fun () ->
          let ended, after, events, _ =
            drain_case
              ~outer:(fun db _ ->
                wait 400 ;
                db.D.faults.refuse <- false ;
                "" )
              ()
          in
          is_true
            ~msg:(Printf.sprintf "the end at %.3f s" ended)
            (ended >= 0.4 && ended < 1.5) ;
          equal (list (list int64)) [[]; []; []] after ;
          equal (list string) ["kill 2 killed"] (only_kills events) )
    ; test "the drain in an outer cancel" (fun () ->
          let ended, after, events, _ =
            drain_case
              ~around:(fun f ->
                match Fiber.with_timeout Mono (Span.ms 350) f with
                | () ->
                    ()
                | exception Fiber.Timeout ->
                    () )
              ~outer:(fun db _ ->
                wait 400 ;
                db.D.faults.refuse <- false ;
                "" )
              ()
          in
          (* The cancel at 350 ms does not stop the drain (its shield). *)
          is_true
            ~msg:(Printf.sprintf "the end at %.3f s" ended)
            (ended >= 0.4 && ended < 1.5) ;
          equal (list (list int64)) [[]; []; []] after ;
          equal (list string) ["kill 2 killed"] (only_kills events) )
    ; test "a waiter of an outer scope fails at once, before the drain (C-M1)"
        (fun () ->
          let ended, _, events, outer =
            drain_case
              ~outer:(fun _ pool ->
                wait 20 ;
                (* The only slot is Killing: the exec waits. *)
                let t0 = Time.now Mono in
                let r = T.result_text (Db.exec pool (q "SELECT 2")) in
                let s = Span.to_ns (Time.diff Mono (Time.now Mono) t0) in
                Printf.sprintf "%s %b" r (s < 320_000_000L) )
              ()
          in
          equal string "Connection_lost true" outer ;
          is_true (ended >= 15.0) ;
          equal (list string)
            ["kill dropped 2 drain_timeout"]
            (only_kills events) )
    ; test "a Kill during the drain: the drain takes it (C-M2)" (fun () ->
          let _, after, events, outer =
            drain_case ~size:2
              ~outer:(fun db pool ->
                let r = ref "" in
                Fiber.scope (fun s ->
                    Fiber.fork s (fun () ->
                        r := cancelled ~ms:350 pool "SLEEP 5000" ) ;
                    wait 400 ;
                    db.D.faults.refuse <- false ) ;
                (* The drain opens its control connection at the end of its
                   backoff. *)
                wait 1000 ;
                !r ^ " " ^ string_of_int db.D.killed )
              ()
          in
          equal string "cancelled 2" outer ;
          equal (list (list int64)) [[]; []; []] after ;
          equal (list string)
            ["kill 2 killed"; "kill 3 killed"]
            (List.sort compare (only_kills events)) )
    ; test "a cancel after the shutdown: Kill_dropped Closed" (fun () ->
          let after, events =
            D.run (fun _ _ ->
                let evs = ref [] and pr = ref None in
                Fiber.scope (fun s ->
                    Db.with_db ~pool_size:1
                      ~on_query:(fun e -> evs := T.event_text e :: !evs)
                      (D.config ~kill:true ())
                      (fun pool ->
                        pr := Some (pool_of pool) ;
                        wait 1 ;
                        (* A fiber of the outer scope: its cancel comes after
                           the shutdown. *)
                        Fiber.fork s (fun () ->
                            ignore (cancelled ~ms:50 pool "SLEEP 5000") ) ;
                        wait 5 ) ) ;
                let p = Option.get !pr in
                ( [ Pool.Private.queue p
                  ; List.init (Pool.Private.killing p) Int64.of_int
                  ; List.init (Pool.Private.sockets p) Int64.of_int ]
                , List.rev !evs ) )
          in
          equal (list (list int64)) [[]; []; []] after ;
          equal (list string) ["kill dropped 2 closed"]
            (List.filter
               (fun e -> String.starts_with ~prefix:"kill" e)
               (kill_events events) ) )
    ; test "the age limit, a full queue, and a KILL after the age limit"
        (fun () ->
          let r, events =
            with_kill ~checkout_timeout:(Span.s 60) (fun _ db pool ->
                let p = pool_of pool in
                let w = ref "" and waited = ref Span.zero in
                Fiber.scope (fun s ->
                    Fiber.fork s (fun () ->
                        ignore (cancelled pool "SLEEP 100000") ) ;
                    wait 2 ;
                    (* A waiter before the cancel: the new Killing slot makes
                       the timer of the opener (L-a), and the age limit gives
                       the waiter its connect. *)
                    Fiber.fork s (fun () ->
                        let t0 = Time.now Mono in
                        w :=
                          T.result_text
                            (Db.exec ~timeout:(Span.s 40) pool (q "SELECT 8")) ;
                        waited := Time.diff Mono (Time.now Mono) t0 ) ;
                    wait 3 ;
                    (* The KILL hangs, and the control connection cannot open
                       again: the entry stays in the queue. *)
                    db.D.faults.kill_ms <- 3000 ;
                    db.D.faults.refuse <- true ;
                    wait 29_000 ;
                    db.D.faults.kill_ms <- 0 ;
                    db.D.faults.refuse <- false ) ;
                (* The slot of the first entry passed its age limit (30 s), but
                   the entry stays in the queue until the killer connects again
                   (at about 33.3 s): the queue is full. *)
                let full = cancelled pool "SLEEP 100000" in
                let queue = Pool.Private.queue p in
                wait 5_000 ;
                [ !w
                ; string_of_bool
                    ( Span.compare !waited (Span.ms 29_900) >= 0
                    && Span.compare !waited (Span.ms 30_200) < 0 )
                ; full
                ; String.concat "," (List.map Int64.to_string queue)
                ; string_of_int db.D.killed
                ; string_of_int (Pool.Private.killing p) ] )
          in
          equal (list string) ["8"; "true"; "cancelled"; "2"; "1"; "0"] r ;
          (* The refused connects take numbers too, thus the id of the full
             queue is not shown. The server ended the first, hung KILL after 3
             s: the KILL after the age limit finds the session gone. *)
          equal (list string)
            ["kill dropped queue_full"; "kill 2 gone"]
            (List.filter_map
               (fun e ->
                 match String.split_on_char ' ' e with
                 | ["kill"; "dropped"; _; reason] ->
                     Some ("kill dropped " ^ reason)
                 | "kill" :: _ ->
                     Some e
                 | _ ->
                     None )
               (kill_events events) ) )
    ; test "kill off: a cancel frees its slot for a waiter at once" (fun () ->
          let r, _ =
            with_kill
              ~setup:(fun db -> db.D.faults.identity <- D.Identity_privileged)
              (fun _ _ pool -> waiter_after_cancel pool)
          in
          equal (list string) ["cancelled"; "9"; "true"] r )
    ; test "Kill_off by the control connection frees the queue for a waiter"
        (fun () ->
          let r, events =
            with_kill
              ~setup:(fun db -> db.D.faults.kill_close <- 1)
              (fun _ db pool ->
                waiter_after_cancel
                  ~before:(fun () ->
                    db.D.faults.identity <- D.Identity_privileged )
                  pool )
          in
          equal (list string) ["cancelled"; "9"; "true"] r ;
          equal (list string)
            ["kill dropped 2 off"; "kill off privileged"]
            (List.sort compare
               (List.filter
                  (fun e -> String.starts_with ~prefix:"kill" e)
                  (kill_events events) ) ) )
    ; test "Kill_off at a connect of the pool closes the control connection"
        (fun () ->
          let r, _ =
            with_kill (fun _ db pool ->
                let p = pool_of pool in
                let before = Pool.Private.control p in
                db.D.faults.identity <- D.Identity_privileged ;
                ignore (Db.exec pool (q "SELECT 1")) ;
                wait 5 ;
                [before; Pool.Private.control p] )
          in
          equal (list int) [1; 0] r )
    ; test "the backoff of the control connection: 100 ms, doubled, at most 5 s"
        (fun () ->
          let r, _ =
            with_kill
              ~setup:(fun db -> db.D.faults.refuse <- true)
              (fun _ db pool ->
                let p = pool_of pool in
                (* Refused at 0, 0.1, 0.3, 0.7, 1.5, 3.1 and 6.3 s: then the
                   step is 5 s, and the control connection opens at 11.3 s. *)
                wait 10_999 ;
                db.D.faults.refuse <- false ;
                let opened = until_control p in
                ignore (Db.exec pool (q "SELECT 1")) ;
                (* After a success the step is 100 ms again: a failed try, and a
                   refused open. *)
                db.D.faults.kill_close <- 1 ;
                db.D.faults.refuse <- true ;
                let o = cancelled pool "SLEEP 5000" in
                wait 50 ;
                db.D.faults.refuse <- false ;
                let again = until_control p in
                wait 20 ;
                [ string_of_bool
                    ( Span.compare opened (Span.ms 11_300) >= 0
                    && Span.compare opened (Span.ms 11_302) < 0 )
                ; o
                ; string_of_bool (Span.compare again (Span.ms 11_500) < 0)
                ; string_of_int db.D.killed ] )
          in
          equal (list string) ["true"; "cancelled"; "true"; "1"] r )
    ; test "the idle limit counts from the last KILL" (fun () ->
          Pool.Private.reset_counters () ;
          let r, _ =
            with_kill
              ~setup:(fun db -> db.D.faults.wait_timeout <- 6)
              (fun _ db pool ->
                (* The idle limit is 1 s: KILLs at 0.6 s and 1.4 s. *)
                wait 600 ;
                let a = cancelled pool "SLEEP 5000" in
                wait 790 ;
                let b = cancelled pool "SLEEP 5000" in
                wait 20 ;
                [a; b; string_of_int db.D.killed] )
          in
          equal (list string) ["cancelled"; "cancelled"; "2"] r ;
          equal int 0 (Pool.Private.counters ()).control_reopened )
    ; test "the control connection over TLS" (fun () ->
          let r, events =
            with_kill ~tls:true (fun _ db pool ->
                let o = cancelled pool "SLEEP 5000" in
                wait 50 ;
                [o; string_of_int db.D.killed; string_of_int db.D.tls_sessions] )
          in
          equal (list string) ["cancelled"; "1"; "2"] r ;
          equal (list string) ["kill 2 killed"]
            (List.filter
               (fun e -> String.starts_with ~prefix:"kill" e)
               (kill_events events) ) )
    ; test "kill=false: no killer and no control connection" (fun () ->
          let r, events =
            T.with_pool ~size:1 (fun _ db pool ->
                wait 1 ;
                let c = Pool.Private.control (pool_of pool) in
                let o = cancelled pool "SLEEP 5000" in
                [o; string_of_int c; string_of_int db.D.killed] )
          in
          equal (list string) ["cancelled"; "0"; "0"] r ;
          equal (list string) ["connect ok"] (kill_events events) ) ]
