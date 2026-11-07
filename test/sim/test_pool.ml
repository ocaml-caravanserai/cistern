(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* Db and the weft shell of the pool against the fake server of many connections
   (the plan of phase 1.2, sections 3.2 to 3.7 and 4.5): the checkout, the
   waiters, the opener, the deadlines, the session reset, the cancel, the end of
   with_db, and one event on each path. *)

open Windtrap
open Weft
module Db = Cistern.Db
module Raw = Cistern.Raw
module Stmt = Cistern.Stmt
module Pool = Cistern.Private.Pool
module E = Cistern_core.Error
module Ev = Cistern_core.Event
module Value = Cistern_core.Value
module D = Fake_db

let error_text = function
  | E.Connect m ->
      "Connect " ^ m
  | e ->
      Format.asprintf "%a" E.pp_summary e

let result_text = function
  | Ok (r : Cistern_core.Raw.result) ->
      String.concat ","
        (List.map
           (fun row ->
             String.concat " "
               (Array.to_list
                  (Array.map
                     (function Value.Int i -> Int64.to_string i | _ -> "?")
                     row ) ) )
           r.rows )
  | Error e ->
      error_text e

let event_text = function
  | Ev.Query {sql; outcome; _} ->
      Printf.sprintf "query %S %s" sql
        ( match outcome with
        | Ev.Done {rows} ->
            Printf.sprintf "done %d" rows
        | Ev.Failed e ->
            "failed " ^ error_text e
        | Ev.Cancelled {sent} ->
            Printf.sprintf "cancelled sent=%b" sent )
  | Ev.Connect {address; error; _} ->
      Printf.sprintf "connect %s %s" address
        (match error with None -> "ok" | Some e -> error_text e)

let q s = Raw.query (Raw.sql s)

let seconds since =
  let d = Time.diff Mono (Time.now Mono) since in
  Int64.to_float (Span.to_ns d) /. 1e9

(* The pool after with_db: no socket, no slot. *)
let check_end pool =
  let p = Db.Private.pool pool in
  equal ~msg:"the sockets at the end" int 0 (Pool.Private.sockets p) ;
  equal ~msg:"the opens over the bound" int 0 (Pool.Private.over_bound p) ;
  let idle, in_use, opening, closing, waiting = Pool.Private.state p in
  equal ~msg:"the slots at the end" (list int) [0; 0; 0; 0; 0]
    [idle; in_use; opening; closing; waiting]

(* [with_pool ?size ... f]: a pool on the fake server, with the events in a
   list. After with_db, it does the checks of [check_end]. *)
let with_pool ?(size = 2) ?max_lifetime ?checkout_timeout ?(seed = 0)
    ?(domains = 1) ?(setup = fun _ -> ()) f =
  let events = ref [] in
  let result =
    D.run ~seed ~domains (fun sim db ->
        setup db ;
        let last = ref None in
        let r =
          Db.with_db ~pool_size:size ?max_lifetime ?checkout_timeout
            ~random:(Random.State.make [|seed|])
            ~on_query:(fun e -> events := event_text e :: !events)
            (D.config ())
            (fun pool ->
              last := Some pool ;
              f sim db pool )
        in
        check_end (Option.get !last) ;
        r )
  in
  (result, List.rev !events)

let bad_literal () = Raw.query Raw.(sql "SELECT" ++ v (Value.Float Float.nan))

(* A statement of the tests with its own run, on top of Exec. *)
let custom ?(raw = true) run =
  let r = Raw.sql "CUSTOM" in
  Stmt.Private.make
    { redacted= (fun _ -> ["CUSTOM"])
    ; kind= Stmt.Private.Write
    ; raw= (if raw then Some r else None)
    ; sql= (fun _ -> ["CUSTOM"])
    ; text= Some (Ok "CUSTOM") }
    ~run
    ~rows:(fun _ -> 0)
    ~finish:Fun.id

(* A statement that is not raw (as the typed statements of phase 2): it does not
   make its entry dirty. *)
let clean () =
  custom ~raw:false (fun ctx ->
      Result.map ignore (Cistern.Private.Exec.exec ctx (Raw.sql "CLEAN")) )

let queries events =
  List.filter (fun e -> String.starts_with ~prefix:"query" e) events

(* The latch of the races: each of the next operations, one each, waits in a
   shield until the time [at] (ms from [t0]) of [ats]. *)
let latch t0 ats =
  let q = ref ats in
  Pool.Private.after_op :=
    fun () ->
      match !q with
      | at :: rest ->
          q := rest ;
          Fiber.shield (fun () ->
              let left =
                Int64.sub
                  (Int64.mul (Int64.of_int at) 1_000_000L)
                  (Span.to_ns (Time.diff Mono (Time.now Mono) t0))
              in
              if Int64.compare left 0L > 0 then
                Time.sleep Mono (Span.of_ns left) )
      | [] ->
          ()

(* A statement that makes the server of its connection deaf: DEAF, then a
   statement that fills the receive buffer of the server to 2 bytes from its end
   (the simulator holds 65536 bytes, as in the COM_QUIT test of
   test_connect.ml), thus a COM_QUIT (5 bytes) cannot go. It is not raw, thus
   the entry stays clean (no reset at its next checkout). [hold]: it keeps its
   connection this long, so that 4 statements take 4 connections. *)
let deaf ~hold =
  let fill = "X" ^ String.make 65528 'x' in
  Stmt.Private.make
    { redacted= (fun _ -> ["DEAF"])
    ; kind= Stmt.Private.Write
    ; raw= None
    ; sql= (fun _ -> ["DEAF"])
    ; text= None }
    ~run:(fun ctx ->
      match Cistern.Private.Exec.exec ctx (Raw.sql "DEAF") with
      | Error e ->
          Error e
      | Ok _ ->
          let r = Cistern.Private.Exec.exec ctx (Raw.sql fill) in
          Time.sleep Mono hold ; r )
    ~rows:(fun _ -> 0)
    ~finish:ignore

let tests =
  group "Db and the pool"
    [ test "a query, and its events" (fun () ->
          let r, events =
            with_pool (fun _ _ pool ->
                result_text (Db.exec pool (q "SELECT 5")) )
          in
          equal string "5" r ;
          equal (list string)
            ["connect 10.0.0.9:3306 ok"; "query \"SELECT 5\" done 1"]
            events )
    ; test "exec_exn and an error of the server" (fun () ->
          let r, events =
            with_pool (fun _ _ pool ->
                match Db.exec_exn pool (q "FAIL 1062") with
                | _ ->
                    "no error"
                | exception E.Cistern_error e ->
                    error_text e )
          in
          equal string "Constraint_violation Unique" r ;
          equal (list string)
            ["query \"FAIL 1062\" failed Constraint_violation Unique"]
            (queries events) )
    ; test "a value with no literal: no checkout" (fun () ->
          let (r, connects), events =
            with_pool (fun _ db pool ->
                let r = Db.exec pool (bad_literal ()) in
                (result_text r, db.connects) )
          in
          equal string "Invalid_value" r ;
          equal int 0 connects ;
          equal (list string)
            ["query \"SELECT ? \" failed Invalid_value"]
            events )
    ; test "a connect error shows at the deadline, with the backoff" (fun () ->
          let (r, t, connects), events =
            with_pool
              ~setup:(fun db -> db.faults.refuse <- true)
              (fun _ db pool ->
                let t0 = Time.now Mono in
                let r = Db.exec ~timeout:(Span.s 1) pool (q "SELECT 1") in
                (result_text r, seconds t0, db.connects) )
          in
          equal string "Connect the server closed the connection" r ;
          equal (float 1e-9) 1.0 t ;
          (* Tries at 0, 100, 300 and 700 ms: the backoff doubles. *)
          equal int 4 connects ;
          equal string
            "query \"SELECT 1\" failed Connect the server closed the connection"
            (List.nth events (List.length events - 1)) ;
          equal string
            "connect 10.0.0.9:3306 Connect the server closed the connection"
            (List.hd events) )
    ; test "a connect error, then a success" (fun () ->
          let r, _ =
            with_pool
              ~setup:(fun db -> db.faults.refuse <- true)
              (fun _ db pool ->
                let a =
                  result_text
                    (Db.exec ~timeout:(Span.ms 50) pool (q "SELECT 1"))
                in
                db.faults.refuse <- false ;
                let b = result_text (Db.exec pool (q "SELECT 2")) in
                a ^ "; " ^ b )
          in
          equal string "Connect the server closed the connection; 2" r )
    ; cases "the error of a waiter at its limit"
        ~id:(fun (n, _, _, _) -> n)
        [ ("the checkout limit first", Span.s 10, Span.s 1, "Pool_exhausted 1")
        ; ("the deadline first", Span.s 1, Span.s 10, "Timeout 1")
        ; ("equal times", Span.s 1, Span.s 1, "Timeout 1") ]
        (fun (_, timeout, checkout_timeout, expected) ->
          let r, _ =
            with_pool ~size:1 ~checkout_timeout (fun _ _ pool ->
                Fiber.scope (fun s ->
                    Fiber.fork s (fun () ->
                        ignore (Db.exec pool (q "SLEEP 5000")) ) ;
                    Time.sleep Mono (Span.ms 1) ;
                    let t0 = Time.now Mono in
                    let r = Db.exec ~timeout pool (q "SELECT 1") in
                    Printf.sprintf "%s %.0f" (result_text r) (seconds t0) ) )
          in
          equal string expected r )
    ; test "the margin: 41 ms left sends nothing, and the entry stays good"
        (fun () ->
          let r, _ =
            with_pool ~size:1 (fun _ db pool ->
                ignore (Db.exec pool (q "SELECT 1")) ;
                Fiber.scope (fun s ->
                    Fiber.fork s (fun () ->
                        ignore (Db.exec pool (q "SLEEP 960")) ) ;
                    Time.sleep Mono (Span.ms 1) ;
                    let before = db.commands in
                    let r =
                      Db.exec ~timeout:(Span.ms 1000) pool (q "SELECT 2")
                    in
                    let sent = db.commands - before in
                    let resets = db.resets in
                    let next = result_text (Db.exec pool (q "SELECT 3")) in
                    Printf.sprintf "%s %d %s %d %d" (result_text r) sent next
                      db.connects (db.resets - resets) ) )
          in
          (* The second exec waits 959 ms, then has 41 ms: Timeout, and it sends
             nothing. The next exec uses the same connection, which stays dirty
             (no reset ran): it gets its reset. *)
          equal string "Timeout 0 3 1 1" r )
    ; test "a stale idle entry: another connection, no error" (fun () ->
          let (r, connects), _ =
            with_pool
              ~setup:(fun db -> db.faults.idle_4031_ms <- Some 100)
              (fun _ db pool ->
                ignore (Db.exec pool (q "SELECT 1")) ;
                Time.sleep Mono (Span.ms 200) ;
                Pool.Private.reset_counters () ;
                let r = result_text (Db.exec pool (q "SELECT 2")) in
                (r, db.connects) )
          in
          equal string "2" r ;
          equal int 2 connects ;
          equal int 1 (Pool.Private.counters ()).stale_at_checkout )
    ; test "the session reset of a dirty entry" (fun () ->
          let r, _ =
            with_pool ~size:1 (fun _ db pool ->
                ignore (Db.exec pool (q "SELECT 1")) ;
                let a = db.resets in
                ignore (Db.exec pool (q "SELECT 2")) ;
                Printf.sprintf "%d %d" a db.resets )
          in
          (* A new connection is clean; a raw statement makes it dirty. *)
          equal string "0 1" r )
    ; test "a failed reset: another connection" (fun () ->
          let r, _ =
            with_pool ~size:1 (fun _ db pool ->
                ignore (Db.exec pool (q "SELECT 1")) ;
                db.faults.fail_reset <- Some 1047 ;
                let r = result_text (Db.exec pool (q "SELECT 2")) in
                (Printf.sprintf "%s %d" r db.connects, db) )
          in
          let r, db = r in
          equal string "2 2" r ;
          (* The pool aborts the first connection: only the second gets a
             QUIT. *)
          equal int 1 db.quits )
    ; test "two failed resets: the error of the second" (fun () ->
          let r, _ =
            with_pool ~size:2 (fun _ db pool ->
                Fiber.scope (fun s ->
                    Fiber.fork s (fun () ->
                        ignore (Db.exec pool (q "SLEEP 10")) ) ;
                    ignore (Db.exec pool (q "SLEEP 10")) ) ;
                db.faults.fail_reset <- Some 1047 ;
                result_text (Db.exec pool (q "SELECT 2")) )
          in
          equal string "Driver 1047 28000" r )
    ; test "BEGIN outside a Tx: the entry is aborted at checkin" (fun () ->
          let r, _ =
            with_pool ~size:1 (fun _ db pool ->
                ignore (Db.exec pool (q "BEGIN")) ;
                ignore (Db.exec pool (q "SELECT 1")) ;
                Printf.sprintf "%d %d" db.connects db.resets )
          in
          (* The second exec opens a new connection, with no reset. *)
          equal string "2 0" r )
    ; test "the deadline does not stay on a reused connection" (fun () ->
          let r, _ =
            with_pool ~size:1 (fun _ db pool ->
                let a =
                  result_text (Db.exec ~timeout:(Span.s 1) pool (q "SELECT 1"))
                in
                Time.sleep Mono (Span.s 1) ;
                let b =
                  result_text (Db.exec ~timeout:(Span.s 5) pool (q "SLEEP 800"))
                in
                Printf.sprintf "%s %s %d" a b db.connects )
          in
          equal string "1  1" r )
    ; test "a read deadline: Outcome_unknown, and the pool stays usable"
        (fun () ->
          let r, events =
            with_pool ~size:1 (fun _ _ pool ->
                let a =
                  result_text
                    (Db.exec ~timeout:(Span.ms 200) pool (q "SLEEP 2000"))
                in
                Pool.Private.reset_counters () ;
                let b = result_text (Db.exec pool (q "SELECT 3")) in
                a ^ "; " ^ b )
          in
          equal string "Outcome_unknown statement; 3" r ;
          (* The pool aborted the broken connection at its checkin. *)
          equal int 0 (Pool.Private.counters ()).stale_at_checkout ;
          equal string "query \"SLEEP 2000\" failed Outcome_unknown statement"
            (List.hd (queries events)) )
    ; test "a cancel in a statement frees the slot at once" (fun () ->
          let r, events =
            with_pool ~size:1 (fun _ _ pool ->
                Fiber.scope (fun s ->
                    let holder =
                      Fiber.fork_promise s (fun () ->
                          Fiber.with_timeout Mono (Span.ms 100) (fun () ->
                              Db.exec pool (q "SLEEP 10000") ) )
                    in
                    Time.sleep Mono (Span.ms 50) ;
                    let t0 = Time.now Mono in
                    let r =
                      result_text
                        (Db.exec ~timeout:(Span.s 5) pool (q "SELECT 7"))
                    in
                    let h =
                      match Promise.await holder with
                      | _ ->
                          "no timeout"
                      | exception Fiber.Timeout ->
                          "timeout"
                    in
                    Printf.sprintf "%s %s %b" h r (seconds t0 < 0.1) ) )
          in
          equal string "timeout 7 true" r ;
          equal string "query \"SLEEP 10000\" cancelled sent=true"
            (List.hd (queries events)) )
    ; test "a cancel while the fiber waits for a connection" (fun () ->
          let r, events =
            with_pool ~size:1 (fun _ _ pool ->
                Fiber.scope (fun s ->
                    Fiber.fork s (fun () ->
                        ignore (Db.exec pool (q "SLEEP 500")) ) ;
                    Time.sleep Mono (Span.ms 1) ;
                    match
                      Fiber.with_timeout Mono (Span.ms 100) (fun () ->
                          Db.exec pool (q "SELECT 1") )
                    with
                    | _ ->
                        "no timeout"
                    | exception Fiber.Timeout ->
                        "timeout" ) )
          in
          equal string "timeout" r ;
          is_true
            (List.mem "query \"SELECT 1\" cancelled sent=false" (queries events)) )
    ; test "an exec in a fiber that is already cancelled" (fun () ->
          let r, events =
            with_pool ~size:1 (fun _ db pool ->
                ignore (Db.exec pool (q "SELECT 1")) ;
                Pool.Private.reset_counters () ;
                let a =
                  match
                    Fiber.with_timeout Mono (Span.ms 1) (fun () ->
                        ( try Time.sleep Mono (Span.s 1)
                          with Fiber.Cancelled _ -> () ) ;
                        Db.exec pool (q "SELECT 2") )
                  with
                  | _ ->
                      "no cancel"
                  | exception Fiber.Timeout ->
                      "cancelled"
                in
                let b = result_text (Db.exec pool (q "SELECT 3")) in
                Printf.sprintf "%s %s %d %d" a b db.connects
                  (Pool.Private.counters ()).cancel_in_reset )
          in
          (* The idle entry stays good: no reset starts. *)
          equal string "cancelled 3 1 0" r ;
          is_true
            (List.mem "query \"SELECT 2\" cancelled sent=false" (queries events)) )
    ; test "an exception of Stmt.map holds no connection" (fun () ->
          let r, events =
            with_pool ~size:2 (fun _ _ pool ->
                for _ = 1 to 4 do
                  match
                    Db.exec pool
                      (Stmt.map (fun _ -> failwith "x") (q "SELECT 1"))
                  with
                  | _ ->
                      ()
                  | exception Failure _ ->
                      ()
                done ;
                let t0 = Time.now Mono in
                let r = result_text (Db.exec pool (q "SELECT 9")) in
                Printf.sprintf "%s %b" r (seconds t0 < 0.01) )
          in
          equal string "9 true" r ;
          (* The events say Done: the statement ran. *)
          equal int 4
            (List.length
               (List.filter
                  (( = ) "query \"SELECT 1\" done 1")
                  (queries events) ) ) )
    ; test "a hook that raises does not change the result" (fun () ->
          let r =
            D.run (fun _ _ ->
                Db.with_db ~pool_size:1
                  ~on_query:(function
                    | Ev.Query _ ->
                        raise (Fiber.Cancelled Exit)
                    | Ev.Connect _ ->
                        failwith "hook" )
                  (D.config ())
                  (fun pool -> result_text (Db.exec pool (q "SELECT 4"))) )
          in
          equal string "4" r )
    ; test "the end of with_db" (fun () ->
          (* One domain: the holder starts before the waiter, in the order of
             their forks. With more domains, the two can start in the other
             order, and the expected text changes. *)
          let r, events =
            D.run (fun _ db ->
                let events = ref [] in
                let waiter = ref "" and holder = ref "" in
                let pool_ref = ref None in
                Fiber.scope (fun s ->
                    Db.with_db ~pool_size:1
                      ~on_query:(fun e -> events := event_text e :: !events)
                      (D.config ())
                      (fun pool ->
                        pool_ref := Some pool ;
                        ignore (Db.exec pool (q "SELECT 1")) ;
                        Fiber.fork s (fun () ->
                            holder := result_text (Db.exec pool (q "SLEEP 100")) ) ;
                        Fiber.fork s (fun () ->
                            waiter := result_text (Db.exec pool (q "SELECT 2")) ) ;
                        Time.sleep Mono (Span.ms 10) ) ) ;
                (* Let the server see the end of the last connection. *)
                Time.sleep Mono (Span.ms 5) ;
                let after s =
                  match Db.exec (Option.get !pool_ref) s with
                  | _ ->
                      "no error"
                  | exception Invalid_argument m ->
                      m
                in
                (* A value with no literal: the misuse comes first. *)
                let after =
                  after (q "SELECT 3") ^ "; " ^ after (bad_literal ())
                in
                ( Printf.sprintf "%s; %s; %s; %d" !holder !waiter after
                    db.open_now
                , List.rev !events ) )
          in
          equal string
            "; Connection_lost; Db.t used after with_db; Db.t used after \
             with_db; 0"
            r ;
          is_true (List.mem "query \"SELECT 2\" failed Connection_lost" events) )
    ; test "no spin of the opener after a failed connect (C-C1)" (fun () ->
          let r, _ =
            with_pool ~size:1
              ~setup:(fun db -> db.faults.refuse <- true)
              (fun _ db pool ->
                ignore (Db.exec ~timeout:(Span.ms 50) pool (q "SELECT 1")) ;
                db.faults.refuse <- false ;
                Fiber.scope (fun s ->
                    Fiber.fork s (fun () ->
                        ignore (Db.exec pool (q "SLEEP 300")) ) ;
                    Time.sleep Mono (Span.ms 200) ;
                    result_text (Db.exec pool (q "SELECT 2")) ) )
          in
          equal string "2" r )
    ; test "at most 2 connects at a time" (fun () ->
          let r, _ =
            with_pool ~size:8
              ~setup:(fun db -> db.faults.latency_ms <- 0)
              (fun _ db pool ->
                Fiber.scope (fun s ->
                    for _ = 1 to 8 do
                      Fiber.fork s (fun () ->
                          ignore (Db.exec pool (q "SLEEP 100")) )
                    done ) ;
                Printf.sprintf "%d %d" db.handshaking_max db.open_max )
          in
          equal string "2 8" r )
    ; test "the defaults: 10 connections" (fun () ->
          let r =
            D.run (fun _ db ->
                Db.with_db (D.config ()) (fun pool ->
                    Fiber.scope (fun s ->
                        for _ = 1 to 12 do
                          Fiber.fork s (fun () ->
                              ignore (Db.exec pool (q "SLEEP 100")) )
                        done ) ) ;
                db.open_max )
          in
          equal int 10 r )
    ; test "the defaults: a checkout limit of 30 s" (fun () ->
          let r =
            D.run (fun _ db ->
                db.faults.hang_greeting <- true ;
                Db.with_db (D.config ()) (fun pool ->
                    let t0 = Time.now Mono in
                    let r = Db.exec ~timeout:(Span.s 60) pool (q "SELECT 1") in
                    Printf.sprintf "%s %.3f" (result_text r) (seconds t0) ) )
          in
          equal string "Connect timeout 30.000" r )
    ; test "the defaults: a timeout of 30 s" (fun () ->
          let r =
            D.run (fun _ _ ->
                Db.with_db (D.config ()) (fun pool ->
                    let t0 = Time.now Mono in
                    let r = Db.exec pool (q "SLEEP 40000") in
                    Printf.sprintf "%s %.3f" (result_text r) (seconds t0) ) )
          in
          equal string "Outcome_unknown statement 30.000" r )
    ; test "the defaults: a life of 1800 s, with a jitter" (fun () ->
          let r =
            D.run (fun _ _ ->
                Db.with_db ~random:(Random.State.make [|7|]) (D.config ())
                  (fun pool ->
                    ignore (Db.exec pool (q "SELECT 1")) ;
                    Pool.Private.idle_expiry (Db.Private.pool pool) ) )
          in
          (* The pool splits its random state, then draws the jitter of the
             first open. *)
          let u =
            Random.State.float
              (Random.State.split (Random.State.make [|7|]))
              0.3
            -. 0.15
          in
          let life = Span.to_ns (Span.s 1800) in
          let expected =
            Int64.add life (Int64.of_float (Int64.to_float life *. u))
          in
          equal
            (list (option int64))
            [Some expected]
            (List.map (Option.map Span.to_ns) r) )
    ; test "a checkout_timeout of 0: Invalid_argument" (fun () ->
          let r =
            D.run (fun _ _ ->
                match
                  Db.with_db ~checkout_timeout:Span.zero (D.config ()) (fun _ ->
                      () )
                with
                | () ->
                    "no error"
                | exception Invalid_argument m ->
                    m )
          in
          equal string "Db.with_db: checkout_timeout must be positive" r )
    ; test "an idle entry past its life: a polite close at the checkout"
        (fun () ->
          let (r, db), _ =
            with_pool ~size:1 ~max_lifetime:(Span.ms 100) (fun _ db pool ->
                ignore (Db.exec pool (q "SELECT 1")) ;
                Time.sleep Mono (Span.ms 200) ;
                Pool.Private.reset_counters () ;
                let r = result_text (Db.exec pool (q "SELECT 2")) in
                ( Printf.sprintf "%s %d %d" r db.connects
                    (Pool.Private.counters ()).expired_at_checkout
                , db ) )
          in
          equal string "2 2 1" r ;
          (* One QUIT from the opener, one from the end of with_db. *)
          equal int 2 db.quits )
    ; cases "the margin of 50 ms at the checkout"
        ~id:(fun (n, _, _) -> n)
        [("50 ms left", 951, "2"); ("49 ms left", 952, "Timeout")]
        (fun (_, sleep, expected) ->
          let r, _ =
            with_pool ~size:1 (fun _ _ pool ->
                ignore (Db.exec pool (q "SELECT 1")) ;
                Fiber.scope (fun s ->
                    Fiber.fork s (fun () ->
                        ignore
                          (Db.exec pool (q (Printf.sprintf "SLEEP %d" sleep))) ) ;
                    Time.sleep Mono (Span.ms 1) ;
                    result_text
                      (Db.exec ~timeout:(Span.ms 1000) pool (q "SELECT 2")) ) )
          in
          equal string expected r )
    ; test "two expired entries, and a second checkout before the opener"
        (fun () ->
          let r, _ =
            with_pool ~size:2 ~max_lifetime:(Span.ms 100) (fun _ _ pool ->
                let two () =
                  Fiber.scope (fun s ->
                      for _ = 1 to 2 do
                        Fiber.fork s (fun () ->
                            ignore (Db.exec pool (q "SLEEP 10")) )
                      done )
                in
                two () ;
                Time.sleep Mono (Span.ms 200) ;
                Pool.Private.reset_counters () ;
                (* The seam holds the doorbell back: the second checkout sees
                   the two entries of the first one in the closing slots. *)
                (Pool.Private.after_op :=
                   fun () ->
                     Fiber.shield (fun () -> Time.sleep Mono (Span.ms 1)) ) ;
                Fun.protect
                  ~finally:(fun () -> Pool.Private.after_op := ignore)
                  two ;
                (Pool.Private.counters ()).expired_at_checkout )
          in
          equal int 2 r )
    ; test "the end of with_db during a connect" (fun () ->
          let r, _ =
            with_pool ~size:1
              ~setup:(fun db -> db.faults.hang_greeting <- true)
              (fun _ db pool ->
                let r =
                  result_text
                    (Db.exec ~timeout:(Span.ms 100) pool (q "SELECT 1"))
                in
                Printf.sprintf "%s %d" r db.handshaking )
          in
          (* The cancel of the opener ends the connect: no slot stays in the
             opening state. *)
          equal string "Timeout 1" r )
    ; test "a statement that sent nothing: a cancel tells sent=false" (fun () ->
          let r, events =
            with_pool ~size:1 (fun _ _ pool ->
                match
                  Fiber.with_timeout Mono (Span.ms 10) (fun () ->
                      Db.exec pool
                        (custom (fun _ ->
                             Time.sleep Mono (Span.s 1) ;
                             Ok () ) ) )
                with
                | _ ->
                    "no timeout"
                | exception Fiber.Timeout ->
                    "timeout" )
          in
          equal string "timeout" r ;
          equal (list string)
            ["query \"CUSTOM\" cancelled sent=false"]
            (queries events) )
    ; test "a cancel after the first command of a statement: sent=true"
        (fun () ->
          let r, events =
            with_pool ~size:1 (fun _ _ pool ->
                let run ctx =
                  ignore (Cistern.Private.Exec.exec ctx (Raw.sql "SELECT 1")) ;
                  (try Time.sleep Mono (Span.s 1) with Fiber.Cancelled _ -> ()) ;
                  (* The fiber is cancelled: this command sends nothing. *)
                  ignore (Cistern.Private.Exec.exec ctx (Raw.sql "SELECT 2")) ;
                  Ok ()
                in
                match
                  Fiber.with_timeout Mono (Span.ms 10) (fun () ->
                      Db.exec pool (custom run) )
                with
                | _ ->
                    "no timeout"
                | exception Fiber.Timeout ->
                    "timeout" )
          in
          equal string "timeout" r ;
          equal (list string)
            ["query \"CUSTOM\" cancelled sent=true"]
            (queries events) )
    ; test "a cancel in the session reset" (fun () ->
          let r, events =
            with_pool ~size:1 (fun _ db pool ->
                ignore (Db.exec pool (q "SELECT 1")) ;
                db.faults.reset_ms <- 1000 ;
                Pool.Private.reset_counters () ;
                let a =
                  match
                    Fiber.with_timeout Mono (Span.ms 10) (fun () ->
                        Db.exec pool (q "SELECT 2") )
                  with
                  | _ ->
                      "no timeout"
                  | exception Fiber.Timeout ->
                      "timeout"
                in
                db.faults.reset_ms <- 0 ;
                let b = result_text (Db.exec pool (q "SELECT 3")) in
                Printf.sprintf "%s %s %d %d" a b db.connects
                  (Pool.Private.counters ()).cancel_in_reset )
          in
          equal string "timeout 3 2 1" r ;
          is_true
            (List.mem "query \"SELECT 2\" cancelled sent=false" (queries events)) )
    ; test "a session reset past the deadline: Timeout" (fun () ->
          let r, _ =
            with_pool ~size:1 (fun _ db pool ->
                ignore (Db.exec pool (q "SELECT 1")) ;
                db.faults.reset_ms <- 1000 ;
                let a =
                  result_text
                    (Db.exec ~timeout:(Span.ms 200) pool (q "SELECT 2"))
                in
                db.faults.reset_ms <- 0 ;
                let b = result_text (Db.exec pool (q "SELECT 3")) in
                Printf.sprintf "%s %s %d" a b db.connects )
          in
          equal string "Timeout 3 2" r )
    ; test "a slow session reset leaves less than the margin: Timeout"
        (fun () ->
          let r, _ =
            with_pool ~size:1 (fun _ db pool ->
                ignore (Db.exec pool (q "SELECT 1")) ;
                db.faults.reset_ms <- 120 ;
                let a =
                  result_text
                    (Db.exec ~timeout:(Span.ms 150) pool (q "SELECT 2"))
                in
                db.faults.reset_ms <- 0 ;
                let resets = db.resets in
                ignore (Db.exec pool (clean ())) ;
                let b = result_text (Db.exec pool (q "SELECT 3")) in
                Printf.sprintf "%s %b %d %s %d" a
                  (List.mem "SELECT 2" db.log)
                  (db.resets - resets) b db.connects )
          in
          (* After the reset, 30 ms are left: the statement does not go, and the
             entry stays good. The reset made it clean: the next statement that
             is not raw needs no reset. *)
          equal string "Timeout false 0 3 1" r )
    ; test "a successful reset makes the entry clean" (fun () ->
          let r, _ =
            with_pool ~size:1 (fun _ db pool ->
                ignore (Db.exec pool (q "SELECT 1")) ;
                ignore (Db.exec pool (clean ())) ;
                ignore (Db.exec pool (clean ())) ;
                db.resets )
          in
          (* The raw statement made the entry dirty: one reset, then none. *)
          equal int 1 r )
    ; test "a timeout of Int64.max_int ns: no overflow" (fun () ->
          let r, _ =
            with_pool (fun _ _ pool ->
                (* The start of the exec is after the origin of the pool, thus
                   the sum overflows. *)
                Time.sleep Mono (Span.ms 1) ;
                result_text
                  (Db.exec ~timeout:(Span.of_ns Int64.max_int) pool
                     (q "SELECT 4") ) )
          in
          equal string "4" r )
    ; test "an idle connection gets a polite close at the end" (fun () ->
          let db, _ =
            with_pool ~size:1 (fun _ db pool ->
                ignore (Db.exec ~timeout:(Span.ms 100) pool (q "SELECT 1")) ;
                Time.sleep Mono (Span.s 1) ;
                db )
          in
          equal int 1 db.quits )
    ; test "an exception of f: the end of with_db" (fun () ->
          let r =
            D.run (fun _ db ->
                let last = ref None in
                ( match
                    Db.with_db (D.config ()) (fun pool ->
                        last := Some pool ;
                        ignore (Db.exec pool (q "SELECT 1")) ;
                        raise Exit )
                  with
                | () ->
                    ()
                | exception Exit ->
                    () ) ;
                check_end (Option.get !last) ;
                Time.sleep Mono (Span.ms 1) ;
                db.quits )
          in
          equal int 1 r )
    ; test "Stmt.to_sql of a raw statement" (fun () ->
          equal
            (list (list string))
            [["SELECT 1"]; ["<Invalid_value: a NaN is not a SQL value>"]]
            [ Stmt.to_sql Cistern_core.Dialect.Mysql (q "SELECT 1")
            ; Stmt.to_sql Cistern_core.Dialect.Mysql (bad_literal ()) ] )
    ; cases "the races of a hand-off and an abandon, forced by the latch"
        ~id:(fun (n, _, _, _) -> n)
        [ (* H gives the entry back at 3 ms, and the latch holds H until 7 ms.
             The limit of W fires at 6 ms, and W abandons first. *)
          ("checkin_lost", [7], Span.s 30, "\"\" Pool_exhausted 0 1")
        ; (* Then the latch also holds W in its abandon until 8 ms: H fills
             first, and W finds the entry. Its deadline leaves the margin, thus
             W uses the entry. *)
          ("waiter_lost, time left", [7; 8], Span.s 30, "\"\" 2 1 0")
        ; (* The same, but at 8 ms the deadline of W (57 ms) leaves 49 ms: W
             gives the entry back and its error. *)
          ( "waiter_lost, no time left"
          , [7; 8]
          , Span.ms 56
          , "\"\" Pool_exhausted 1 0" ) ]
        (fun (_, ats, w_timeout, expected) ->
          Pool.Private.reset_counters () ;
          let r, _ =
            with_pool ~size:1 ~checkout_timeout:(Span.ms 5) (fun _ _ pool ->
                ignore (Db.exec pool (q "SELECT 1")) ;
                let t0 = Time.now Mono in
                Fun.protect
                  ~finally:(fun () -> Pool.Private.after_op := ignore)
                  (fun () ->
                    Fiber.scope (fun s ->
                        let h =
                          Fiber.fork_promise s (fun () ->
                              result_text (Db.exec pool (q "SLEEP 3")) )
                        in
                        Time.sleep Mono (Span.ms 1) ;
                        let w =
                          Fiber.fork_promise s (fun () ->
                              result_text
                                (Db.exec ~timeout:w_timeout pool (q "SELECT 2")) )
                        in
                        Time.sleep Mono (Span.ms 1) ;
                        latch t0 ats ;
                        let h = Promise.await h and w = Promise.await w in
                        let c = Pool.Private.counters () in
                        Printf.sprintf "%S %s %d %d" h w c.waiter_lost
                          c.checkin_lost ) ) )
          in
          equal string expected r ;
          Pool.Private.reset_counters () ;
          let c = Pool.Private.counters () in
          equal ~msg:"the counters after a reset" (list int) [0; 0; 0; 0; 0]
            [ c.waiter_lost
            ; c.checkin_lost
            ; c.cancel_in_reset
            ; c.expired_at_checkout
            ; c.stale_at_checkout ] )
    ; test "a cancelled waiter finds a late entry: it goes back" (fun () ->
          (* H gives the entry back at 3 ms, and the latch holds H until 7 ms. W
             is cancelled at 6 ms, and the latch holds W in its abandon until 8
             ms. H fills first. W gives the entry back with no I/O and raises
             Cancelled. The next exec uses the same connection. *)
          Pool.Private.reset_counters () ;
          let r, _ =
            with_pool ~size:1 (fun _ db pool ->
                ignore (Db.exec pool (q "SELECT 1")) ;
                let t0 = Time.now Mono in
                Fun.protect
                  ~finally:(fun () -> Pool.Private.after_op := ignore)
                  (fun () ->
                    let w =
                      Fiber.scope (fun s ->
                          Fiber.fork s (fun () ->
                              ignore (Db.exec pool (q "SLEEP 3")) ) ;
                          Time.sleep Mono (Span.ms 1) ;
                          let w =
                            Fiber.fork_promise s (fun () ->
                                match
                                  Fiber.with_timeout Mono (Span.ms 5) (fun () ->
                                      Db.exec pool (q "SELECT 2") )
                                with
                                | r ->
                                    result_text r
                                | exception Fiber.Timeout ->
                                    "cancelled" )
                          in
                          Time.sleep Mono (Span.ms 1) ;
                          latch t0 [7; 8] ;
                          Promise.await w )
                    in
                    let next = result_text (Db.exec pool (q "SELECT 3")) in
                    Printf.sprintf "%s %d %s %d" w
                      (Pool.Private.counters ()).waiter_lost next db.connects ) )
          in
          equal string "cancelled 1 3 1" r )
    ; test "the limit of a waiter races the Failed fill of the shutdown"
        (fun () ->
          (* The shutdown takes W from the queue at 2 ms, and the latch holds it
             until 7 ms. The limit of W fires at 6 ms, and the latch holds W in
             its abandon until 8 ms. The shutdown fills first: W gets
             Connection_lost, not its own error. *)
          let r =
            D.run (fun _ _ ->
                let w = ref "" and h = ref "" and last = ref None in
                Fiber.scope (fun s ->
                    Db.with_db ~pool_size:1 ~checkout_timeout:(Span.ms 5)
                      (D.config ()) (fun pool ->
                        last := Some pool ;
                        let t0 = Time.now Mono in
                        Fiber.fork s (fun () ->
                            h := result_text (Db.exec pool (q "SLEEP 20")) ) ;
                        Time.sleep Mono (Span.ms 1) ;
                        Fiber.fork s (fun () ->
                            w := result_text (Db.exec pool (q "SELECT 2")) ) ;
                        Time.sleep Mono (Span.ms 1) ;
                        latch t0 [7; 8] ) ;
                    Pool.Private.after_op := ignore ) ;
                check_end (Option.get !last) ;
                Printf.sprintf "%S %s" !h !w )
          in
          equal string "\"\" Connection_lost" r )
    ; test "a statement that raises: the entry is discarded" (fun () ->
          let r, _ =
            with_pool ~size:1 (fun _ db pool ->
                let run ctx =
                  ignore (Cistern.Private.Exec.exec ctx (Raw.sql "SELECT 1")) ;
                  failwith "a bug"
                in
                let a =
                  match Db.exec pool (custom run) with
                  | _ ->
                      "no exception"
                  | exception Failure m ->
                      m
                in
                let b = result_text (Db.exec pool (q "SELECT 2")) in
                Printf.sprintf "%s; %s; %d" a b db.connects )
          in
          (* The slot comes back at once, with a new connection. *)
          equal string "a bug; 2; 2" r )
    ; test "Raw.exec: the affected rows and the insert id" (fun () ->
          let r, events =
            with_pool (fun _ _ pool ->
                match Db.exec pool (Raw.exec (Raw.sql "INSERT x")) with
                | Ok (x : Cistern_core.Exec_result.t) ->
                    Printf.sprintf "%Ld %Ld" x.affected_rows x.last_insert_id
                | Error e ->
                    error_text e )
          in
          equal string "2 7" r ;
          equal (list string) ["query \"INSERT x\" done 2"] (queries events) )
    ; test "the Query event comes after the checkin" (fun () ->
          let seen = ref [] and pr = ref None in
          D.run (fun _ _ ->
              Db.with_db ~pool_size:1
                ~on_query:(function
                  | Ev.Query _ ->
                      let _, in_use, _, _, _ =
                        Pool.Private.state (Option.get !pr)
                      in
                      seen := in_use :: !seen
                  | Ev.Connect _ ->
                      () )
                (D.config ())
                (fun pool ->
                  pr := Some (Db.Private.pool pool) ;
                  ignore (Db.exec pool (q "SELECT 1")) ;
                  ignore (Db.exec pool (q "FAIL 1062")) ) ) ;
          (* A hook never sees the slot of its own statement in use. *)
          equal (list int) [0; 0] !seen )
    ; test "a checkin fills the waiter before the doorbell" (fun () ->
          (* The connects fail, thus the backoff gives a timer, and each
             operation wakes the opener while W2 waits. H gives its entry to W1:
             the fill comes before the doorbell. *)
          let r, _ =
            with_pool ~size:2 (fun _ db pool ->
                ignore (Db.exec pool (q "SELECT 1")) ;
                db.faults.refuse <- true ;
                let trace = ref [] in
                Fiber.scope (fun s ->
                    Fiber.fork s (fun () ->
                        ignore (Db.exec pool (q "SLEEP 50")) ) ;
                    Time.sleep Mono (Span.ms 1) ;
                    Fiber.fork s (fun () ->
                        ignore (Db.exec pool (q "SELECT 2")) ) ;
                    Fiber.fork s (fun () ->
                        ignore (Db.exec pool (q "SELECT 3")) ) ;
                    Time.sleep Mono (Span.ms 40) ;
                    Pool.Private.trace := fun x -> trace := x :: !trace ) ;
                Pool.Private.trace := ignore ;
                db.faults.refuse <- false ;
                String.concat " "
                  (List.filteri (fun i _ -> i < 2) (List.rev !trace)) )
          in
          equal string "fill ring" r )
    ; test "an outer cancel of with_db: the polite closes run in the shield"
        (fun () ->
          let r =
            D.run (fun _ db ->
                let last = ref None in
                let a =
                  match
                    Fiber.with_timeout Mono (Span.ms 50) (fun () ->
                        Db.with_db (D.config ()) (fun pool ->
                            last := Some pool ;
                            (* Two idle connections. *)
                            Fiber.scope (fun s ->
                                for _ = 1 to 2 do
                                  Fiber.fork s (fun () ->
                                      ignore (Db.exec pool (q "SLEEP 5")) )
                                done ) ;
                            Time.sleep Mono (Span.s 10) ) )
                  with
                  | () ->
                      "no timeout"
                  | exception Fiber.Timeout ->
                      "timeout"
                in
                check_end (Option.get !last) ;
                Time.sleep Mono (Span.ms 1) ;
                Printf.sprintf "%s %d" a db.quits )
          in
          (* Each idle connection got its COM_QUIT, also in the cancel. *)
          equal string "timeout 2" r )
    ; test "a cancel races the Failed fill of the shutdown: Cancelled"
        (fun () ->
          (* As the test of the limit, but a cancel at 6 ms in place of the
             limit. The shutdown fills Failed first, and the cancel goes up. *)
          let r, events =
            D.run (fun _ _ ->
                let w = ref "" and events = ref [] and last = ref None in
                Fiber.scope (fun s ->
                    Db.with_db ~pool_size:1
                      ~on_query:(fun e -> events := event_text e :: !events)
                      (D.config ())
                      (fun pool ->
                        last := Some pool ;
                        let t0 = Time.now Mono in
                        Fiber.fork s (fun () ->
                            ignore (Db.exec pool (q "SLEEP 20")) ) ;
                        Time.sleep Mono (Span.ms 1) ;
                        Fiber.fork s (fun () ->
                            w :=
                              match
                                Fiber.with_timeout Mono (Span.ms 5) (fun () ->
                                    Db.exec pool (q "SELECT 2") )
                              with
                              | r ->
                                  result_text r
                              | exception Fiber.Timeout ->
                                  "cancelled" ) ;
                        Time.sleep Mono (Span.ms 1) ;
                        latch t0 [7; 8] ) ;
                    Pool.Private.after_op := ignore ) ;
                check_end (Option.get !last) ;
                (!w, List.rev !events) )
          in
          equal string "cancelled" r ;
          is_true (List.mem "query \"SELECT 2\" cancelled sent=false" events) )
    ; test "a broken entry is aborted before its slot is free" (fun () ->
          (* H breaks its connection (a read deadline) at 100 ms. The latch
             holds the checkin of H until 102 ms. W2 comes at 101 ms and wakes
             the opener, whose connect ends at once. The abort came before the
             checkin, thus no open finds more sockets than the size. *)
          let r, _ =
            with_pool ~size:1 (fun _ _ pool ->
                let t0 = Time.now Mono in
                Fun.protect
                  ~finally:(fun () -> Pool.Private.after_op := ignore)
                  (fun () ->
                    Fiber.scope (fun s ->
                        let h =
                          Fiber.fork_promise s (fun () ->
                              result_text
                                (Db.exec ~timeout:(Span.ms 100) pool
                                   (q "SLEEP 1000") ) )
                        in
                        Time.sleep Mono (Span.ms 10) ;
                        let w1 =
                          Fiber.fork_promise s (fun () ->
                              result_text (Db.exec pool (q "SELECT 1")) )
                        in
                        Time.sleep Mono (Span.ms 80) ;
                        latch t0 [102] ;
                        Time.sleep Mono (Span.ms 11) ;
                        let w2 =
                          Fiber.fork_promise s (fun () ->
                              result_text (Db.exec pool (q "SELECT 2")) )
                        in
                        String.concat " " (List.map Promise.await [h; w1; w2]) ) ) )
          in
          equal string "Outcome_unknown statement 1 2" r )
    ; test "a server that never reads COM_QUIT: the shutdown ends at 2 s"
        (fun () ->
          (* 4 idle connections whose server reads nothing: each polite close
             waits 1 s (the write timeout of Connection.close). The limit of 2 s
             cuts the second or the third close (a close that raises), and the
             aborts after it close the rest. *)
          let r =
            D.run (fun _ db ->
                let last = ref None and t_f = ref (Time.now Mono) in
                Db.with_db ~pool_size:4 (D.config ()) (fun pool ->
                    last := Some pool ;
                    Fiber.scope (fun s ->
                        for _ = 1 to 4 do
                          Fiber.fork s (fun () ->
                              ignore (Db.exec pool (deaf ~hold:(Span.ms 20))) )
                        done ) ;
                    t_f := Time.now Mono ) ;
                let d = seconds !t_f in
                check_end (Option.get !last) ;
                (* The server closes its side 3 s after DEAF. *)
                Time.sleep Mono (Span.s 2) ;
                Printf.sprintf "%.3f %d" d db.quits )
          in
          equal string "2.000 0" r )
    ; test "the shutdown fills its waiters before its polite closes" (fun () ->
          (* X and Y are deaf connections (their server reads nothing), with a
             life of about 50 ms. X is idle. At 60 ms, the checkout of H moves X
             to close, and the opener waits 1 s in its polite close. Y comes
             back at 70 ms past its life: it waits in "to close", because the
             opener is busy. W waits from 80 ms. At 90 ms with_db ends: the
             cancel of the opener cuts the close of X, and the shutdown gets Y
             and W. W gets Connection_lost at once, before the polite close of Y
             (1 s). *)
          let r =
            D.run (fun _ _ ->
                let w = ref "" and t_w = ref None and t_f = ref None in
                let last = ref None in
                Fiber.scope (fun s ->
                    Db.with_db ~pool_size:3 ~max_lifetime:(Span.ms 50)
                      (D.config ()) (fun pool ->
                        last := Some pool ;
                        Fiber.fork s (fun () ->
                            ignore (Db.exec pool (deaf ~hold:Span.zero)) ) ;
                        Fiber.fork s (fun () ->
                            ignore (Db.exec pool (deaf ~hold:(Span.ms 65))) ) ;
                        Time.sleep Mono (Span.ms 60) ;
                        Fiber.fork s (fun () ->
                            ignore (Db.exec pool (q "SLEEP 2000")) ) ;
                        Time.sleep Mono (Span.ms 20) ;
                        Fiber.fork s (fun () ->
                            w := result_text (Db.exec pool (q "SELECT 2")) ;
                            t_w := Some (Time.now Mono) ) ;
                        Time.sleep Mono (Span.ms 10) ;
                        t_f := Some (Time.now Mono) ) ) ;
                check_end (Option.get !last) ;
                let d =
                  Int64.to_float
                    (Span.to_ns
                       (Time.diff Mono (Option.get !t_w) (Option.get !t_f)) )
                  /. 1e9
                in
                (* The servers close their side 3 s after DEAF. *)
                Time.sleep Mono (Span.s 2) ;
                Printf.sprintf "%s %b" !w (d < 0.1) )
          in
          equal string "Connection_lost true" r )
    ; test "the opener starts its connects before its polite closes" (fun () ->
          (* D is idle and past its life, and its server reads nothing: its
             polite close waits 1 s. W takes the other slot with a new connect,
             before the close of D ends. *)
          let r, _ =
            with_pool ~size:2 ~max_lifetime:(Span.ms 50)
              ~checkout_timeout:(Span.ms 100) (fun _ _ pool ->
                ignore (Db.exec pool (deaf ~hold:Span.zero)) ;
                Time.sleep Mono (Span.ms 100) ;
                let t0 = Time.now Mono in
                let w = result_text (Db.exec pool (q "SELECT 2")) in
                let fast = seconds t0 < 0.05 in
                Time.sleep Mono (Span.s 3) ;
                Printf.sprintf "%s %b" w fast )
          in
          equal string "2 true" r ) ]
