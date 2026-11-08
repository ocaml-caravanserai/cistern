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
  | (Ev.Kill _ | Ev.Kill_dropped _ | Ev.Kill_off _) as e ->
      Format.asprintf "%a" Ev.pp e

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
    [idle; in_use; opening; closing; waiting] ;
  (* Phase 1.5c1a: no control connection, no Killing slot and no entry of the
     kill queue. *)
  equal ~msg:"the control connections at the end" int 0 (Pool.Private.control p) ;
  equal ~msg:"the Killing slots at the end" int 0 (Pool.Private.killing p) ;
  equal ~msg:"the kill queue at the end" (list int64) [] (Pool.Private.queue p)

(* [with_pool ?size ... f]: a pool on the fake server, with the events in a
   list. After with_db, it does the checks of [check_end]. With [tls] (phase
   1.4), the fake server runs TLS, and the pool uses VERIFY_IDENTITY with the
   test CA: the generator is seeded again at the start, and the simulation has
   the time 2025 and the CA files of Tls_fixture. *)
let with_pool ?(size = 2) ?max_lifetime ?checkout_timeout ?(seed = 0)
    ?(domains = 1) ?(setup = fun _ -> ()) ?(tls = false) ?(kill = false) f =
  let events = ref [] in
  if tls then Tls_fixture.seed () ;
  let result =
    D.run ~seed ~domains (fun sim db ->
        let config =
          if tls then (
            Tls_fixture.at_2025 sim ;
            Tls_fixture.install () ;
            db.faults.tls <- Some (Tls_fixture.server_config Tls_fixture.pki) ;
            Tls_fixture.config ~kill ~host:"10.0.0.9" ~ca:Tls_fixture.ca_file
              Verify_identity )
          else D.config ~kill ()
        in
        setup db ;
        let last = ref None in
        let r =
          Db.with_db ~pool_size:size ?max_lifetime ?checkout_timeout
            ~random:(Random.State.make [|seed|])
            ~on_query:(fun e -> events := event_text e :: !events)
            config
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
    ; text= Some (Ok "CUSTOM")
    ; for_update= false
    ; atomic= false
    ; multi= false }
    ~run
    ~rows:(fun _ -> 0)
    ~finish:Fun.id

(* An atomic statement of the tests (phase 4b1): its commands in order, each
   with the redacted text "atomic: <command>", until the first error. Its value
   is the count of the commands. *)
let atomic ?(around = fun _ f -> f ()) ?text cmds =
  let redacted c = "atomic: " ^ c in
  (* "QUERY sql" runs Exec.query, "ROWS sql" Exec.rows, another text
     Exec.exec. *)
  let command ctx c =
    let redacted = redacted c in
    let after prefix =
      String.sub c (String.length prefix)
        (String.length c - String.length prefix)
    in
    if String.starts_with ~prefix:"QUERY " c then
      Result.map ignore
        (Cistern.Private.Exec.query ~redacted ctx (Raw.sql (after "QUERY ")))
    else if String.starts_with ~prefix:"ROWS " c then
      Result.map ignore
        (Cistern.Private.Exec.rows ~redacted ctx
           (Raw.sql (after "ROWS "))
           (fun _ -> Ok ()) )
    else Result.map ignore (Cistern.Private.Exec.exec ~redacted ctx (Raw.sql c))
  in
  Stmt.Private.make
    { redacted= (fun _ -> List.map redacted cmds)
    ; kind= Stmt.Private.Write
    ; raw= None
    ; sql= (fun _ -> cmds)
    ; text
    ; for_update= false
    ; atomic= true
    ; multi= false }
    ~run:(fun ctx ->
      List.fold_left
        (fun acc c ->
          Result.bind acc (fun n ->
              Result.map (fun () -> n + 1) (around c (fun () -> command ctx c)) ) )
        (Ok 0) cmds )
    ~rows:Fun.id ~finish:Fun.id

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
    ; text= None
    ; for_update= false
    ; atomic= false
    ; multi= false }
    ~run:(fun ctx ->
      match Cistern.Private.Exec.exec ctx (Raw.sql "DEAF") with
      | Error e ->
          Error e
      | Ok _ ->
          let r = Cistern.Private.Exec.exec ctx (Raw.sql fill) in
          Time.sleep Mono hold ; r )
    ~rows:(fun _ -> 0)
    ~finish:ignore

(* The storm (plan of phase 1.2, section 4.5): [fibers] fibers on 4 virtual
   domains and a pool of 8, with raw statements (dirty entries and resets),
   BEGIN (an abort at checkin), server errors, short deadlines, cancels, a short
   max_lifetime and 4031 faults. Each fiber starts at a random time in one of
   three waves and records one outcome. Then, when the pool is quiet, 8 fibers
   must each get a connection at once. It gives the outcomes, sorted. *)
(* Connection_lost: the server of the idle fault (4031 after 25 ms) can send
   its 4031 between the stale check of a connection and the read of the
   statement, on another virtual domain at the same time. The ERR has the
   sequence id 0 (rule R-SEQ0): the statement did not run. Round trip 3 of PR
   1.5b changed the schedule, and seed 3 of the slow storm reaches it. *)
let permitted =
  [ "ok"
  ; "cancelled"
  ; "Timeout"
  ; "Pool_exhausted"
  ; "Outcome_unknown statement"
  ; "Connection_lost"
  ; "Constraint_violation Unique" ]

(* While it is set, the killer cannot open its control connection (the seam
   Pool.Private.connect_control of a storm with KILL): only the control
   connection is refused. *)
let refuse_control = Atomic.make false

let with_refuse_control f =
  (Pool.Private.connect_control :=
     fun c ->
       if Atomic.get refuse_control then
         Error (Cistern_core.Error.Connect "refused")
       else Cistern_mysql.Connection.connect c ) ;
  Fun.protect
    ~finally:(fun () ->
      Atomic.set refuse_control false ;
      Pool.Private.connect_control := Cistern_mysql.Connection.connect )
    f

(* The storms with KILL (phase 1.5c2). [kill_faults db ~seed ~stop ~sample]:
   until [stop ()], every 10 to 30 ms one fault of KILL: - for 5 ms: a late
   reply (300 ms), a reply past the limit of a try (2.5 s), or the end of the
   control connection at its next KILL; - for 5 s: replies past the limit, thus
   the two tries of an entry fail (Kill_dropped Failed); - for 50 ms: the end of
   the control connection at its next KILL, and the refusal of its reopen, only
   of the control connection (the killer waits for its backoff); - for 30 ms:
   another server identity for the new connections, thus an entry and the
   control connection can name two servers (Kill_dropped Other_server). [sample]
   runs at each turn. *)
let kill_faults (db : D.t) ~seed ~stop ~sample =
  let rs = Random.State.make [|seed; -1|] in
  while not (stop ()) do
    Time.sleep Mono (Span.ms (10 + Random.State.int rs 20)) ;
    sample () ;
    let hold =
      match Random.State.int rs 8 with
      | 0 ->
          db.faults.kill_ms <- 300 ;
          5
      | 1 ->
          db.faults.kill_ms <- 2500 ;
          5
      | 2 ->
          db.faults.kill_close <- 1 ;
          5
      | 3 ->
          db.faults.kill_ms <- 2500 ;
          5000
      | 4 ->
          db.faults.kill_close <- 1 ;
          Atomic.set refuse_control true ;
          50
      | 5 ->
          db.faults.server_uuid <- "another-server" ;
          30
      | _ ->
          5
    in
    Time.sleep Mono (Span.ms hold) ;
    db.faults.kill_ms <- 0 ;
    db.faults.kill_close <- 0 ;
    Atomic.set refuse_control false ;
    db.faults.server_uuid <- "5c607771-db0a-11f1-86e8-e86538021fcd"
  done

(* The fibers of the kills in a storm with KILL: [fibers / 50] statements of 60
   ms ("SLEEP 60"), between the waves (at 35 to 55 ms and at 100 to 190 ms),
   each cancelled after 5 to 40 ms while it runs. Thus each seed has kill
   requests. *)
let kill_fibers s pool ~seed ~fibers =
  let n = fibers / 50 in
  for i = 1 to n do
    Fiber.fork s (fun () ->
        let rs = Random.State.make [|seed; -2; i|] in
        let at =
          if Random.State.bool rs then 35 + Random.State.int rs 20
          else 100 + Random.State.int rs 90
        in
        Time.sleep Mono (Span.ms at) ;
        match
          Fiber.with_timeout Mono
            (Span.ms (5 + Random.State.int rs 35))
            (fun () -> Db.exec ~timeout:(Span.s 1) pool (q "SLEEP 60"))
        with
        | _ ->
            ()
        | exception Fiber.Timeout ->
            () )
  done ;
  n

(* In the seeds with [seed mod 4 = 3], each KILL gives 1095: KILL goes off for
   the pool at the first one (Kill_off). *)
let not_owner_seed seed = seed mod 4 = 3

(* Wait until the killer ended its work: no Killing slot and no entry (at most
   20 s, the time of the late replies). *)
let wait_killer p =
  let n = ref 0 in
  while
    (Pool.Private.killing p > 0 || Pool.Private.queue p <> []) && !n < 2000
  do
    Time.sleep Mono (Span.ms 10) ;
    incr n
  done

(* The kill events: "kill <duration> <id> <outcome>" and "kill dropped ...", not
   "kill off". *)
let kill_events events =
  List.filter
    (fun e ->
      String.starts_with ~prefix:"kill " e
      && not (String.starts_with ~prefix:"kill off" e) )
    events

let kills events =
  List.length
    (List.filter
       (fun e -> not (String.starts_with ~prefix:"kill dropped" e))
       (kill_events events) )

type kill_record =
  { requests: int
  ; events: int
  ; kills: int
  ; control_max: int
  ; off: bool
  ; offs: int  (** The Kill_off events. *)
  ; failed: int  (** The Kill_dropped Failed events. *)
  ; other: int  (** The Kill_dropped Other_server events. *) }

let dropped_with reason events =
  List.length
    (List.filter
       (fun e ->
         String.starts_with ~prefix:"kill dropped " e
         && String.ends_with ~suffix:(" " ^ reason) e )
       events )

(* The record of KILL of a storm, from the counters and the events. *)
let kill_record ~control_max events =
  let c = Pool.Private.counters () in
  { requests= c.kill_requests
  ; events= List.length (kill_events events)
  ; kills= kills events
  ; control_max
  ; off= List.exists (String.starts_with ~prefix:"kill off") events
  ; offs=
      List.length (List.filter (String.starts_with ~prefix:"kill off") events)
  ; failed= dropped_with "failed" events
  ; other= dropped_with "other_server" events }

let storm ?(kill = false) ~fibers ~seed ~seam () =
  Pool.Private.after_op :=
    if seam then fun () -> Fiber.shield (fun () -> Time.sleep Mono (Span.ms 1))
    else ignore ;
  Fun.protect
    ~finally:(fun () ->
      Pool.Private.after_op := ignore ;
      Pool.Private.on_hold := ignore )
    (fun () ->
      let control_max = ref 0 and extra = ref 0 in
      (* Phase 4b1: the atomic statements of the storm, and the holders that got
         a session of the fake server with an open transaction. *)
      let atomics = Atomic.make 0 and in_tx_holders = Atomic.make 0 in
      let (outcomes, quiet, live), events =
        with_pool ~kill ~size:8 ~seed ~domains:4 ~max_lifetime:(Span.ms 80)
          ~checkout_timeout:(Span.ms 5)
          ~setup:(fun db ->
            db.faults.idle_4031_ms <- Some 25 ;
            db.faults.reset_ms <- 1 ;
            if kill && not_owner_seed seed then db.faults.kill_err <- Some 1095 )
          (fun _ db pool ->
            let p = Db.Private.pool pool in
            (Pool.Private.on_hold :=
               fun c ->
                 match
                   Hashtbl.find_opt db.D.in_tx
                     (Cistern_mysql.Connection.info c).thread_id
                 with
                 | Some open_ when open_ () ->
                     Atomic.incr in_tx_holders
                 | _ ->
                     () ) ;
            let outcomes = ref [] and ended = Atomic.make 0 in
            Fiber.scope (fun s ->
                if kill then extra := kill_fibers s pool ~seed ~fibers ;
                if kill then
                  Fiber.fork s (fun () ->
                      kill_faults db ~seed
                        ~stop:(fun () -> Atomic.get ended >= fibers)
                        ~sample:(fun () ->
                          control_max :=
                            Int.max !control_max (Pool.Private.control p) ) ) ;
                for i = 1 to fibers do
                  Fiber.fork s (fun () ->
                      let rs = Random.State.make [|seed; i|] in
                      (* Three waves of 30 ms, at 0, 60 and 200 ms. Between them
                         the entries wait: 4031 after 25 ms idle, the end of
                         life after 68 to 92 ms. *)
                      let wave = [|0; 60; 200|].(Random.State.int rs 3) in
                      Time.sleep Mono (Span.ms (wave + Random.State.int rs 30)) ;
                      let sleep () =
                        (* With KILL, longer statements: more cancels come while
                           a statement runs. *)
                        Printf.sprintf "SLEEP %d"
                          (Random.State.int rs (if kill then 31 else 11))
                      in
                      let stmt =
                        match Random.State.int rs 8 with
                        | 0 ->
                            Stmt.map ignore (q "SELECT 1")
                        | 1 ->
                            Stmt.map ignore (q "BEGIN")
                        | 2 ->
                            Stmt.map ignore (q "FAIL 1062")
                        | 3 ->
                            (* Phase 4b1: an atomic statement of two commands,
                               in the transaction of Db.transaction. *)
                            Atomic.incr atomics ;
                            Stmt.map ignore (atomic ["ATOM a"; sleep ()])
                        | 4 ->
                            (* A BEGIN that is not raw: the entry stays clean,
                               and only the IN_TRANS check of the checkin aborts
                               it (the review of 4b1, M2). *)
                            Stmt.map ignore
                              (custom ~raw:false (fun ctx ->
                                   Result.map ignore
                                     (Cistern.Private.Exec.exec ctx
                                        (Raw.sql "BEGIN") ) ) )
                        | _ ->
                            Stmt.map ignore (q (sleep ()))
                      in
                      (* Above the margin of 50 ms. *)
                      let timeout = Span.ms (50 + Random.State.int rs 70) in
                      let run () = Db.exec ~timeout pool stmt in
                      let o =
                        match
                          if Random.State.int rs (if kill then 2 else 4) = 0
                          then
                            Fiber.with_timeout Mono
                              (Span.ms (1 + Random.State.int rs 60))
                              run
                          else run ()
                        with
                        | Ok _ ->
                            "ok"
                        | Error e ->
                            error_text e
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
            (* The pool is quiet when the connects, the polite closes and the
               work of the killer ended. The check of liveness runs with no
               seam, because each operation of the seam takes 1 ms. *)
            Pool.Private.after_op := ignore ;
            wait_killer p ;
            Time.sleep Mono (Span.ms 100) ;
            let idle, in_use, opening, _, waiting =
              Pool.Private.state (Db.Private.pool pool)
            in
            let quiet = (idle <= 8, in_use, opening, waiting) in
            let t0 = Time.now Mono in
            let live =
              Fiber.scope (fun s ->
                  let ps =
                    List.init 8 (fun _ ->
                        Fiber.fork_promise s (fun () ->
                            result_text
                              (Db.exec ~timeout:(Span.s 1) pool (q "SELECT 2")) ) )
                  in
                  let rs = List.map Promise.await ps in
                  (rs, seconds t0 < 0.05) )
            in
            (List.sort compare !outcomes, quiet, live) )
      in
      (* The events of the atomic statements: START TRANSACTION, their commands,
         COMMIT and ROLLBACK. The other events are one for each exec (and for
         each statement of the kill fibers). *)
      let atomic_event e =
        List.exists
          (fun prefix -> String.starts_with ~prefix e)
          [ "query \"START TRANSACTION\""
          ; "query \"COMMIT\""
          ; "query \"ROLLBACK\""
          ; "query \"atomic: " ]
      in
      let qs = queries events in
      let starts =
        List.length
          (List.filter
             (String.starts_with ~prefix:"query \"START TRANSACTION\"")
             qs )
      in
      ( ( outcomes
        , List.length (List.filter (fun e -> not (atomic_event e)) qs) - !extra
        , quiet
        , live
        , (Atomic.get atomics, starts, Atomic.get in_tx_holders) )
      , kill_record ~control_max:!control_max events ) )

(* The checks of a storm with KILL: one Kill or Kill_dropped event for each kill
   request, at most one control connection, KILL off only in the seeds of 1095,
   and kills in the other seeds. check_end (in with_pool) requires no Killing
   slot, no entry and no control connection at the end. *)
let check_kill ~seed ~min_kills k =
  equal ~msg:"one Kill or Kill_dropped event for each kill request" int
    k.requests k.events ;
  (* A sample each 10 to 30 ms, not a proof: check_end finds a leak at the
     end. *)
  is_true ~msg:"at most one control connection" (k.control_max <= 1) ;
  is_true ~msg:"at most one Kill_off event" (k.offs <= 1) ;
  (* A seed of 1095 with no kill request sends no KILL. *)
  equal ~msg:"Kill_off only in the seeds of 1095" bool
    (not_owner_seed seed && k.requests > 0)
    k.off ;
  if not (not_owner_seed seed) then
    is_true
      ~msg:(Printf.sprintf "at least %d kills (%d)" min_kills k.kills)
      (k.kills >= min_kills)

let check_storm ?(min_ran = 80) ~fibers
    (outcomes, queries, quiet, live, (atomics, starts, in_tx_holders)) =
  equal ~msg:"one outcome for each fiber" int fibers (List.length outcomes) ;
  (* The storm must run statements, not only waiters at their limit: at least 80
     execs of each seed run a statement (ok, or the error of FAIL). In the
     record of PR 1.2c2, the fewest were 116 of 200 and 112 of 1000. *)
  let ran =
    List.length
      (List.filter
         (fun o -> o = "ok" || o = "Constraint_violation Unique")
         outcomes )
  in
  is_true
    ~msg:(Printf.sprintf "at least %d statements ran (%d)" min_ran ran)
    (ran >= min_ran) ;
  List.iter
    (fun o -> is_true ~msg:("a permitted outcome: " ^ o) (List.mem o permitted))
    outcomes ;
  (* Phase 4b1: an atomic statement gives one event for each command that ran,
     with one START TRANSACTION event for each exec, and no joined event. *)
  equal ~msg:"one Query event for each exec that is not atomic" int
    (fibers - atomics + 8)
    queries ;
  equal ~msg:"one START TRANSACTION event for each atomic exec" int atomics
    starts ;
  is_true ~msg:"the storm ran atomic statements" (atomics > 0) ;
  equal ~msg:"no holder got a session in a transaction" int 0 in_tx_holders ;
  equal ~msg:"quiet: idle <= 8, in use, opening, waiting" (list string)
    ["true"; "0"; "0"; "0"]
    (let a, b, c, d = quiet in
     [string_of_bool a; string_of_int b; string_of_int c; string_of_int d] ) ;
  let rs, fast = live in
  equal ~msg:"8 fibers get a connection" (list string)
    (List.init 8 (fun _ -> "2"))
    rs ;
  is_true ~msg:"8 fibers get a connection at once" fast

(* The storm over [seeds] seeds. The race counters are a sum over the seeds, and
   each must be above 0. The first 2 seeds run with no seam. This sum depends on
   the seeds, thus each counter also has a test that forces its branch: the
   latch tests (waiter_lost, checkin_lost), "a cancel in the session reset"
   (cancel_in_reset), "an idle entry past its life" and "two expired entries"
   (expired_at_checkout), and "a stale idle entry" (stale_at_checkout). *)
let storm_test ~fibers ~seeds () =
  Pool.Private.reset_counters () ;
  for seed = 0 to seeds - 1 do
    check_storm ~fibers (fst (storm ~fibers ~seed ~seam:(seed >= 2) ()))
  done ;
  let c = Pool.Private.counters () in
  List.iter
    (fun (name, n) -> is_true ~msg:(name ^ " above 0") (n > 0))
    [ ("waiter_lost", c.waiter_lost)
    ; ("checkin_lost", c.checkin_lost)
    ; ("cancel_in_reset", c.cancel_in_reset)
    ; ("expired_at_checkout", c.expired_at_checkout)
    ; ("stale_at_checkout", c.stale_at_checkout) ]

(* The storm with kill=true (phase 1.5c2) over [seeds] seeds: the checks of the
   storm and of KILL for each seed. The Killing slots and the fibers of the
   kills take slots, thus fewer statements run: in the record of PR 1.5c2 (with
   the faults of its review), the fewest were 53 of 200 and 18 of 1000 (the
   bounds are 25 and 9), and the fewest kills of a seed with no 1095 were 6 of
   200 and 7 of 1000 (the bound is 2). Each bound is about half of its
   record. With
   [race], the race counters of the killer (a sum over the seeds) must be above
   0. *)
(* The sums of a run of storms with KILL. With [race], the race counters of the
   killer and the drops of Failed and Other_server (not the Off drops of the
   seeds of 1095), as sums over the seeds, must be above 0. *)
let check_race ~race records =
  if race then
    let c = Pool.Private.counters () in
    let sum f = List.fold_left (fun n k -> n + f k) 0 records in
    List.iter
      (fun (name, n) ->
        is_true ~msg:(Printf.sprintf "%s above 0 (%d)" name n) (n > 0) )
      [ ("kill_retry", c.kill_retry)
      ; ("control_reopened", c.control_reopened)
      ; ("Kill_dropped Failed", sum (fun k -> k.failed))
      ; ("Kill_dropped Other_server", sum (fun k -> k.other)) ]

let kill_storm_test ?(race = false) ~fibers ~seeds ~min_ran ~min_kills () =
  with_refuse_control
  @@ fun () ->
  Pool.Private.reset_counters () ;
  let records = ref [] in
  for seed = 0 to seeds - 1 do
    let before = (Pool.Private.counters ()).kill_requests in
    let r, k = storm ~kill:true ~fibers ~seed ~seam:(seed >= 2) () in
    check_storm ~min_ran ~fibers r ;
    check_kill ~seed ~min_kills {k with requests= k.requests - before} ;
    records := k :: !records
  done ;
  check_race ~race !records

let tests =
  group "Db and the pool"
    [ test "Pool.with_conn: Discard on a usable connection aborts it" (fun () ->
          let r, _ =
            with_pool ~size:1 (fun _ db pool ->
                ignore (Db.exec pool (clean ())) ;
                let p = Db.Private.pool pool in
                let start = Pool.now p in
                let deadline = Pool.add_span start (Span.s 5) in
                let at_event = ref "" in
                let usable = ref false in
                let b =
                  Pool.with_conn p ~start ~deadline (fun held ->
                      usable := Cistern_mysql.Connection.usable held.c ;
                      ( Pool.Value ()
                      , (fun () ->
                          (* The event comes after the checkin: the slot is
                             free, and the socket is closed. *)
                          let _, in_use, _, _, _ = Pool.Private.state p in
                          at_event :=
                            Printf.sprintf "sockets %d in use %d"
                              (Pool.Private.sockets p) in_use )
                      , Pool.Discard ) )
                in
                ( match b with
                | Pool.Ran (Pool.Value (), event) ->
                    event ()
                | _ ->
                    at_event := "no Ran" ) ;
                (* The server sees the close. *)
                Time.sleep Mono (Span.ms 1) ;
                let open_after = db.open_now in
                ignore (Db.exec pool (clean ())) ;
                Printf.sprintf "%b %s, open %d, connects %d" !usable !at_event
                  open_after db.connects )
          in
          equal string "true sockets 0 in use 0, open 0, connects 2" r )
    ; test "a query, and its events" (fun () ->
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
                    | Ev.Connect _
                    | Ev.Kill _
                    | Ev.Kill_dropped _
                    | Ev.Kill_off _ ->
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
                  | Ev.Connect _ | Ev.Kill _ | Ev.Kill_dropped _ | Ev.Kill_off _
                    ->
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
          equal string "2 true" r )
    ; test "the storm: 200 fibers, 8 seeds, 4 domains"
        (storm_test ~fibers:200 ~seeds:8)
    ; test "the storm with KILL: 200 fibers, 8 seeds, 4 domains"
        (kill_storm_test ~race:true ~fibers:200 ~seeds:8 ~min_ran:25
           ~min_kills:2 ) ]

(* The slow storm, outside the gate: make sim-slow (CISTERN_SLOW). *)
let slow_tests =
  group "Db and the pool, slow"
    [ tag "slow"
        (test "the storm: 1000 fibers, 20 seeds, 4 domains"
           (storm_test ~fibers:1000 ~seeds:20) )
    ; tag "slow"
        (test "the storm with KILL: 1000 fibers, 20 seeds, 4 domains"
           (kill_storm_test ~race:true ~fibers:1000 ~seeds:20 ~min_ran:9
              ~min_kills:2 ) ) ]
