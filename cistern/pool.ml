(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

open Weft
module P = Pool_state
module Connection = Cistern_mysql.Connection
module Config = Cistern_mysql.Config
module Message = Cistern_mysql.Message
module Error = Cistern_core.Error
module Event = Cistern_core.Event
module Dialect = Cistern_core.Dialect

(* A connection of the pool, with the test-only owner field: the id of the fiber
   that holds it, or -1. *)
type conn = {c: Connection.t; owner: int Atomic.t}

type outcome = Got of conn P.entry | Failed of Error.t | Abandoned

(* An entry of the kill queue (the plan of phase 1.5, D4): the session to kill
   on the server of its identity, and the token of its Killing slot. *)
type kill_entry = {kid: int64; identity: Connection.identity; token: P.token}

(* The control connection of the killer, outside the slots (D5). [used] is the
   time of its open or of its last KILL. *)
type control = {cc: Connection.t; mutable used: Span.t; limit: Span.t}

(* The state of the killer: the backoff of its connects, and the token of an
   entry whose first try failed. Like the control connection, the killer owns it
   while it runs, and the drain after the join of the scope (D5a). *)
type killer_state =
  { mutable step: Span.t
  ; mutable retry_at: Span.t option
  ; mutable failed_once: P.token option }

(* The fibers that hold a [Tx] (phase 1.3). *)
module Fibers = Set.Make (Fiber.Id)

type t =
  { config: Config.t
  ; size: int
  ; checkout_timeout: Span.t
  ; on_query: Event.t -> unit
  ; random: Random.State.t  (** Under [mutex]. *)
  ; origin: Clock.mono
  ; mutex: Mutex.t
  ; mutable st: (conn, outcome Ivar.t) P.t  (** Under [mutex]. *)
  ; bell: Fiber.Semaphore.t
  ; closed: bool Atomic.t
  ; sockets: int Atomic.t
  ; over_bound: int Atomic.t
  ; kill_bell: Fiber.Semaphore.t  (** The doorbell of the killer. *)
  ; mutable queue: kill_entry list  (** Under [mutex]: the oldest first. *)
  ; mutable kill_off: bool
        (** Under [mutex]: [KILL] is off for the pool (D6a). *)
  ; mutable drained: bool
        (** Under [mutex]: the shutdown took the last entries of the queue. *)
  ; mutable control: control option
        (** Only the killer writes it while it runs. The shutdown uses it after
            the join of the scope (D5a). *)
  ; control_sockets: int Atomic.t
  ; killer: killer_state
  ; txs: Fibers.t Atomic.t }

module Private = struct
  let after_op = ref ignore

  let before_try = ref ignore

  let connect_control = ref Connection.connect

  let trace = ref ignore

  type counters =
    { waiter_lost: int
    ; checkin_lost: int
    ; cancel_in_reset: int
    ; expired_at_checkout: int
    ; stale_at_checkout: int
    ; cancel_in_stale: int
    ; kill_dropped: int
    ; kill_requests: int
    ; kill_retry: int
    ; control_reopened: int }

  let waiter_lost = Atomic.make 0

  let checkin_lost = Atomic.make 0

  let cancel_in_reset = Atomic.make 0

  let expired_at_checkout = Atomic.make 0

  let stale_at_checkout = Atomic.make 0

  let cancel_in_stale = Atomic.make 0

  let kill_dropped = Atomic.make 0

  let kill_requests = Atomic.make 0

  let kill_retry = Atomic.make 0

  let control_reopened = Atomic.make 0

  let counters () =
    { waiter_lost= Atomic.get waiter_lost
    ; checkin_lost= Atomic.get checkin_lost
    ; cancel_in_reset= Atomic.get cancel_in_reset
    ; expired_at_checkout= Atomic.get expired_at_checkout
    ; stale_at_checkout= Atomic.get stale_at_checkout
    ; cancel_in_stale= Atomic.get cancel_in_stale
    ; kill_dropped= Atomic.get kill_dropped
    ; kill_requests= Atomic.get kill_requests
    ; kill_retry= Atomic.get kill_retry
    ; control_reopened= Atomic.get control_reopened }

  let reset_counters () =
    List.iter
      (fun a -> Atomic.set a 0)
      [ waiter_lost
      ; checkin_lost
      ; cancel_in_reset
      ; expired_at_checkout
      ; stale_at_checkout
      ; cancel_in_stale
      ; kill_dropped
      ; kill_requests
      ; kill_retry
      ; control_reopened ]

  let sockets t = Atomic.get t.sockets

  let over_bound t = Atomic.get t.over_bound

  let kill_off t = Mutex.protect t.mutex (fun () -> t.kill_off)

  let queue t =
    Mutex.protect t.mutex (fun () -> List.map (fun e -> e.kid) t.queue)

  let control t = Atomic.get t.control_sockets

  let killing t =
    Mutex.protect t.mutex (fun () -> List.length (P.killing_slots t.st))

  let idle_expiry t =
    Mutex.protect t.mutex (fun () ->
        List.map (fun (e : _ P.entry) -> e.expires) (P.idle t.st) )

  let state t =
    Mutex.protect t.mutex (fun () ->
        let s = t.st in
        ( List.length (P.idle s)
        , P.in_use s
        , P.opening s
        , P.closing s
        , P.waiting s ) )
end

let margin () = Span.ms 50

let create ?random ?(on_query = ignore) ~pool_size ~max_lifetime
    ~checkout_timeout config =
  if Span.compare checkout_timeout Span.zero <= 0 then
    invalid_arg "Db.with_db: checkout_timeout must be positive" ;
  let st = P.create ~size:pool_size ~max_lifetime in
  { config
  ; size= pool_size
  ; checkout_timeout
  ; on_query
  ; random=
      ( match random with
      | Some r ->
          Random.State.split r
      | None ->
          Random.split () )
  ; origin= Time.now Clock.Mono
  ; mutex= Mutex.create ()
  ; st
  ; bell= Fiber.Semaphore.create ~max:1 0
  ; closed= Atomic.make false
  ; sockets= Atomic.make 0
  ; over_bound= Atomic.make 0
  ; kill_bell= Fiber.Semaphore.create ~max:1 0
  ; queue= []
  ; kill_off= false
  ; drained= false
  ; control= None
  ; control_sockets= Atomic.make 0
  ; killer= {step= Span.ms 100; retry_at= None; failed_once= None}
  ; txs= Atomic.make Fibers.empty }

(* The time of the pool: the span since its origin. *)
let now t = Time.diff Clock.Mono (Time.now Clock.Mono) t.origin

let mono t span = Time.add Clock.Mono t.origin span

(* One pure operation under the mutex, then the seam of the tests. *)
let op t f =
  let r =
    Mutex.protect t.mutex (fun () ->
        let s, r = f t.st in
        t.st <- s ;
        r )
  in
  !Private.after_op () ; r

let ring t =
  !Private.trace "ring" ;
  ignore (Fiber.Semaphore.try_release t.bell)

let release t first = if first then Atomic.decr t.sockets

let abort t (c : conn) = release t (Connection.abort c.c)

(* A checkin, then its effects: the fills, then the doorbell. *)
let rec checkin t (e : conn P.entry) ~reusable ~dirty =
  let now = now t in
  let r, wake =
    op t (fun s ->
        let s, r, wake = P.checkin s now e ~reusable ~dirty in
        (s, (r, wake)) )
  in
  ( match r with
  | P.Give (w, e) ->
      hand_off t w e
  | P.Idle | P.Closing | P.Freed ->
      ()
  | P.Abort c ->
      abort t c ) ;
  if wake then ring t

(* The fill of a waiter. If it abandoned, the entry goes back. *)
and hand_off t w e =
  !Private.trace "fill" ;
  if not (Ivar.try_fill w (Got e)) then (
    Atomic.incr Private.checkin_lost ;
    checkin t e ~reusable:true ~dirty:e.dirty )

(* An entry that its holder cannot give back as good: abort it first, then free
   its slot. No cancellation point. *)
let discard t (e : conn P.entry) =
  abort t e.conn ;
  checkin t e ~reusable:false ~dirty:e.dirty

(* An event holds no message of a server error packet (the plan of phase 2,
   D23): its error is redacted. The result of the statement keeps the full
   error. *)
let redact_event = function
  | Event.Query ({outcome= Event.Failed e; _} as q) ->
      Event.Query {q with outcome= Event.Failed (Error.redact e)}
  | Event.Connect ({error= Some e; _} as c) ->
      Event.Connect {c with error= Some (Error.redact e)}
  | ( Event.Query _
    | Event.Connect _
    | Event.Kill _
    | Event.Kill_dropped _
    | Event.Kill_off _ ) as ev ->
      ev

let emit t ev = try t.on_query (redact_event ev) with _ -> ()

let dropped t kid reason =
  Atomic.incr Private.kill_dropped ;
  emit t (Event.Kill_dropped {connection_id= kid; reason})

(* KILL goes off for the pool (D6a, review C2 of the plan, M-a): the flag, the
   drop of the queue and the free of its Killing slots in one mutex section.
   Only the first call gives the event. Then the effects of D4a: the doorbell of
   the opener if wake, then the events, with Kill_dropped last. *)
let turn_kill_off t reason =
  let now = now t in
  let r =
    op t (fun s ->
        if t.kill_off then (s, None)
        else (
          t.kill_off <- true ;
          let gone = t.queue in
          t.queue <- [] ;
          let s, wake =
            List.fold_left
              (fun (s, _) e ->
                let s, _, wake = P.killed s now e.token in
                (s, wake) )
              ( s
              , (false
                [@mutate
                  off
                    "with an empty queue the state does not change: an extra \
                     doorbell only costs one tick of the opener"] ) )
              gone
          in
          (s, Some (gone, wake)) ) )
  in
  match r with
  | None ->
      ()
  | Some (gone, wake) ->
      if wake then ring t ;
      (* The killer closes its control connection. *)
      ignore (Fiber.Semaphore.try_release t.kill_bell) ;
      emit t (Event.Kill_off {reason}) ;
      List.iter (fun e -> dropped t e.kid `Off) gone

(* An entry whose statement can still run on the server (D1): the holder rule
   aborts it first. Then one mutex section reads the flags kill_off and drained,
   takes the token, and checks the duplicates and the bound of the queue (D4,
   M-a). A full queue or a duplicate frees the slot in the same section. Then
   the effects of D4a. *)
let kill t (e : conn P.entry) =
  abort t e.conn ;
  let info = Connection.info e.conn.c in
  let kid = info.connection_id in
  let now = now t in
  let r =
    op t (fun s ->
        match info.identity with
        | Connection.Checked identity when not t.kill_off ->
            if t.drained then
              let s, _, wake =
                P.checkin s now e
                  ~reusable:
                    (false
                    [@mutate
                      off
                        "the pool is closed: a reusable checkin gives Abort, \
                         which frees the slot in the same way, and the \
                         connection is already aborted"] )
                  ~dirty:e.dirty
              in
              (s, `Dropped (`Closed, wake))
            else
              let s, token, wake = P.killing s now e in
              let full = List.length t.queue >= t.size in
              if
                full
                || List.exists
                     (fun q -> q.kid = kid && q.identity = identity)
                     t.queue
              then
                let s, _, wake = P.killed s now token in
                (s, `Dropped ((if full then `Queue_full else `Duplicate), wake))
              else (
                t.queue <- t.queue @ [{kid; identity; token}] ;
                (s, `Queued wake) )
        | Connection.Checked _ | Connection.Unchecked | Connection.Refused _ ->
            let s, _, wake = P.checkin s now e ~reusable:false ~dirty:e.dirty in
            (s, `Freed wake) )
  in
  ( match r with
  | `Queued _ | `Dropped _ ->
      Atomic.incr Private.kill_requests
  | `Freed _ ->
      () ) ;
  match r with
  | `Queued wake ->
      if wake then ring t ;
      ignore (Fiber.Semaphore.try_release t.kill_bell)
  | `Freed wake ->
      if wake then ring t
  | `Dropped (reason, wake) ->
      if wake then ring t ;
      dropped t kid reason

(* An abort of a connection that a holder holds (a [Tx]), with the count of the
   sockets. The checkin then discards the entry. *)
let abort_conn t c = release t (Connection.abort c)

let check_open t =
  if Atomic.get t.closed then invalid_arg "Db.t used after with_db"

(* The set of the fibers that hold a [Tx]. A failed compare_and_set means that
   another domain changed the set: read it again. *)
let rec change_txs t f =
  let s = Atomic.get t.txs in
  ( if not (Atomic.compare_and_set t.txs s (f s)) then change_txs t f )
  [@mutate
    off
      "no switch point between get and compare_and_set in the simulator: the \
       retry runs only on real domains"]

let enter_tx t id = change_txs t (Fibers.add id)

let leave_tx t id = change_txs t (Fibers.remove id)

let in_tx t = Fibers.mem (Fiber.self ()) (Atomic.get t.txs)

let tx_fibers t = Fibers.cardinal (Atomic.get t.txs)

let address config =
  match Config.address config with
  | Config.Tcp {host; port} ->
      Printf.sprintf "%s:%d" host port
  | Config.Socket {path; _} ->
      path

let open_failed t err =
  let now = now t in
  let wake = op t (fun s -> P.open_failed s now err) in
  if wake then ring t

(* Only a cancel of the opener calls it: the opener ends, and it needs no
   doorbell. *)
let open_aborted t =
  let now = now t in
  ignore (op t (fun s -> P.open_aborted s now) : bool)

(* A connect fiber. It catches each exception except a cancel, so that the scope
   of the opener never fails. *)
let connect t =
  let t0 = Time.now Clock.Mono in
  let event error =
    emit t
      (Event.Connect
         { address= address t.config
         ; duration= Time.diff Clock.Mono (Time.now Clock.Mono) t0
         ; error } )
  in
  match Connection.connect t.config with
  | Ok c -> (
      ( if Atomic.fetch_and_add t.sockets 1 >= t.size then
          Atomic.incr t.over_bound )
      [@mutate off "a detector for the tests: a correct pool never reaches it"] ;
      let conn = {c; owner= Atomic.make (-1)} in
      let info = Connection.info c in
      let now = now t in
      let r, wake =
        op t (fun s ->
            let u = Random.State.float t.random 0.3 -. 0.15 in
            let s, r, wake =
              P.opened s now conn ~wait_timeout:info.wait_timeout
                ~dialect:info.dialect ~u
            in
            (s, (r, wake)) )
      in
      ( match r with
      | P.Opened_give (w, e) ->
          hand_off t w e
      | P.Opened_idle ->
          ()
      | P.Opened_close c ->
          (abort [@mutate off "connect fibers end before shutdown"]) t c ) ;
      if wake then ring t ;
      event None ;
      (* The first refused identity turns KILL off for the pool, with one event
         (D6a). *)
      match info.identity with
      | Connection.Refused reason ->
          turn_kill_off t reason
      | Connection.Unchecked | Connection.Checked _ ->
          () )
  | Error err ->
      open_failed t err ; event (Some err)
  | exception (Fiber.Cancelled _ as ex) ->
      open_aborted t ; raise ex
  | exception ex ->
      (* Connection.connect gives Error for each failure of the network: an
         exception here is a bug, and the opener must go on. *)
      let err = Error.Connect ("connect: " ^ Printexc.to_string ex) in
      ( (open_failed t err ; event (Some err) )
      [@mutate off "Connection.connect raises only for a bug"] )

let closed_done t id =
  let now = now t in
  let wake = op t (fun s -> P.closed_done s now id) in
  if wake then ring t

(* A polite close, by the opener or by the shutdown. A close that raises (a
   cancel of the opener, or the limit of 2 s of the shutdown) closed the flow
   first. *)
let polite_close t (e : conn P.entry) =
  release t
    (match Connection.close e.conn.c with first -> first | exception _ -> true)

let opener t =
  Fiber.scope (fun cs ->
      let iteration () =
        let now = now t in
        let tk = op t (fun s -> P.tick s now) in
        for _ = 1 to tk.connects do
          match Fiber.fork cs (fun () -> connect t) with
          | () ->
              ()
          | exception _ ->
              (open_aborted
              [@mutate off "no cancellation point between tick and the forks"])
                t
        done ;
        List.iter
          (fun (e : conn P.entry) -> polite_close t e ; closed_done t e.id)
          tk.to_close ;
        let timer =
          match Option.bind tk.next_timer (mono t) with
          | Some at ->
              Time.at Clock.Mono at
          | None ->
              never
        in
        sync (choose [Fiber.Semaphore.acquire t.bell; timer])
      in
      let rec loop () =
        ( match iteration () with
        | () ->
            ()
        | exception (Fiber.Cancelled _ as e) ->
            raise e
        | exception _ ->
            () ) ;
        loop ()
      in
      loop () )

let in_trans c = Connection.status c land Message.status_bit In_trans <> 0

(* The dialect of an event: of its connection, else of the last connect. *)
let dialect t (c : Connection.t option) =
  match c with
  | Some c ->
      (Connection.info c).dialect
  | None -> (
    match Mutex.protect t.mutex (fun () -> P.last_dialect t.st) with
    | Some d ->
        d
    | None ->
        Dialect.Mysql )

(* The saturated sum of two spans: a very large timeout gives the largest time,
   not a negative one. *)
let add_span a b =
  let x = Span.to_ns a and y = Span.to_ns b in
  Span.of_ns
    ( if
        ( Int64.compare y (Int64.sub Int64.max_int x)
        > 0 )
        [@mutate off "at the bound, the sum and the largest time are equal"]
      then Int64.max_int
      else Int64.add x y )

let time_left t ~deadline =
  Span.compare
    (Span.of_ns (Int64.sub (Span.to_ns deadline) (Span.to_ns (now t))))
    (margin ())
  >= 0

type held = {c: Connection.t; dirty: bool}

type held_end = Give_back of {dirty: bool} | Discard

type 'a fn_outcome =
  | Value of 'a
  | Err of Error.t
  | Reraise of exn * Printexc.raw_backtrace
  | Cancel of exn

type 'a before =
  | Checkout_failed of Error.t
  | Margin of Connection.t
  | Reset_failed of Connection.t * Error.t
  | Cancelled_before of exn
  | Ran of 'a fn_outcome * (unit -> unit)

(* What the steps before the function gave. *)
type 'a step =
  | Stale
  | Margin_step
  | Reset_step of Error.t
  | Cancel_step of exn
  | Ran_step of 'a fn_outcome * (unit -> unit) * held_end

let with_conn t ~start ~deadline f =
  let limit =
    Span.of_ns
      (Int64.min (Span.to_ns deadline)
         (Span.to_ns (add_span start t.checkout_timeout)) )
  in
  let flow_deadline = mono t deadline in
  let time_left () = time_left t ~deadline in
  (* The error of a waiter at its limit (P3): one [now]. *)
  let wait_error now stored =
    match stored with
    | Some e ->
        e
    | None ->
        if Span.compare deadline now <= 0 then Error.Timeout
        else Error.Pool_exhausted
  in
  let rec get resets =
    let now = now t in
    let w = Ivar.create () in
    let r, wake, expired =
      op t (fun s ->
          let before = P.closing s in
          let s, r, wake = P.checkout s now w in
          (s, (r, wake, P.closing s - before)) )
    in
    ignore (Atomic.fetch_and_add Private.expired_at_checkout expired) ;
    if wake then ring t ;
    match r with
    | P.Closed ->
        invalid_arg "Db.t used after with_db"
    | P.Conn e ->
        hold e resets
    | P.Wait id ->
        wait id w resets
  and wait id w resets =
    match
      sync
        (choose
           [ wrap (Ivar.read w) (fun o -> `Out o)
           ; ( match mono t limit with
             | Some at ->
                 wrap (Time.at Clock.Mono at) (fun () -> `Limit)
             | None ->
                 never ) ] )
    with
    | `Out (Got e) ->
        hold e resets
    | `Out (Failed err) ->
        Checkout_failed err
    | `Out Abandoned | `Limit ->
        abandon id w ~cancel:None resets
    | exception (Fiber.Cancelled _ as ex) ->
        abandon id w ~cancel:(Some ex) resets
  and abandon id w ~cancel resets =
    let at = now t in
    let stored =
      op t (fun s ->
          (* The wake of cancel_wait comes with the Killing slots of the shell
             (phase 1.5c1a): before, no shell operation makes a Killing slot,
             thus cancel_wait has no work for the opener. *)
          let s, _found, err, wake = P.cancel_wait s at id in
          (s, (err, wake)) )
    in
    let stored, wake = stored in
    (* The removal of an expired Killing slot can start a connect (review C of
       1.5a, L1). *)
    ( if wake then ring t )
    [@mutate
      off
        "the opener armed its timer at the age limit, and it fires anyway: the \
         doorbell matters only when two fibers read the clock in one order and \
         run their operations in the other order"] ;
    let give_up () =
      match cancel with
      | Some ex ->
          Cancelled_before ex
      | None ->
          Checkout_failed (wait_error at stored)
    in
    if Ivar.try_fill w Abandoned then give_up ()
    else
      match Ivar.peek w with
      | Some (Got e) ->
          (* The entry came while the waiter gave up. At its limit, the waiter
             uses it if its deadline leaves the margin. Else, and after a
             cancel, it goes back. *)
          Atomic.incr Private.waiter_lost ;
          if cancel = None && time_left () then hold e resets
          else (
            checkin t e ~reusable:true ~dirty:e.dirty ;
            give_up () )
      | Some (Failed err) -> (
        (* A cancel always goes up. *)
        match cancel with
        | Some ex ->
            Cancelled_before ex
        | None ->
            Checkout_failed err )
      | Some Abandoned | None ->
          give_up ()
  and hold e resets =
    let me = Fiber.Id.to_int (Fiber.self ()) in
    ( if not (Atomic.compare_and_set e.conn.owner (-1) me) then
        invalid_arg "Pool: an entry with two holders" )
    [@mutate off "a detector for the tests: a correct pool never reaches it"] ;
    (* The entry goes back with no owner. Its deadline stays: each holder sets
       its own before its first wait, and a write of [Connection.close] waits
       only with a full send buffer. *)
    let give_back ~reusable ~dirty =
      Atomic.set e.conn.owner (-1) ;
      checkin t e ~reusable ~dirty
    in
    let c = e.conn.c in
    (* A successful reset makes the entry clean. *)
    let was_reset = ref false in
    match
      (* A cancel at the lock of a TLS session in the stale check is a cancel at
         the checkout: its event comes (Cancelled_before). The stale check broke
         the connection first. *)
      match Connection.stale c with
      | exception (Fiber.Cancelled _ as ex) ->
          Atomic.incr Private.cancel_in_stale ;
          Cancel_step ex
      | true ->
          Stale
      | false -> (
          Connection.set_deadline c flow_deadline ;
          if e.dirty && not (time_left ()) then Margin_step
          else
            match
              if e.dirty then (
                match Connection.reset c with
                | r ->
                    r
                | exception (Fiber.Cancelled _ as ex) ->
                    Atomic.incr Private.cancel_in_reset ;
                    raise ex )
              else Ok ()
            with
            | exception (Fiber.Cancelled _ as ex) ->
                Cancel_step ex
            | Error err ->
                Reset_step err
            | Ok () ->
                was_reset := e.dirty ;
                if not (time_left ()) then Margin_step
                else
                  (* No wait between the margin check and the function. *)
                  let outcome, event, held_end =
                    f {c; dirty= e.dirty && not !was_reset}
                  in
                  Ran_step (outcome, event, held_end) )
    with
    | exception ex ->
        (* A bug of Cistern. *)
        discard t e ; raise ex
    | Stale ->
        Atomic.incr Private.stale_at_checkout ;
        discard t e ;
        get resets
    | Margin_step ->
        give_back ~reusable:true ~dirty:(e.dirty && not !was_reset) ;
        Margin c
    | Cancel_step ex ->
        discard t e ; Cancelled_before ex
    | Reset_step Error.Timeout ->
        discard t e ;
        Reset_failed (c, Error.Timeout)
    | Reset_step err ->
        discard t e ;
        if resets >= 1 then Reset_failed (c, err) else get (resets + 1)
    | Ran_step (outcome, event, held_end) ->
        ( match held_end with
        | Give_back {dirty} when Connection.usable c && not (in_trans c) ->
            give_back ~reusable:true ~dirty
        | (Give_back _ | Discard) when Connection.in_flight c ->
            (* The server can still run the last command (D1): KILL. *)
            kill t e
        | Give_back {dirty} ->
            abort t e.conn ;
            give_back ~reusable:false ~dirty
        | Discard ->
            discard t e ) ;
        Ran (outcome, event)
  in
  get 0

let exec t ~timeout stmt =
  (* Before each other check: a misuse emits no event. *)
  check_open t ;
  if in_tx t then invalid_arg "Db.exec in the fiber of a Tx" ;
  match Stmt.Private.pure_value stmt with
  | Some v ->
      Ok (v ())
  | None -> (
      let meta = Stmt.Private.meta stmt in
      let t0 = Time.now Clock.Mono in
      let event ?c outcome =
        emit t
          (Event.Query
             { sql= String.concat "; " (meta.redacted (dialect t c))
             ; duration= Time.diff Clock.Mono (Time.now Clock.Mono) t0
             ; outcome } )
      in
      let failed ?c e = event ?c (Event.Failed e) ; Error e in
      let cancelled ?c ~sent ex =
        event ?c (Event.Cancelled {sent}) ;
        raise ex
      in
      ( match Fiber.check_cancel () with
      | () ->
          ()
      | exception (Fiber.Cancelled _ as ex) ->
          cancelled ~sent:false ex ) ;
      let start = now t in
      let deadline = add_span start timeout in
      (* One statement on the held connection, as data for [with_conn]. *)
      let run (h : held) =
        let c = h.c in
        let ctx = Exec.make c in
        let give_back = Give_back {dirty= h.dirty || meta.raw <> None} in
        match Stmt.Private.run stmt ctx with
        | Ok ran ->
            ( Value ran
            , (fun () -> event ~c (Event.Done {rows= ran.rows}))
            , give_back )
        | Error err ->
            (Err err, (fun () -> event ~c (Event.Failed err)), give_back)
        | exception (Fiber.Cancelled _ as ex) ->
            let sent = Exec.sent ctx in
            (Cancel ex, (fun () -> event ~c (Event.Cancelled {sent})), Discard)
      in
      match meta.text with
      | Some (Error message) ->
          failed (Error.Invalid_value {code= None; message})
      | _ -> (
        match with_conn t ~start ~deadline run with
        | Checkout_failed err ->
            failed err
        | Margin c ->
            failed ~c Error.Timeout
        | Reset_failed (c, err) ->
            failed ~c err
        | Cancelled_before ex ->
            cancelled ~sent:false ex
        | Ran (Value ran, event) ->
            event () ;
            Ok (ran.finish ())
        | Ran (Err err, event) ->
            event () ; Error err
        | Ran (Cancel ex, event) ->
            event () ; raise ex
        | Ran (Reraise (ex, bt), event) ->
            (event () ;
             Printexc.raise_with_backtrace ex bt )
            [@mutate off "the function of exec gives no Reraise"] ) )

(* The killer (D5 to D9). *)

let kill_limit () = Span.s 2

let first_step () = Span.ms 100

let max_step () = Span.s 5

(* The abort of the control connection: its state is not known. *)
let drop_control t =
  match t.control with
  | None ->
      ()
  | Some ctl ->
      t.control <- None ;
      if Connection.abort ctl.cc then Atomic.decr t.control_sockets

(* The end of an entry: killed and the removal of the entry with this token, in
   one mutex section (review C2 of the plan, M-b), then the doorbell of the
   opener if wake (L-b). *)
let finish_entry t token =
  let now = now t in
  let wake =
    op t (fun s ->
        let s, _, wake = P.killed s now token in
        t.queue <- List.filter (fun q -> q.token <> token) t.queue ;
        (s, wake) )
  in
  if wake then ring t

(* One try of an entry, in its own shield with the limit of 2 s (D8). The
   identity checks, KILL, and for a final result the end of the entry. *)
let try_entry t ctl (e : kill_entry) ~last =
  let info = Connection.info ctl.cc in
  Fiber.shield (fun () ->
      let r =
        match
          Fiber.with_timeout Clock.Mono (kill_limit ()) (fun () ->
              (* Another fiber can turn KILL off and drop the entry after the
                 killer read it (the drop of Kill_off empties the queue in the
                 same section as the flag): then no KILL. A Kill_off that comes
                 after this check and before the write of KILL does not stop it:
                 the entry was valid when the check ran, and its identity checks
                 follow (review C of 1.5c1a, L1). *)
              if
                Mutex.protect t.mutex (fun () ->
                    not (List.exists (fun q -> q.token = e.token) t.queue) )
              then `Dropped
              else if
                info.identity <> Connection.Checked e.identity
                || e.kid = info.connection_id
              then `Other
              else
                match Connection.kill ctl.cc e.kid with
                | Ok Connection.Killed ->
                    `Done `Killed
                | Ok Connection.Gone ->
                    `Done `Gone
                | Ok Connection.Not_owner ->
                    `Not_owner
                | Error _ ->
                    `Failed )
        with
        | r ->
            r
        | exception Fiber.Timeout ->
            `Failed
      in
      ( match r with
      | `Failed when not last ->
          ()
      | `Dropped ->
          ()
      | `Done _ | `Other | `Not_owner | `Failed ->
          finish_entry t e.token ) ;
      r )

let locked t f = Mutex.protect t.mutex f

let open_control t k =
  match k.retry_at with
  | Some at when Span.compare (now t) at < 0 ->
      ()
  | _ -> (
      let t0 = Time.now Clock.Mono in
      let event error =
        emit t
          (Event.Connect
             { address= address t.config
             ; duration= Time.diff Clock.Mono (Time.now Clock.Mono) t0
             ; error } )
      in
      match !Private.connect_control t.config with
      | Ok c -> (
          Atomic.incr t.control_sockets ;
          let info = Connection.info c in
          t.control <-
            Some {cc= c; used= now t; limit= P.idle_limit info.wait_timeout} ;
          k.step <- first_step () ;
          event None ;
          (* A control connection that cannot verify an entry (D5a). *)
          match info.identity with
          | Connection.Refused reason ->
              turn_kill_off t reason
          | Connection.Checked _ | Connection.Unchecked ->
              () )
      | Error err ->
          event (Some err) ;
          k.retry_at <- Some (add_span (now t) k.step) ;
          k.step <-
            Span.of_ns
              (Int64.min
                 (Span.to_ns (max_step ()))
                 (Int64.mul 2L (Span.to_ns k.step)) ) )

(* D7: before each entry, the idle limit and the stale check. *)
let check_control t =
  match t.control with
  | Some ctl
    when ( Span.compare
             (Span.of_ns (Int64.sub (Span.to_ns (now t)) (Span.to_ns ctl.used)))
             ctl.limit
         >= 0 )
         [@mutate
           off
             "the boundary of one nanosecond: no test can place the check at \
              the exact end of the idle limit"]
         || Connection.stale ctl.cc ->
      Atomic.incr Private.control_reopened ;
      drop_control t
  | _ ->
      ()

(* The entries of the queue, until it is empty, KILL is off, or no control
   connection opens. *)
let rec work t k =
  if locked t (fun () -> t.kill_off) then drop_control t
  else
    match locked t (fun () -> t.queue) with
    | [] ->
        ()
    | e :: _ -> (
        check_control t ;
        match t.control with
        | None ->
            (* A new control connection can turn KILL off (D5a): start again. *)
            open_control t k ;
            if t.control <> None then work t k
        | Some ctl ->
            let last = k.failed_once = Some e.token in
            !Private.before_try () ;
            let t0 = Time.now Clock.Mono in
            let r = try_entry t ctl e ~last in
            ctl.used <- now t ;
            let duration = Time.diff Clock.Mono (Time.now Clock.Mono) t0 in
            ( match r with
            (* The tokens are unique: a token of an entry that ended never comes
               again, thus failed_once needs no reset. *)
            | `Dropped ->
                ()
            | `Done outcome ->
                emit t (Event.Kill {connection_id= e.kid; duration; outcome})
            | `Other ->
                dropped t e.kid `Other_server
            | `Not_owner ->
                dropped t e.kid `Not_owner ; turn_kill_off t `Not_owner
            | `Failed ->
                drop_control t ;
                if last then dropped t e.kid `Failed
                else (
                  Atomic.incr Private.kill_retry ;
                  k.failed_once <- Some e.token ) ) ;
            work t k )

let killer t =
  let k = t.killer in
  let iteration () =
    let off = locked t (fun () -> t.kill_off) in
    if (not off) && t.control = None then open_control t k ;
    work t k ;
    let timer =
      match (t.control, k.retry_at) with
      | None, Some at when not off -> (
        match mono t at with Some m -> Time.at Clock.Mono m | None -> never )
      | _ ->
          never
    in
    sync (choose [Fiber.Semaphore.acquire t.kill_bell; timer])
  in
  let rec loop () =
    ( match iteration () with
    | () ->
        ()
    | exception (Fiber.Cancelled _ as e) ->
        raise e
    | exception _ ->
        () ) ;
    loop ()
  in
  loop ()

let drain_limit () = Span.s 15

(* The drain at the end of with_db (D10, D10a): after close_all and the join of
   the scope, in a shield with the limit of 15 s. It sends the entries with the
   rules of the killer, until the queue is empty, also the entries that come
   during the drain. One mutex section ends it and sets [drained]: after the
   limit, it drops the entries that remain (Kill_dropped Drain_timeout) and
   frees their slots. It catches each exception. *)
let drain t =
  let k = t.killer in
  let finish ~late =
    let now = now t in
    op t (fun s ->
        match t.queue with
        | [] ->
            t.drained <- true ;
            (s, `Done)
        | gone when late ->
            t.drained <- true ;
            t.queue <- [] ;
            let s =
              List.fold_left
                (fun s e ->
                  let s, _, _ = P.killed s now e.token in
                  s )
                s gone
            in
            (s, `Dropped gone)
        | _ ->
            (s, `More) )
  in
  let rec run () =
    (* With kill=false or KILL off, the queue is empty: work opens no control
       connection. *)
    work t k ;
    match finish ~late:false with
    | `Done | `Dropped _ ->
        ()
    | `More ->
        (* No control connection: wait for the end of its backoff. *)
        ( match (t.control, k.retry_at) with
        | None, Some at -> (
          match mono t at with
          | Some m ->
              sync (Time.at Clock.Mono m)
          | None ->
              () )
        | _ ->
            () ) ;
        run ()
  in
  Fiber.shield (fun () ->
      match Fiber.with_timeout Clock.Mono (drain_limit ()) run with
      | () ->
          ()
      | exception _ ->
          () ) ;
  (* After a run that ended, the queue is empty, and this section changes
     nothing. *)
  match finish ~late:true with
  | `Dropped gone ->
      List.iter (fun e -> dropped t e.kid `Drain_timeout) gone
  | `Done | `More ->
      ()

(* The polite close of the control connection, at the shutdown. *)
let close_control t =
  match t.control with
  | None ->
      ()
  | Some ctl ->
      if
        match Connection.close ctl.cc with
        | first ->
            first
        | exception _ ->
            true
            [@mutate
              off
                "reached only when the polite closes of the slots used the \
                 whole limit of 2 s: then the COM_QUIT of the control \
                 connection runs in a cancelled fiber"]
      then Atomic.decr t.control_sockets

let shutdown t =
  Atomic.set t.closed true ;
  let now = now t in
  let waiters, entries =
    op t (fun s ->
        let s, waiters, entries = P.close_all s now in
        (s, (waiters, entries)) )
  in
  List.iter
    (fun w -> ignore (Ivar.try_fill w (Failed Error.Connection_lost)))
    waiters ;
  (* The order of review C of the plan, M1: the waiters failed at once, then the
     drain, then the polite closes. *)
  drain t ;
  (* A polite close never raises, and it closes the flow in each case. After the
     limit of 2 s, each close runs in a cancelled fiber: a COM_QUIT that must
     wait raises at once, and its close closes the flow. *)
  Fiber.shield (fun () ->
      match
        Fiber.with_timeout Clock.Mono (Span.s 2) (fun () ->
            List.iter (polite_close t) entries ;
            close_control t )
      with
      | () ->
          ()
      | exception Fiber.Timeout ->
          () )
