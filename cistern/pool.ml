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
  ; txs: Fibers.t Atomic.t }

module Private = struct
  let after_op = ref ignore

  let trace = ref ignore

  type counters =
    { waiter_lost: int
    ; checkin_lost: int
    ; cancel_in_reset: int
    ; expired_at_checkout: int
    ; stale_at_checkout: int }

  let waiter_lost = Atomic.make 0

  let checkin_lost = Atomic.make 0

  let cancel_in_reset = Atomic.make 0

  let expired_at_checkout = Atomic.make 0

  let stale_at_checkout = Atomic.make 0

  let counters () =
    { waiter_lost= Atomic.get waiter_lost
    ; checkin_lost= Atomic.get checkin_lost
    ; cancel_in_reset= Atomic.get cancel_in_reset
    ; expired_at_checkout= Atomic.get expired_at_checkout
    ; stale_at_checkout= Atomic.get stale_at_checkout }

  let reset_counters () =
    List.iter
      (fun a -> Atomic.set a 0)
      [ waiter_lost
      ; checkin_lost
      ; cancel_in_reset
      ; expired_at_checkout
      ; stale_at_checkout ]

  let sockets t = Atomic.get t.sockets

  let over_bound t = Atomic.get t.over_bound

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

let emit t ev = try t.on_query ev with _ -> ()

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
  | Ok c ->
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
      event None
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
          let s, _found, err = P.cancel_wait s at id in
          (s, err) )
    in
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
        | Give_back {dirty} ->
            let reusable = Connection.usable c && not (in_trans c) in
            if not reusable then abort t e.conn ;
            give_back ~reusable ~dirty
        | Discard ->
            discard t e ) ;
        Ran (outcome, event)
  in
  get 0

let exec t ~timeout stmt =
  (* Before each other check: a misuse emits no event. *)
  check_open t ;
  if in_tx t then invalid_arg "Db.exec in the fiber of a Tx" ;
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
        [@mutate off "the function of exec gives no Reraise"] )

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
  (* A polite close never raises, and it closes the flow in each case. After the
     limit of 2 s, each close runs in a cancelled fiber: a COM_QUIT that must
     wait raises at once, and its close closes the flow. *)
  Fiber.shield (fun () ->
      match
        Fiber.with_timeout Clock.Mono (Span.s 2) (fun () ->
            List.iter (polite_close t) entries )
      with
      | () ->
          ()
      | exception Fiber.Timeout ->
          () )
