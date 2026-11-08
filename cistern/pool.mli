(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(** The weft shell of the pool (DESIGN.md, "Pool", "Deadlines", "Cancel" and
    "Shared state and the query hook"). {!Db} is its public face.

    A stdlib [Mutex] protects the {!Pool_state} value and the random state of
    the jitter. In its section the code runs one pure operation, with no [sync],
    no I/O and no fill. Then, in this order: the fills of the waiters, the
    doorbell of the opener, the abort of a connection that an [opened] on a
    closed pool gives, and the events. An abort always runs before the operation
    that frees its slot, thus the client never has more sockets than
    [pool_size]. *)

type t

val create :
     ?random:Random.State.t
  -> ?on_query:(Cistern_core.Event.t -> unit)
  -> pool_size:int
  -> max_lifetime:Weft.Span.t
  -> checkout_timeout:Weft.Span.t
  -> Cistern_mysql.Config.t
  -> t
(** [create ...] is an open pool with no connection. It reads [Time.now Mono]:
    call it in a fiber.

    @raise Invalid_argument
      if [pool_size] is not 1 to 1000, or [max_lifetime] or [checkout_timeout]
      is not positive. *)

val opener : t -> unit
(** [opener t] is the body of the opener daemon: it starts the connects (at most
    2 at a time, in a scope of its own), does the polite closes, and waits for
    its doorbell or its timer. Only a cancel ends it. *)

(** {1 One held connection}

    [with_conn] is the checkout and the checkin of {!exec}, for a function that
    runs on the held connection: one statement ({!exec}), or a transaction
    (phase 1.3). The plan of phase 1.3, section 3.11. *)

(** The held connection. [dirty] is the dirty mark after the session reset: a
    successful reset makes the entry clean. *)
type held = {c: Cistern_mysql.Connection.t; dirty: bool}

(** The end of the entry, that the function gives. *)
type held_end =
  | Give_back of {dirty: bool}
      (** The checkin as good if the connection is usable and [IN_TRANS] is
          clear, else the abort first. *)
  | Discard  (** The abort, then the checkin as not reusable. *)

(** The outcome of the function, as data: the function never raises for an
    outcome of the user. *)
type 'a fn_outcome =
  | Value of 'a
  | Err of Cistern_core.Error.t
  | Reraise of exn * Printexc.raw_backtrace  (** A user exception. *)
  | Cancel of exn  (** A [Cancelled]. *)

(** The outcome of [with_conn]: the steps before the function, or the outcome of
    the function with its pending event. The caller emits the event, then gives
    the result or raises. *)
type 'a before =
  | Checkout_failed of Cistern_core.Error.t
      (** The error of a waiter at its limit, or the [Failed] of the shutdown.
      *)
  | Margin of Cistern_mysql.Connection.t
      (** Less than the margin was left. The entry went back as good. *)
  | Reset_failed of Cistern_mysql.Connection.t * Cistern_core.Error.t
      (** A reset that passed the deadline, or the second failed reset. *)
  | Cancelled_before of exn
      (** A cancel in the wait (no connection) or in the reset. A late entry
          went back as good, and the entry of a reset was discarded. *)
  | Ran of 'a fn_outcome * (unit -> unit)
      (** The function ran: its outcome and its pending event, after the
          checkin. *)

val with_conn :
     t
  -> start:Weft.Span.t
  -> deadline:Weft.Span.t
  -> (held -> 'a fn_outcome * (unit -> unit) * held_end)
  -> 'a before
(** [with_conn t ~start ~deadline f] does the checkout (the waiter limit is the
    earlier of [deadline] and [start] plus [checkout_timeout], with the rules of
    1.2), the stale check, the flow deadline, the session reset of a dirty entry
    with the margin before and after it, then [f] on the held connection, then
    the checkin of the end that [f] gives. [start] and [deadline] are times of
    the pool ({!now}). Only a bug of Cistern raises out of [f]: then the entry
    is discarded, and the exception goes up.

    @raise Invalid_argument
      "Db.t used after with_db" at a checkout on a closed pool. *)

val now : t -> Weft.Span.t
(** The time of the pool: the span since its origin. *)

val mono : t -> Weft.Span.t -> Weft.Clock.mono option
(** [mono t span] is the [Mono] time of the time [span] of the pool. *)

val time_left : t -> deadline:Weft.Span.t -> bool
(** [time_left t ~deadline] is true if [deadline] leaves the margin of 50 ms. *)

val emit : t -> Cistern_core.Event.t -> unit
(** [emit t e] gives [e] to [on_query], and catches its exceptions. *)

val abort_conn : t -> Cistern_mysql.Connection.t -> unit
(** [abort_conn t c] aborts the held connection [c], with the count of the
    sockets. Then the end of the holder must be [Discard]. *)

val config : t -> Cistern_mysql.Config.t
(** The configuration of the pool. *)

val address : Cistern_mysql.Config.t -> string
(** The address of a [Connect] event: [host:port] or the path of the socket. *)

val check_open : t -> unit
(** @raise Invalid_argument "Db.t used after with_db" after {!shutdown}. *)

(** {1 The fibers that hold a transaction (phase 1.3)} *)

val enter_tx : t -> Weft.Fiber.Id.t -> unit

val leave_tx : t -> Weft.Fiber.Id.t -> unit

val in_tx : t -> bool
(** [in_tx t] is true if the fiber that calls it holds a [Tx] of [t]. *)

val tx_fibers : t -> int
(** The count of the fibers that hold a [Tx]. *)

val add_span : Weft.Span.t -> Weft.Span.t -> Weft.Span.t
(** A saturated sum: a very large span gives the largest time. *)

val exec :
  t -> timeout:Weft.Span.t -> 'a Stmt.t -> ('a, Cistern_core.Error.t) result
(** [exec t ~timeout s] runs [s] on one connection (DESIGN.md, "Execution of one
    statement"), and emits exactly one [Query] event, except on the paths of a
    misuse ([Invalid_argument]) or of a bug exception.

    @raise Invalid_argument
      "Db.t used after with_db" after {!shutdown}, and "Db.exec in the fiber of
      a Tx" in the fiber of a [Tx]. *)

val killer : t -> unit
(** [killer t] is the body of the killer daemon (DESIGN.md, "Cancel"): it opens
    its control connection at once (with the backoff of the connects after a
    failure), and sends [KILL CONNECTION] for each entry of the kill queue.
    Before each entry, a control connection past its idle limit or stale is
    closed and opened again. Each try runs in its own shield with a limit of 2
    s. After a failure, the control connection is aborted and opened again
    outside the shields, then the second try runs; after the second failure the
    entry is dropped. A 1095 turns [KILL] off for the pool. The events come
    after the shields. Only a cancel ends it. Start it only with [kill=true]. *)

val shutdown : t -> unit
(** [shutdown t] closes the pool, after the end of the opener and the killer:
    the waiters get [Connection_lost] at once. Then the drain (DESIGN.md,
    "Cancel", step 10): in a shield with a limit of 15 s, it sends the entries
    of the kill queue, also those that come during it, and then one mutex
    section ends it (after the limit, the entries that remain get
    [Kill_dropped Drain_timeout]). Then the idle connections, those to close and
    the control connection get a polite close, in a shield with one limit of 2 s
    for all of them. After the limit, a COM_QUIT that must wait raises at once,
    and its flow closes. A connection that comes back later is aborted at its
    checkin, and a kill request after the drain frees its slot with
    [Kill_dropped Closed]. *)

(** For the tests only. It can change in each version. [after_op], [trace] and
    the counters are global values: all the pools of a process share them, thus
    a test that reads them runs alone. *)
module Private : sig
  val on_hold : (Cistern_mysql.Connection.t -> unit) ref
  (** Called with the connection of each holder, right before the function of
      the holder runs. The pool storm checks with it that no session of the fake
      server is in a transaction when the pool hands it to a holder (phase 4b1).
      It must not raise, wait or see a cancel. The default does nothing. *)

  val after_op : (unit -> unit) ref
  (** Called between the mutex section of an operation and its effects. A test
      sets it to a short shielded sleep, so that the timer of a waiter can fire
      and another fiber can run in that window. It must not raise and must not
      see a cancel. The default does nothing. *)

  val before_try : (unit -> unit) ref
  (** Called by the killer after it read the head of the kill queue, before the
      shield of the try. A test sets it to a shielded sleep, so that another
      fiber can drop the entry in that window. It must not raise. The default
      does nothing. *)

  val connect_control :
    (   Cistern_mysql.Config.t
     -> (Cistern_mysql.Connection.t, Cistern_core.Error.t) result )
    ref
  (** The connect of the control connection of the killer: [Connection.connect]
      by default. A storm sets it to refuse only the control connection for a
      time (phase 1.5c2). *)

  val trace : (string -> unit) ref
  (** Called with ["fill"] before the fill of a waiter, and with ["ring"] before
      the doorbell of the opener. A test reads the order of the effects with it.
      The default does nothing. *)

  (** The counts of the race branches. *)
  type counters =
    { waiter_lost: int  (** A waiter abandoned, but an entry had come. *)
    ; checkin_lost: int  (** A hand-off found its waiter abandoned. *)
    ; cancel_in_reset: int  (** A cancel during a session reset. *)
    ; expired_at_checkout: int  (** Idle entries too old at a checkout. *)
    ; stale_at_checkout: int  (** Idle entries that the stale check found. *)
    ; cancel_in_stale: int
          (** A cancel in the stale check (at the lock of a TLS session). *)
    ; kill_dropped: int  (** The [Kill_dropped] events. *)
    ; kill_requests: int
          (** The kill requests that went to the queue, or that a full queue, a
              duplicate or the end of the drain dropped: each one gives exactly
              one [Kill] or [Kill_dropped] event. *)
    ; kill_retry: int  (** A first try of a [KILL] that failed. *)
    ; control_reopened: int
          (** A control connection past its idle limit, or stale. *) }

  val counters : unit -> counters

  val reset_counters : unit -> unit

  val sockets : t -> int
  (** The open sockets of the client. *)

  val kill_off : t -> bool
  (** [KILL] is off for the pool: a connection gave a refused identity, or a
      [KILL] gave 1095. *)

  val queue : t -> int64 list
  (** The connection ids of the kill queue, the oldest first. *)

  val control : t -> int
  (** The open control connections: 0 or 1. *)

  val killing : t -> int
  (** The [Killing] slots, also those past their age limit that no operation
      removed. *)

  val over_bound : t -> int
  (** The count of the opens that found more than [pool_size] sockets: 0 in a
      correct run. *)

  val idle_expiry : t -> Weft.Span.t option list
  (** The end of life of each idle entry, as a span from the origin of the pool.
  *)

  val state : t -> int * int * int * int * int
  (** idle, in use, opening, closing, waiting. *)
end
