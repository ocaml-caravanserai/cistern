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

val exec :
  t -> timeout:Weft.Span.t -> 'a Stmt.t -> ('a, Cistern_core.Error.t) result
(** [exec t ~timeout s] runs [s] on one connection (DESIGN.md, "Execution of one
    statement"), and emits exactly one [Query] event, except on the paths of a
    misuse ([Invalid_argument]) or of a bug exception.

    @raise Invalid_argument "Db.t used after with_db" after {!shutdown}. *)

val shutdown : t -> unit
(** [shutdown t] closes the pool, after the end of the opener: the waiters get
    [Connection_lost], and the idle connections and those to close get a polite
    close, in a shield with one limit of 2 s for all of them. After the limit, a
    COM_QUIT that must wait raises at once, and its flow closes. A connection
    that comes back later is aborted at its checkin. *)

(** For the tests only. It can change in each version. [after_op], [trace] and
    the counters are global values: all the pools of a process share them, thus
    a test that reads them runs alone. *)
module Private : sig
  val after_op : (unit -> unit) ref
  (** Called between the mutex section of an operation and its effects. A test
      sets it to a short shielded sleep, so that the timer of a waiter can fire
      and another fiber can run in that window. It must not raise and must not
      see a cancel. The default does nothing. *)

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
    ; stale_at_checkout: int  (** Idle entries that the stale check found. *) }

  val counters : unit -> counters

  val reset_counters : unit -> unit

  val sockets : t -> int
  (** The open sockets of the client. *)

  val over_bound : t -> int
  (** The count of the opens that found more than [pool_size] sockets: 0 in a
      correct run. *)

  val idle_expiry : t -> Weft.Span.t option list
  (** The end of life of each idle entry, as a span from the origin of the pool.
  *)

  val state : t -> int * int * int * int * int
  (** idle, in use, opening, closing, waiting. *)
end
