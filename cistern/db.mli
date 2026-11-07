(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(** The pool of connections (DESIGN.md, "Core user types" and "Pool"). *)

(** The pool. It is safe across fibers and domains. A use after {!with_db}
    raises [Invalid_argument]. *)
type t

val with_db :
     ?pool_size:int
  -> ?max_lifetime:Weft.Span.t
  -> ?checkout_timeout:Weft.Span.t
  -> ?on_query:(Cistern_core.Event.t -> unit)
  -> ?random:Random.State.t
  -> Cistern_mysql.Config.t
  -> (t -> 'a)
  -> 'a
(** [with_db config f] runs [f] with a pool, in a [Fiber.scope] that holds the
    opener of the connections. The defaults: [pool_size] 10, [max_lifetime] 30
    min (with a jitter of ±15 %), [checkout_timeout] 30 s (DESIGN.md, "Limits
    and defaults"). The pool opens no connection before the first {!exec}, thus
    a connect error (for example a wrong password) shows at the first [exec], as
    [Connect msg] at its deadline, and at once as a [Connect] event.

    When [f] returns or raises, the opener stops, the waiters get
    [Connection_lost], and the idle connections close (at most 2 s for all of
    them). [random] gives the jitter of the lives of the connections; the
    default is a split of the generator of the domain.

    Phase 1.2 has no [KILL]: a cancelled statement keeps running on the server
    until it ends, and its session stays until then.

    @raise Invalid_argument
      if [pool_size] is not 1 to 1000, or [max_lifetime] or [checkout_timeout]
      is not positive. *)

val exec :
  ?timeout:Weft.Span.t -> t -> 'a Stmt.t -> ('a, Cistern_core.Error.t) result
(** [exec db s] runs [s] on one connection of the pool, with a deadline of
    [timeout] (default 30 s) for the checkout and the statement (DESIGN.md,
    "Execution of one statement" and "Deadlines"). A cancel of the fiber is not
    an [Error.t]: [Weft.Fiber.Cancelled] goes up, and the pool aborts the
    connection. *)

val exec_exn : ?timeout:Weft.Span.t -> t -> 'a Stmt.t -> 'a
(** [exec_exn] is {!exec}, and raises [Error.Cistern_error] for an error. *)

(** For the tests. It can change in each version. *)
module Private : sig
  val pool : t -> Pool.t
end
