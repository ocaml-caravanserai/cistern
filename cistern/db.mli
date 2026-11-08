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

    TLS (phase 1.4) takes its randomness from the default generator of
    mirage-crypto-rng. Set it one time before [with_db], with
    [Mirage_crypto_rng_unix.use_default ()]: that generator is safe on many
    domains, and a Fortuna generator is not (the pool connects from fibers on
    many domains).

    @raise Invalid_argument
      if [pool_size] is not 1 to 1000, or [max_lifetime] or [checkout_timeout]
      is not positive, or if [config] needs TLS and no default generator is set
      (before any fiber starts). *)

val exec :
  ?timeout:Weft.Span.t -> t -> 'a Stmt.t -> ('a, Cistern_core.Error.t) result
(** [exec db s] runs [s] on one connection of the pool, with a deadline of
    [timeout] (default 30 s) for the checkout and the statement (DESIGN.md,
    "Execution of one statement" and "Deadlines"). A cancel of the fiber is not
    an [Error.t]: [Weft.Fiber.Cancelled] goes up, and the pool aborts the
    connection. *)

val push_schema :
     ?timeout:Weft.Span.t
  -> t
  -> (module Cistern_core.Model_intf.MODEL) list
  -> unit
(** [push_schema db models] makes the tables of [models] that do not exist
    (DESIGN.md, "DDL"):

    + {!Cistern_core.Schema.check} of the models. Its messages raise
      [Invalid_argument], with no connect.
    + Its own connection, outside the pool (each call that runs adds one session
      on the server), with [kill=false] (no round trip 3), a [Connect] event,
      and the deadline [timeout] (default 30 s) for the connect and the
      statements.
    + [SET foreign_key_checks = 0, lock_wait_timeout = <n>]: [n] is the time
      left at the [SET] in whole seconds, minus 1 s, rounded up, at least 1.
      Thus the server gives 1205 for a metadata lock of the first table before
      the deadline. For a later table, less time is left, and the deadline can
      come first ([Timeout]).
    + One [CREATE TABLE IF NOT EXISTS] for each model, in order, each with a
      [Query] event. It never changes a table that exists.
    + The close of the connection.

    An error raises [Cistern_error]: [Connect] for a failed connect, [Timeout]
    at the deadline, [Retryable `Lock_timeout] for 1205, and the error of the
    server for a failed statement. The connection is then aborted. The tables of
    the statements before the error exist, and [push_schema] can run again. An
    empty list does nothing.

    @raise Invalid_argument
      after {!with_db}, in the fiber of a [Tx] (DDL commits the transaction and
      waits for its metadata lock), or for a model that the checks refuse. *)

val check_schema :
  t -> (module Cistern_core.Model_intf.MODEL) list -> (unit, string list) result
(** [check_schema db models] compares the tables of the database with [models]
    (DESIGN.md, "DDL"). [Error] holds the messages of
    {!Cistern_core.Schema.check}, or one message for each difference of
    {!Cistern_sql.Ddl.compare}. It reads [information_schema.COLUMNS] with one
    statement through the pool (with a [Query] event, and the names of the
    tables as [?]). An empty list gives [Ok ()]. A table that the user of the
    pool has no privilege on is not in [information_schema] for this user, thus
    it shows as missing.

    @raise Cistern_error
      for an error of the pool, the connection or the server: it is not a
      difference.
    @raise Invalid_argument after {!with_db} or in the fiber of a [Tx]. *)

val transaction :
  ?timeout:Weft.Span.t -> t -> (Tx.t -> 'a) -> ('a, Cistern_core.Error.t) result
(** [transaction db f] runs [f] in a transaction on one connection of the pool
    (DESIGN.md, "Transactions"). [timeout] (default 30 s) is one deadline for
    the checkout, [START TRANSACTION], the callback and its statements. The end:

    - [f] returns before the deadline, and the transaction did not fail:
      [COMMIT], and the result of the [COMMIT].
    - [f] raises an exception that is not [Cistern_error]: [ROLLBACK], then the
      exception again (also [Weft.Fiber.Timeout] before the deadline).
    - The transaction failed with [Implicit_commit] (or an [Outcome_unknown]
      that a retry cannot fix): [ROLLBACK], and that error. It wins over
      [Cistern_error] and over the deadline, because the writes before it can be
      committed: do not run the transaction again.
    - [f] raises [Cistern_error e]: [ROLLBACK], and [Error e].
    - The deadline passed (also when [f] returns a value after it): [ROLLBACK],
      and [Timeout], or the first error of a failed transaction.
    - The transaction failed: [ROLLBACK], and its first error.
    - The fiber is cancelled: the connection is aborted (the server rolls back),
      and [Weft.Fiber.Cancelled] goes up.

    [COMMIT] and [ROLLBACK] run in a shield, with their own deadline of 5 to 30
    s. A child fiber of [f] is not the fiber of the transaction: its [Db.exec]
    waits for a second connection (with [pool_size = 1], until
    [checkout_timeout]).

    @raise Invalid_argument
      in the fiber of a transaction, and after the end of {!with_db}. *)

val exec_exn : ?timeout:Weft.Span.t -> t -> 'a Stmt.t -> 'a
(** [exec_exn] is {!exec}, and raises [Error.Cistern_error] for an error. *)

(** For the tests. It can change in each version. *)
module Private : sig
  val pool : t -> Pool.t
end
