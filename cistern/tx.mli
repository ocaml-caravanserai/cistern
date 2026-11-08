(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(** A transaction: one connection of the pool for the callback of
    {!Db.transaction} (DESIGN.md, "Transactions"). Only the fiber of the
    callback can use it, and only until the end of the callback. *)

type t

val exec :
  ?timeout:Weft.Span.t -> t -> 'a Stmt.t -> ('a, Cistern_core.Error.t) result
(** [exec tx s] runs [s] in the transaction. The deadline is the earlier of
    [timeout] (default: none) and the transaction deadline.

    - A failed transaction gives its first error, with no I/O.
    - An OK with [IN_TRANS] clear means that the server committed (an implicit
      commit): the transaction fails with [Implicit_commit]. A failed DDL
      statement also commits first: after such an error, the ping of the client
      shows it, and the result is [Implicit_commit] too.
    - Some errors fail the transaction (a deadlock, [Timeout],
      [Outcome_unknown], [Connection_lost], [Read_only], …). The errors of the
      statement alone (a constraint, [Invalid_value], [Statement_timeout],
      [Not_found], [Decode]) leave it usable.
    - A cancel aborts the connection, fails the transaction and raises
      [Weft.Fiber.Cancelled].

    It raises [Invalid_argument] from another fiber than the fiber of the
    callback, after the end of the transaction, and for a raw statement whose
    first word is refused: [BEGIN], [START], [COMMIT], [ROLLBACK], [SAVEPOINT],
    [RELEASE], [XA], [LOCK], [UNLOCK], [SET], [EXECUTE], [PREPARE], or no word
    (a value, a comment or a parenthesis first). A [CALL] of a procedure that
    commits is not detected. *)

val exec_exn : ?timeout:Weft.Span.t -> t -> 'a Stmt.t -> 'a
(** [exec_exn] is {!exec}, and raises [Error.Cistern_error] for an error. *)

val savepoint : t -> (t -> 'a) -> ('a, Cistern_core.Error.t) result
(** [savepoint tx g] runs [g tx] in a [SAVEPOINT]. [g] returns:
    [RELEASE SAVEPOINT], and the value. [g] raises [Cistern_error e]:
    [ROLLBACK TO SAVEPOINT], and [Error e]. [g] raises another exception:
    [ROLLBACK TO SAVEPOINT], then the exception again. After a failure of the
    transaction in the scope, no command goes, and the first error (or the
    exception of [g]) comes back. An [Error] that [g] handled does not roll back
    the scope. *)

(** For the library ({!Db.transaction}). It can change in each version. *)
module Private : sig
  val transaction :
       ?caller:string
    -> timeout:Weft.Span.t
    -> Pool.t
    -> (t -> 'a)
    -> ('a, Cistern_core.Error.t) result

  val atomic :
       timeout:Weft.Span.t
    -> Pool.t
    -> 'a Stmt.t
    -> ('a, Cistern_core.Error.t) result
  (** [atomic ~timeout pool s] runs the atomic statement [s] of [Db.exec] in the
      transaction of [Db.transaction] (phase 4b1): [START TRANSACTION], its
      commands with one event each, and [COMMIT], or [ROLLBACK] after the first
      error of a command. The misuse checks are those of [Db.transaction], with
      the message "Db.exec in the fiber of a Tx". *)
end
