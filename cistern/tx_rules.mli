(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(** The rules of a transaction as pure functions (DESIGN.md, "Transactions", and
    the plan of phase 1.3, sections 3.5 to 3.9). [Tx] of phase 1.3 applies them.
    Internal. *)

module Error = Cistern_core.Error

(** {1 The errors of a statement} *)

(** The class of an error of a statement in a [Tx] (3.7). *)
type klass =
  | Fails  (** The [Tx] fails with the error. *)
  | Statement_only  (** The [Tx] stays usable, with no ping. *)
  | Ping  (** A [COM_PING] decides with its [IN_TRANS] flag ({!after_ping}). *)

val classify :
     usable:bool
  -> rollback_on_timeout:bool
  -> statement_deadline:bool
  -> Error.t
  -> klass
(** [classify ~usable ~rollback_on_timeout ~statement_deadline e]:

    - A connection that is not usable after the statement: [Fails], with no
      ping.
    - [Fails]: [Retryable] (1205 only when [rollback_on_timeout]), [Read_only],
      [Timeout] (except [statement_deadline]), [Outcome_unknown _],
      [Connection_lost], [Implicit_commit], 1153 ([Invalid_value] with the code
      1153), and the errors of a checkout ([Pool_exhausted], [Connect]).
    - [Statement_only]: [Constraint_violation _], [Invalid_value _],
      [Statement_timeout], [Not_found], [Decode _], and [Timeout] when
      [statement_deadline] (a margin failure of the [?timeout] of the statement,
      before the transaction deadline).
    - [Ping]: [Driver _], and 1205 when [rollback_on_timeout] is off. *)

val after_ping : in_trans:bool -> Error.t option
(** [after_ping ~in_trans] is the error that fails the [Tx] after the ping of a
    [Ping] error: [Some Implicit_commit] if [IN_TRANS] is clear (a failed DDL
    statement committed the transaction before it failed), else [None]. *)

(** {1 The first word} *)

val refused : string option -> bool
(** [refused w] is true if [Tx.exec] refuses a raw statement whose
    {!Cistern_core.Raw.first_word} is [w]: [None], and the words [BEGIN],
    [START], [COMMIT], [ROLLBACK], [SAVEPOINT], [RELEASE], [XA], [LOCK],
    [UNLOCK], [SET], [EXECUTE] and [PREPARE], in any case. *)

val dml : string option -> bool
(** [dml w] is true for the first words of a DML statement (rule D5): [INSERT],
    [UPDATE], [DELETE], [REPLACE], [SELECT], [WITH], [VALUES] and [TABLE], in
    any case. *)

(** {1 The first error} *)

(** The error that failed a [Tx]. *)
type first =
  { error: Error.t  (** What [Tx.exec] and [Tx.savepoint] give. *)
  ; final: Error.t  (** What [Db.transaction] gives (rule D5). *)
  ; do_not_retry: bool
        (** The writes can be committed, or the outcome of a statement that is
            not DML is not known: no other outcome hides it (3.6.1). *) }

val first : only_dml:bool -> deadline:bool -> Error.t -> first
(** [first ~only_dml ~deadline e] is the first error [e] of a [Tx]. If each
    write statement of the [Tx] was DML ([only_dml]), the server rolled back
    when the connection closed, thus [Outcome_unknown `Statement] becomes
    [Timeout] (a [deadline]) or [Connection_lost] in [final], and it is not
    do-not-retry. [Implicit_commit] and each other [Outcome_unknown] are
    do-not-retry. *)

(** {1 The end of the callback and of a savepoint} *)

(** The state of a [Tx] at the end. *)
type state = Good | Failed of first * [`Usable | `Aborted]

(** The outcome of the callback (the exception itself stays with the caller). *)
type outcome =
  | Value
  | Cistern of Error.t  (** It raised [Cistern_error e]. *)
  | Fiber_timeout  (** It raised [Weft.Fiber.Timeout]. *)
  | Other  (** It raised another exception. *)

type command = Rollback | Nothing

(** What the end does. *)
type action =
  | Cancel  (** Abort the connection, and raise [Cancelled]. *)
  | Reraise of command  (** The command, then raise the exception again. *)
  | Fail of command * Error.t  (** The command, then [Error e]. *)
  | Commit  (** [COMMIT] ({!commit_reply}). *)

val end_of_callback :
  cancelled:bool -> deadline_passed:bool -> state -> outcome -> action
(** [end_of_callback ~cancelled ~deadline_passed state outcome] is the priority
    rule of the plan of phase 1.3 (3.6.1). The first row that applies:

    + [cancelled] (by [check_cancel]): [Cancel].
    + [Other], or [Fiber_timeout] before the deadline: [Reraise].
    + A do-not-retry first error: [Fail] with it.
    + [Cistern e]: [Fail] with [e].
    + [deadline_passed] ([Value] or [Fiber_timeout]): [Fail] with the [final] of
      the first error, else [Timeout].
    + A first error: [Fail] with its [final].
    + Else [Commit].

    The command is [Rollback] if the connection is usable ([Good] or [`Usable]),
    else [Nothing]. *)

(** What the end of a savepoint does. *)
type savepoint_action =
  | Sp_cancel  (** No command, and [Cancelled] goes up. *)
  | Sp_reraise of bool  (** [ROLLBACK TO SAVEPOINT] if [true], then raise. *)
  | Sp_fail of bool * Error.t
      (** [ROLLBACK TO SAVEPOINT] if [true], then [Error e]. *)
  | Release  (** [RELEASE SAVEPOINT], then [Ok v]. *)

val end_of_savepoint : cancelled:bool -> state -> outcome -> savepoint_action
(** [end_of_savepoint ~cancelled state outcome] is the table of 3.9, with the
    priority rule: [cancelled] ([Sp_cancel]), then a user exception
    ([Sp_reraise], with [ROLLBACK TO] if the [Tx] is good), then a do-not-retry
    first error ([Sp_fail] with it), then [Cistern e] ([Sp_fail] with [e], with
    [ROLLBACK TO] if the [Tx] is good), then a first error ([Sp_fail] with it),
    else [Release]. [Fiber_timeout] is a user exception here: a savepoint has no
    deadline of its own. The first error is its [error], as [Tx.exec] gives it.
*)

(** {1 The replies to COMMIT} *)

(** What a [COMMIT] gives (3.8). *)
type commit =
  { result: (unit, Error.t) result
  ; rollback: bool  (** Send [ROLLBACK] first ("another error"). *)
  ; reusable: bool  (** The connection can go back as good. *) }

val commit_reply : (bool, Error.t) result -> commit
(** [commit_reply r] is the result of the [COMMIT] command [r]: [Ok in_trans]
    (the [IN_TRANS] flag of its OK), or the error of [Outcome.decide] for a
    write:

    - [Ok false]: committed, reusable. [Ok true] (not expected with
      [completion_type = 'NO_CHAIN']): committed, and the connection is not
      reusable.
    - [Retryable _] (1213, 1205, 3101) and [Driver] 1180:
      [Retryable `Rolled_back], reusable.
    - [Driver] 1317 and [Outcome_unknown _]: [Outcome_unknown `Commit], not
      reusable.
    - [Connection_lost] (the flush failed, or 4031 with the id 0 before the
      server read [COMMIT]) and [Timeout] (a write deadline): the server did not
      run [COMMIT] and rolls back at the close: the same error, not reusable.
    - Another error: [ROLLBACK] first, then the error. *)

val commit_guard : Error.t
(** The result when the outer [with_timeout] of the shield cuts [COMMIT]:
    [Outcome_unknown `Commit]. *)
