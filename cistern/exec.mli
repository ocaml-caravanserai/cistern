(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(** The context of one statement: the one connection that the pool gave it
    (DESIGN.md, "Execution of one statement"). A statement runs all its commands
    on it, in order. *)

type ctx

(** The scope of a statement with one event for each command (phase 4b1, the
    plan of phase 4, E7): the emitter of the scope, and the margin check before
    each command. *)
type scope =
  { emit: sql:string -> t0:Weft.Clock.mono -> Cistern_core.Event.outcome -> unit
        (** One [Query] event with its redacted SQL, duration and outcome. *)
  ; time_left: unit -> bool
        (** The margin of the deadline: [false] gives [Timeout], with no byte
            sent. *)
  ; after_ok: unit -> (unit, Cistern_core.Error.t) result
        (** The check after the OK of a command: an error fails the command (for
            example [Implicit_commit] if the server ended the transaction). *)
  }

val make : ?scope:scope -> Cistern_mysql.Connection.t -> ctx
(** With [scope], each command checks the margin first, then emits its own event
    ({!scope}). The [sent] flag of a [Cancelled] event of a command is of that
    command only. With no scope, the caller emits one event for the statement.
*)

val query :
     ?kind:Cistern_mysql.Outcome.command
  -> ?redacted:string
  -> ctx
  -> Cistern_core.Raw.t
  -> (Cistern_core.Raw.result, Cistern_core.Error.t) result
(** [redacted] is the SQL of the event of the command in a scope. *)

val exec :
     ?kind:Cistern_mysql.Outcome.command
  -> ?redacted:string
  -> ctx
  -> Cistern_core.Raw.t
  -> (Cistern_core.Exec_result.t, Cistern_core.Error.t) result

val rows :
     ?kind:Cistern_mysql.Outcome.command
  -> ?redacted:string
  -> ctx
  -> Cistern_core.Raw.t
  -> (string option array -> ('a, string) result)
  -> ('a list, Cistern_core.Error.t) result
(** [rows ctx r decode] is {!Cistern_mysql.Connection.query_rows}: [decode] of
    each row. *)

val dialect : ctx -> Cistern_core.Dialect.t
(** The dialect of the connection. *)

val sent : ctx -> bool
(** [sent ctx] is true if a command of this statement flushed its packet: an
    earlier command that returned, or the command in progress
    ([Connection.sent]). The pool reads it after a cancel in the statement,
    before it aborts the connection. *)
