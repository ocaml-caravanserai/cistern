(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(** The context of one statement: the one connection that the pool gave it
    (DESIGN.md, "Execution of one statement"). A statement runs all its commands
    on it, in order. *)

type ctx

val make : Cistern_mysql.Connection.t -> ctx

val query :
     ?kind:Cistern_mysql.Outcome.command
  -> ctx
  -> Cistern_core.Raw.t
  -> (Cistern_core.Raw.result, Cistern_core.Error.t) result

val exec :
     ?kind:Cistern_mysql.Outcome.command
  -> ctx
  -> Cistern_core.Raw.t
  -> (Cistern_core.Exec_result.t, Cistern_core.Error.t) result

val sent : ctx -> bool
(** [sent ctx] is true if a command of this statement flushed its packet: an
    earlier command that returned, or the command in progress
    ([Connection.sent]). The pool reads it after a cancel in the statement,
    before it aborts the connection. *)
