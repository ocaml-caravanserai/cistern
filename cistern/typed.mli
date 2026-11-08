(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(** The statements of the typed API (DESIGN.md, "Execution of one statement").
    The SQL AST holds the literals, thus a value with no literal gives
    [Invalid_value] before the checkout. A typed statement is not raw: it does
    not mark its connection dirty. *)

val make :
     Stmt.Private.kind
  -> (Cistern_sql.Ast.t, string) result
  -> run:
       (   Exec.ctx
        -> kind:Cistern_mysql.Outcome.command
        -> Cistern_core.Raw.t
        -> ('r, Cistern_core.Error.t) result )
  -> rows:('r -> int)
  -> finish:('r -> 'a)
  -> 'a Stmt.t
(** [make kind ast ~run ~rows ~finish] is the statement of [ast].
    [run ctx ~kind sql] runs the SQL of the dialect of the connection, with the
    command kind of [kind] for {!Exec}. [Error msg] of the compiler gives
    [Invalid_value {code = None; message = msg}], with no byte sent. A read
    ([Read]) that has no reply gives [Connection_lost], and a write
    [Outcome_unknown]. *)
