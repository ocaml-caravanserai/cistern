(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(** The text of a SQL AST (DESIGN.md, "Schema, AST and SQL"). One serializer
    gives the SQL of the statement and the redacted copy of the events. *)

val ident : string -> string
(** [ident s] is [s] between backquotes, with each backquote in it doubled. *)

val to_sql : Cistern_core.Dialect.t -> Ast.t -> string
(** [to_sql d s] is the SQL of [s] for the dialect [d], with the literals. Each
    identifier is between backquotes, with a backquote in it doubled. A literal
    has a space or a parenthesis before it, and a space, a comma, a parenthesis
    or the end after it, so that it never joins the text next to it. *)

val redacted : Cistern_core.Dialect.t -> Ast.t -> string
(** [redacted d s] is {!to_sql} with [?] in place of each literal: the SQL of an
    event (DESIGN.md, "Shared state and the query hook"). The numbers of [LIMIT]
    and [OFFSET] stay. *)
