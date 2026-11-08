(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(** [Db.push_schema] and [Db.check_schema] (DESIGN.md, "DDL"). *)

module type MODEL = Cistern_core.Model_intf.MODEL

val lock_wait_timeout : remaining:Weft.Span.t -> int
(** The [lock_wait_timeout] of [push_schema], in whole seconds: [remaining] (the
    time left at the [SET]) minus 1 s, rounded up, at least 1 (MariaDB reads 0
    as "no wait"). Thus the server gives its error of a metadata lock of the
    first table before the deadline of the flow. For a later table, the deadline
    can come first. *)

val push_schema : ?timeout:Weft.Span.t -> Pool.t -> (module MODEL) list -> unit

val check_query : Cistern_core.Schema.model list -> Cistern_core.Raw.t
(** The read of [check_schema]: the columns of [information_schema.COLUMNS] of
    {!Cistern_sql.Ddl.column} for the tables of the models, in the order of the
    tables and of the columns. The list is not empty. *)

val check_schema :
     exec:
       (   (Cistern_core.Dialect.t * Cistern_sql.Ddl.column list) Stmt.t
        -> ( Cistern_core.Dialect.t * Cistern_sql.Ddl.column list
           , Cistern_core.Error.t )
           result )
  -> Pool.t
  -> (module MODEL) list
  -> (unit, string list) result
(** [exec] is [Db.exec] with its default deadline. *)
