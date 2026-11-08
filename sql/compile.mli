(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(** The compiler of the typed statements (DESIGN.md, "Schema, AST and SQL" and
    "Execution of one statement"). It makes the SQL AST of each statement from
    the model AST, with each literal as text. Thus a value with no literal gives
    [Error msg] when the statement is made, before the checkout. [msg] never
    contains the value. The caller makes
    [Invalid_value {code = None; message = msg}] from it.

    The columns of a [SELECT] are the fields of the model, in order, each with
    its table. With a limit or an offset, the reads and the bulk writes append
    the key as the last key of the order, if the order does not have it yet:
    thus two pages never hold the same row.

    The model AST can come from code other than {!Cistern_core.Expr}. Thus an
    [In] with no value gives [FALSE], an [And] with no filter [TRUE], and an
    [Or] with no filter [FALSE]: the SQL never has [IN ()], [()] or an empty
    [AND] or [OR]. *)

open Cistern_core

type direction = Asc | Desc

(** A query (DESIGN.md, "Core user types"): [Query.t] of [cistern] holds one.
    Each function of a query raises [Invalid_argument] for a negative [limit] or
    [offset]. *)
type query =
  { model: Schema.model
  ; where: Model_ast.t list  (** The filters, joined with [AND]. *)
  ; order: (Model_ast.col * direction) list
  ; limit: int option
  ; offset: int option
  ; lock: Ast.lock }

val all : Schema.model -> query
(** [all m] is the query of all the rows of [m]: no filter, no order, no limit,
    no offset and no lock. *)

val select : query -> (Ast.t, string) result
(** The rows of the query ([to_list]). *)

val first : query -> (Ast.t, string) result
(** The first row: [select] with [LIMIT 1], or with the limit of the query if it
    is 0. *)

val count : query -> (Ast.t, string) result
(** The count of the rows. With no limit and no offset, the count has no order.
    With a limit or an offset, it counts the rows of a derived table. [count]
    ignores the lock. *)

val exists : query -> (Ast.t, string) result
(** [SELECT EXISTS (SELECT 1 ...)], with the order only if a limit or an offset
    is in the query. [exists] ignores the lock. *)

val get : Schema.model -> Value.t -> (Ast.t, string) result
(** [get m k] is the row with the key [k]: [WHERE <key> = <k> LIMIT 1], with no
    order. [find] has the same SQL. *)

val insert : Schema.model -> (string * Value.t) list -> (Ast.t, string) result
(** [insert m cols] is the [INSERT] of the columns [cols] and their values. No
    column gives [INSERT INTO t () VALUES ()]. *)

val update :
     Schema.model
  -> key:Value.t
  -> (string * Value.t) list
  -> (Ast.t, string) result
(** [update m ~key cols] is the [UPDATE] of the row with the key [key].

    @raise Invalid_argument if [cols] is empty. *)

val delete : Schema.model -> key:Value.t -> (Ast.t, string) result
(** [delete m ~key] is the [DELETE] of the row with the key [key]. *)

val update_all : query -> (string * Value.t) list -> (Ast.t, string) result
(** [update_all q cols] is the [UPDATE] of the rows of [q], with the order and
    the limit of [q]. With no filter, it changes the whole table.

    @raise Invalid_argument
      if [cols] is empty, or if [q] has an offset or a lock (MySQL has no
      [UPDATE ... OFFSET]). *)

val delete_all : query -> (Ast.t, string) result
(** [delete_all q] is the [DELETE] of the rows of [q], with the order and the
    limit of [q]. With no filter, it deletes all the rows.

    @raise Invalid_argument if [q] has an offset or a lock. *)
