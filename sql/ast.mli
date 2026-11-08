(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(** The SQL AST (DESIGN.md, "Schema, AST and SQL"). {!Compile} makes it from the
    model AST, with each literal already as text ({!Cistern_core.Literal.add}),
    and {!Print} gives its text for a dialect. Only the clause of a shared lock
    depends on the dialect. *)

(** A column of a table. *)
type col = Cistern_core.Model_ast.col = {table: string; column: string}

(** The text of a literal. *)
type lit = string

type cmp = Cistern_core.Model_ast.cmp = Eq | Ne | Lt | Gt | Le | Ge

type expr =
  | Cmp of cmp * col * lit
  | Null_safe_eq of col * lit
  | Is_null of col
  | Is_not_null of col
  | In of col * lit list  (** Not empty. *)
  | Like of col * lit  (** [LIKE <lit> ESCAPE '!']. *)
  | And of expr list  (** Not empty. *)
  | Or of expr list  (** Not empty. *)
  | Not of expr
  | Bool of bool

type order = {by: col; desc: bool}

type lock = No_lock | For_update | For_share

(** The columns of a [SELECT]. *)
type columns =
  | Columns of col list  (** Not empty. *)
  | One  (** [SELECT 1], in a derived table and in [EXISTS]. *)

(** [offset] with no [limit] gives [LIMIT 18446744073709551615 OFFSET m]. *)
type select =
  { columns: columns
  ; table: string
  ; where: expr option
  ; order: order list
  ; limit: int option
  ; offset: int option
  ; lock: lock }

type t =
  | Select of select
  | Count of {table: string; where: expr option}
      (** The count of the rows of [t] that [where] selects. *)
  | Count_derived of select
      (** The count of the rows of the derived table [(<select>) AS t], for a
          query with a limit or an offset. *)
  | Exists of select  (** [SELECT EXISTS (<select>)]. *)
  | Insert of {table: string; columns: string list; values: lit list}
      (** [INSERT INTO t (...) VALUES (...)]. No column gives [() VALUES ()]. *)
  | Update of
      { table: string
      ; set: (string * lit) list  (** Not empty. *)
      ; where: expr option
      ; order: order list
      ; limit: int option }
  | Delete of
      {table: string; where: expr option; order: order list; limit: int option}
