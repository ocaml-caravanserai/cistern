(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(** The model AST of a filter (DESIGN.md, "Schema, AST and SQL"). {!Expr} builds
    it, and [cistern.sql] compiles it to SQL. It has no types of OCaml: the
    values are {!Value.t}. Each comparison has a column on the left and a value
    on the right, so that the compiler never compares two literals. A literal
    has the case-insensitive collation of the session, and a column has a binary
    collation, which wins. *)

(** A column of a table. *)
type col = {table: string; column: string}

type cmp = Eq | Ne | Lt | Gt | Le | Ge

type t =
  | Cmp of cmp * col * Value.t
      (** [=], [<>], [<], [>], [<=] or [>=]. The value is never [Null]. *)
  | Null_safe_eq of col * Value.t  (** [<=>]. The value is never [Null]. *)
  | Is_null of col
  | Is_not_null of col
  | In of col * Value.t list
      (** [IN (...)]. The list is not empty and holds no [Null]. *)
  | Like of col * string
      (** [LIKE <pattern> ESCAPE '!']. The pattern is the text of the literal:
          [!] escapes [!], [%] and [_]. *)
  | And of t list  (** Not empty. *)
  | Or of t list  (** Not empty. *)
  | Not of t
  | Bool of bool  (** [TRUE] or [FALSE]. *)
