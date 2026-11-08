(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

type col = Cistern_core.Model_ast.col = {table: string; column: string}

type lit = string

type cmp = Cistern_core.Model_ast.cmp = Eq | Ne | Lt | Gt | Le | Ge

type expr =
  | Cmp of cmp * col * lit
  | Null_safe_eq of col * lit
  | Is_null of col
  | Is_not_null of col
  | In of col * lit list
  | Like of col * lit
  | And of expr list
  | Or of expr list
  | Not of expr
  | Bool of bool

type order = {by: col; desc: bool}

type lock = No_lock | For_update | For_share

type columns = Columns of col list | One

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
  | Count_derived of select
  | Exists of select
  | Insert of {table: string; columns: string list; values: lit list}
  | Update of
      { table: string
      ; set: (string * lit) list
      ; where: expr option
      ; order: order list
      ; limit: int option }
  | Delete of
      {table: string; where: expr option; order: order list; limit: int option}
