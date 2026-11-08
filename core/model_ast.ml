(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

type col = {table: string; column: string}

type cmp = Eq | Ne | Lt | Gt | Le | Ge

type t =
  | Cmp of cmp * col * Value.t
  | Null_safe_eq of col * Value.t
  | Is_null of col
  | Is_not_null of col
  | In of col * Value.t list
  | Like of col * string
  | And of t list
  | Or of t list
  | Not of t
  | Bool of bool
