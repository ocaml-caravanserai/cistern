(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* The hostile names of D17 (the plan of phase 3): user modules List, Option,
   Fun and Q before the group (the defaults name Q), field names that are local
   names or values of the generated code, short type names, and SQL names: the
   table "order", the columns "select", "key" and "desc", a column with a space,
   with a quote, with a double quote and a backslash, a column with a non-ASCII
   letter, and a column of 64 characters with [@unique] (its index name gets the
   hash). The integration suite runs it on the three servers. *)

module List = struct
  let filter_map = ()
end

module Option = struct
  let map = ()
end

module Fun = struct
  let id = ()
end

module Q = struct
  let v = 7

  let d = "q"
end

type t =
  { c: int64 [@key] [@auto]
  ; r: string [@unique] [@column "select"]
  ; v: int [@default Q.v] [@column "key"]
  ; k: string option [@index] [@column "desc"]
  ; d: string [@default Q.d] [@column "a b"]
  ; e: bool [@column "it's"] }
[@@table "order"]

and s =
  { draft: int64 [@key]
  ; schema: string [@unique] [@column "q\"uote\\back"]
  ; s_name: string [@column "name"]
  ; name: string option [@column "s_name"] [@unique]
  ; model: int [@index] [@column "caf\xc3\xa9"] }

and f =
  { update: string [@key]
  ; key: int
        [@unique]
        [@column
          "cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc"]
  ; all: int option [@default None] }

and d = {where: int64 [@key] [@auto]; delete: int}

and m = {fields: int64 [@key] [@auto]; decode: string}

and u = {cistern: int64 [@key] [@auto]} [@@deriving cistern]
