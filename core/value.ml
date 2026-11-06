(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

type t =
  | Null
  | Int of int64
  | Float of float
  | Text of string
  | Bytes of string
  | Datetime of Ptime.t
  | Decimal of Decimal.t

let equal a b =
  match (a, b) with
  | Null, Null ->
      true
  | Int x, Int y ->
      Int64.equal x y
  | Float x, Float y ->
      Int64.equal (Int64.bits_of_float x) (Int64.bits_of_float y)
  | Text x, Text y ->
      String.equal x y
  | Bytes x, Bytes y ->
      String.equal x y
  | Datetime x, Datetime y ->
      Ptime.equal x y
  | Decimal x, Decimal y ->
      Decimal.equal x y
  | (Null | Int _ | Float _ | Text _ | Bytes _ | Datetime _ | Decimal _), _ ->
      false
