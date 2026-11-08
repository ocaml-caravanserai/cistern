(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* A filter with the fields of two models. The compiler refuses it. *)

open Cistern_core

type a

type b

let _f (x : (a, int64) Field.t) (y : (b, int64) Field.t) =
  Expr.Ops.(x ==. 1L && y ==. 2L)
