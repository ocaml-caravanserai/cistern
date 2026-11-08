(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* starts_with on an int64 field. The compiler refuses it. *)

open Cistern_core

type a

let _f (x : (a, int64) Field.t) = Expr.Ops.(starts_with x "1")
