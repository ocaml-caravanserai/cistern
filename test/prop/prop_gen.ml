(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* The generators that the property tests share. *)

open Windtrap

(* [n] decimal digits. *)
let digits n = Gen.string_of ~size:(Gen.constant n) (Gen.char_range '0' '9')
