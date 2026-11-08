(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* An order by a field of kinds in a query of person (phase 2d2), with the
   derived models of phase 3d. The compiler refuses it. *)

open Cistern_test_models_ppx

let _ = Cistern.Query.order_by Kinds.Kinds.Q.s `Asc Person.Person.all
