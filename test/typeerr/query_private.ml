(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* The public Query has no Private: a program cannot make a query of one model
   from the schema of another (phase 2d2). The compiler refuses it. *)

let _ = Cistern.Query.Private.make
