(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* An order by a field of todo in a query of user (phase 2d2). The compiler
   refuses it. *)

open Cistern_test_models.User_todo

let _ = Cistern.Query.order_by Todo.Q.title `Asc User.all
