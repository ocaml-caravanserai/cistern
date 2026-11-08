(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* A filter of the fields of user in a query of todo (phase 2d2). The compiler
   refuses it. *)

open Cistern_test_models.User_todo

let _ = Todo.where User.Q.(name ==. "x")
