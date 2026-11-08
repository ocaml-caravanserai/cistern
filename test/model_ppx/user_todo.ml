(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* The group of user and todo through the ppx (phase 4h): the type declaration
   of test/model/user_todo.ml (the suite test/ppx checks that the two are
   equal). The run-time tests of the relations run on this group. *)

type user =
  { id: int64 [@key] [@auto]
  ; name: string
  ; email: string [@unique]
  ; nick: string option [@unique]
  ; age: int option
  ; active: bool [@default true]
  ; todos: todo list Cistern.Deferred.t [@has_many] }

and todo =
  { id: int64 [@key] [@auto]
  ; user_id: int64
  ; user: user Cistern.Deferred.t [@belongs_to {key= user_id; references= id}]
  ; title: string [@index]
  ; done_: bool [@default false] [@column "done"]
  ; note: string option }
[@@deriving cistern]
