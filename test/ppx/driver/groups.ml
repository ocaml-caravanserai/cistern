(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* Each Cistern attribute, in its short form and in its long form. *)

type person =
  { id: int64 [@key] [@auto]
  ; email: string [@unique]
  ; nick: string option [@unique] [@column "nick_name"]
  ; city: string [@index]
  ; active: bool [@default true]
  ; born: Ptime.t }

and tag = {name: string [@key]; note: string option [@default None]}
[@@table "tags"] [@@deriving cistern]

type other =
  { key: int [@cistern.key]
  ; c: string [@cistern.column "cc"] [@cistern.unique]
  ; d: float [@cistern.index] [@cistern.default 1.0] }
[@@cistern.table "others"] [@@deriving cistern]
