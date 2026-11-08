(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* The group of person through the ppx (see kinds.ml). *)

type person =
  { id: int64 [@key] [@auto]
  ; email: string [@unique]
  ; nick: string option [@unique] [@column "nick_name"]
  ; city: string [@index]
  ; age: int option
  ; active: bool [@default true] }
[@@table "people"] [@@deriving cistern]
