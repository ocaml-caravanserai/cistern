(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

type item =
  { id: int64 [@key] [@auto]
  ; name: string [@unique]
  ; note: string option [@default None] }

and tag = {label: string [@key]; item_id: int64 [@index]} [@@deriving cistern]
