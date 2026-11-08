(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* Two groups in one file (D14): each group has its own alias
   Cistern__private__<first> and its own modules. *)

type note = {id: int64 [@key] [@auto]; text: string} [@@deriving cistern]

type label = {name: string [@key]; color: string option} [@@deriving cistern]
