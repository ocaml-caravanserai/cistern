(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* The payload of [@default] has the type of its field (D10): the error is at
   the payload. *)

type t = {id: int64 [@key]; flag: bool [@default 1]} [@@deriving cistern]
