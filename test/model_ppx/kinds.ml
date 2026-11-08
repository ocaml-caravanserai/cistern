(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* The group of kinds through the ppx: the type declaration of
   test/model/kinds.ml (the suite test/ppx checks that the two are equal). *)

type kinds =
  { id: int64 [@key]
  ; i: int
  ; i_opt: int option
  ; s: string
  ; s_opt: string option
  ; b: bool
  ; b_opt: bool option
  ; f: float
  ; f_opt: float option
  ; y: bytes
  ; y_opt: bytes option
  ; t: Ptime.t
  ; t_opt: Ptime.t option }
[@@deriving cistern]
