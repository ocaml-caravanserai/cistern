(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* Valid UTF-8 from code points, for the generators. *)

open Windtrap

let utf8 =
  Gen.map
    (fun us ->
      let b = Buffer.create 16 in
      List.iter (Buffer.add_utf_8_uchar b) us ;
      Buffer.contents b )
    (Gen.list ~size:(Gen.int_range 0 6) Gen.uchar)
