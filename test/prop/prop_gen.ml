(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* The generators that the property tests share, and a reference of UTF-8 that
   does not use the stdlib check. *)

open Windtrap

(* [n] decimal digits. *)
let digits n = Gen.string_of ~size:(Gen.constant n) (Gen.char_range '0' '9')

(* Valid UTF-8 from code points. *)
let utf8 =
  Gen.map
    (fun us ->
      let b = Buffer.create 16 in
      List.iter (Buffer.add_utf_8_uchar b) us ;
      Buffer.contents b )
    (Gen.list Gen.uchar)

(* Valid UTF-8 with one byte changed to another byte. Most of these strings are
   not valid, but they are near valid text. *)
let changed_utf8 =
  let open Gen in
  let* s = utf8 in
  let* c = char in
  let+ i = int_range 0 (Int.max 0 (String.length s - 1)) in
  if s = "" then String.make 1 c
  else String.mapi (fun j x -> if j = i then c else x) s

(* Text for a check of UTF-8: any bytes, valid UTF-8, and nearly valid UTF-8. *)
let text = Gen.one_of [Gen.string; utf8; changed_utf8]

(* The well-formed byte sequences of UTF-8 (The Unicode Standard, table 3-7):
   the range of the first byte, and the ranges of the next bytes. *)
let sequences =
  [ (0x00, 0x7F, [])
  ; (0xC2, 0xDF, [(0x80, 0xBF)])
  ; (0xE0, 0xE0, [(0xA0, 0xBF); (0x80, 0xBF)])
  ; (0xE1, 0xEC, [(0x80, 0xBF); (0x80, 0xBF)])
  ; (0xED, 0xED, [(0x80, 0x9F); (0x80, 0xBF)])
  ; (0xEE, 0xEF, [(0x80, 0xBF); (0x80, 0xBF)])
  ; (0xF0, 0xF0, [(0x90, 0xBF); (0x80, 0xBF); (0x80, 0xBF)])
  ; (0xF1, 0xF3, [(0x80, 0xBF); (0x80, 0xBF); (0x80, 0xBF)])
  ; (0xF4, 0xF4, [(0x80, 0x8F); (0x80, 0xBF); (0x80, 0xBF)]) ]

(* [valid_utf_8 s] is true if [s] is a series of well-formed sequences. *)
let valid_utf_8 s =
  let n = String.length s in
  let byte i = Char.code s.[i] in
  let rec at i =
    i = n
    ||
    match
      List.find_opt (fun (lo, hi, _) -> lo <= byte i && byte i <= hi) sequences
    with
    | None ->
        false
    | Some (_, _, next) ->
        follow (i + 1) next
  and follow i = function
    | [] ->
        at i
    | (lo, hi) :: rest ->
        i < n && lo <= byte i && byte i <= hi && follow (i + 1) rest
  in
  at 0

(* A time in the years 0 to 9999, with more cases near the year 1000. *)
let time =
  let open Gen in
  let* y = frequency [(3, int_range 0 9999); (1, int_range 990 1010)] in
  let* mo = int_range 1 12 in
  let* d = int_range 1 28 in
  let* h = int_range 0 23 in
  let* mi = int_range 0 59 in
  let* s = int_range 0 59 in
  let+ ps = int64_range 0L 999_999_999_999L in
  let t = Option.get (Ptime.of_date_time ((y, mo, d), ((h, mi, s), 0))) in
  Option.get (Ptime.add_span t (Option.get (Ptime.Span.of_d_ps (0, ps))))
