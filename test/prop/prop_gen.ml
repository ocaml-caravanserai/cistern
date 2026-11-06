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

(* A marker of 12 letters. A printer for logs must not print it. *)
let marker = Gen.string_of ~size:(Gen.constant 12) (Gen.char_range 'a' 'z')

(* An error that holds the message [m] where its constructor has a message. *)
let error_with m =
  let open Cistern_core.Error in
  Gen.one_of
    [ Gen.of_list
        [ Not_found
        ; Retryable `Deadlock
        ; Retryable `Lock_timeout
        ; Retryable `Rolled_back
        ; Statement_timeout
        ; Timeout
        ; Pool_exhausted
        ; Outcome_unknown `Statement
        ; Outcome_unknown `Commit
        ; Implicit_commit
        ; Connection_lost
        ; Read_only ]
    ; Gen.map
        (fun kind -> Constraint_violation {kind; message= m})
        (Gen.of_list [Unique; Foreign_key; Not_null; Check; Other_constraint])
    ; Gen.map
        (fun code -> Invalid_value {code; message= m})
        (Gen.option (Gen.int_range 0 5000))
    ; Gen.constant (Connect m)
    ; Gen.constant (Decode m)
    ; Gen.map
        (fun (code, sqlstate) -> Driver {code; sqlstate; message= m})
        (Gen.pair (Gen.int_range 0 5000)
           (Gen.string_of (Gen.char_range '0' 'Z')) ) ]

(* An error, and the marker in its message. *)
let error =
  let open Gen in
  let* m = marker in
  let+ e = error_with ("Duplicate entry '" ^ m ^ "' for key 'email'") in
  (m, e)

(* [contains s sub] is true if [sub] is a part of [s]. *)
let contains s sub =
  let n = String.length s and k = String.length sub in
  let rec at i = i + k <= n && (String.sub s i k = sub || at (i + 1)) in
  at 0
