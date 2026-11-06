(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* Decimal.of_string agrees with a reference function of the grammar
   -?[0-9]+(\.[0-9]+)?, with at most 65 digits in total and at most 38 after the
   point. *)

open Windtrap
open Cistern_core

let all_digits s = s <> "" && String.for_all (fun c -> c >= '0' && c <= '9') s

let reference s =
  let body =
    if String.starts_with ~prefix:"-" s then String.sub s 1 (String.length s - 1)
    else s
  in
  match String.split_on_char '.' body with
  | [i] ->
      all_digits i && String.length i <= 65
  | [i; f] ->
      all_digits i && all_digits f
      && String.length f <= 38
      && String.length i + String.length f <= 65
  | _ ->
      false

(* Text near the grammar: an optional sign, 0 to 67 digits, and an optional
   point with 0 to 40 digits. Random text alone is almost never valid. *)
let near =
  let open Gen in
  let* minus = bool in
  let* i = bind (int_range 0 67) Prop_gen.digits in
  let* point = bool in
  let+ f = bind (int_range 0 40) Prop_gen.digits in
  (if minus then "-" else "") ^ i ^ if point then "." ^ f else ""

let noise =
  Gen.string_of ~size:(Gen.int_range 0 100)
    (Gen.of_list (List.of_seq (String.to_seq "-0123456789.e+ ;'")))

let tests =
  group "Decimal"
    [ prop "of_string agrees with the grammar" ~count:500
        ~examples:
          [ String.make 65 '1'
          ; String.make 66 '1'
          ; String.make 27 '1' ^ "." ^ String.make 38 '1'
          ; String.make 28 '1' ^ "." ^ String.make 38 '1'
          ; "1." ^ String.make 39 '1' ]
        (Gen.frequency [(3, near); (1, noise); (1, Gen.string)])
        (fun s ->
          equal bool (reference s) (Option.is_some (Decimal.of_string s)) ) ]
