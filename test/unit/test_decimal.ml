(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* Decimal.of_string accepts only -?[0-9]+(\.[0-9]+)?, with at most 65 digits in
   total and at most 38 after the point (DESIGN.md, "Core user types"). *)

open Windtrap
open Cistern_core

let decimal s = Option.get (Decimal.of_string s)

let accepted =
  [ "0"
  ; "-0"
  ; "123.456"
  ; "007"
  ; "-12.50"
  ; String.make 65 '9'
  ; String.make 27 '9' ^ "." ^ String.make 38 '9'
  ; "1." ^ String.make 38 '9' ]

let refused =
  [ ""
  ; "-"
  ; "--1"
  ; "+1"
  ; " 1"
  ; "1 "
  ; "1."
  ; ".5"
  ; "-.5"
  ; "1e5"
  ; "1.2.3"
  ; "1.a"
  ; "0x10"
  ; "1;DROP TABLE t"
  ; "1\000"
  ; "1'"
  ; String.make 66 '9'
  ; String.make 28 '9' ^ "." ^ String.make 38 '9'
  ; "1." ^ String.make 39 '9'
  ; String.make 65 '9' ^ ".9" ]

let tests =
  group "Decimal"
    [ cases "of_string accepts the grammar" ~id:(Printf.sprintf "%S") accepted
        (fun s ->
          match Decimal.of_string s with
          | Some d ->
              equal string s (Decimal.to_string d)
          | None ->
              fail "of_string refused the text" )
    ; cases "of_string refuses other text" ~id:(Printf.sprintf "%S") refused
        (fun s -> is_none ~pp:Decimal.pp (Decimal.of_string s) )
    ; test "equal compares the text, not the number" (fun () ->
          is_false (Decimal.equal (decimal "1.0") (decimal "1")) ;
          is_true (Decimal.equal (decimal "1.0") (decimal "1.0")) )
    ; test "pp prints the text" (fun () ->
          equal string "-12.50"
            (Format.asprintf "%a" Decimal.pp (decimal "-12.50")) ) ]
