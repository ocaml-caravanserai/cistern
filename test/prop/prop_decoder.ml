(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* The decoders read the text that a server sends for each type back to the same
   value. *)

open Windtrap
open Cistern_core

(* One column, through Decoder.run. *)
let decoded d text =
  Result.to_option (Decoder.run (fun c -> Decoder.next c d) [|Some text|])

let bits a b = Int64.equal (Int64.bits_of_float a) (Int64.bits_of_float b)

let tests =
  group "Decoder"
    [ prop "int64 reads the text of a BIGINT" ~count:300
        ~examples:[Int64.min_int; Int64.max_int; 0L; -1L] Gen.int64 (fun i ->
          equal (option int64) (Some i)
            (decoded Decoder.int64 (Int64.to_string i)) )
    ; prop "int reads the text of a BIGINT in the range of int" ~count:300
        ~examples:[min_int; max_int; 0] Gen.int (fun i ->
          equal (option int) (Some i) (decoded Decoder.int (string_of_int i)) )
    ; prop "float reads the text of a DOUBLE" ~count:300
        ~examples:
          [ -0.0
          ; 0.0
          ; 1e15
          ; 1e14
          ; 0.001
          ; 1e-4
          ; 1e-5
          ; Float.min_float
          ; Float.max_float
          ; 5e-324
          ; Float.ldexp 1.0 (-1017) ]
        Gen.any_float
        (fun f ->
          assume (Float.is_finite f) ;
          match decoded Decoder.float (Server_text.double f) with
          | Some g ->
              is_true (bits f g)
          | None ->
              failf "the decoder refused %S" (Server_text.double f) )
    ; prop "bool reads 0 and 1" ~count:20 Gen.bool (fun b ->
          equal (option bool) (Some b)
            (decoded Decoder.bool (if b then "1" else "0")) )
    ; prop "string accepts the text if it is valid UTF-8 (the reference)"
        ~count:500
        ~examples:
          [ "\xf4\x8f\xbf\xbf"
          ; "\xed\xa0\x80"
          ; "\xc0\x80"
          ; "\xf4\x90\x80\x80"
          ; "\xf5\x80\x80\x80"
          ; "\xf8\x88\x80\x80\x80"
          ; "a\xe2\x82" ] Prop_gen.text (fun s ->
          equal (option string)
            (if Prop_gen.valid_utf_8 s then Some s else None)
            (decoded Decoder.string s) )
    ; prop "string reads valid UTF-8" ~count:300 Prop_gen.utf8 (fun s ->
          equal (option string) (Some s) (decoded Decoder.string s) )
    ; prop "bytes reads any bytes" ~count:300 Gen.string (fun s ->
          equal (option string) (Some s)
            (Option.map Bytes.to_string (decoded Decoder.bytes s)) )
    ; prop "ptime reads the text of a DATETIME(6)" ~count:300
        ~examples:[Ptime.min; Ptime.max] Prop_gen.time (fun t ->
          is_true
            (Option.equal Ptime.equal
               (Some (Ptime.truncate ~frac_s:6 t))
               (decoded Decoder.ptime (Server_text.datetime t)) ) ) ]
