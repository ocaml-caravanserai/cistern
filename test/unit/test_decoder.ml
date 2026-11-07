(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* The decoders of the text protocol. The float forms are the forms of the
   my_gcvt function of the servers (to be verified in phase 1.1). *)

open Windtrap
open Cistern_core

(* One column, through Decoder.run. *)
let decode d c = Decoder.run (fun cur -> Decoder.next cur d) [|c|]

let ok name d text v pp eq =
  test name (fun () ->
      match decode d (Some text) with
      | Ok x ->
          if not (eq x v) then failf "got %a" pp x
      | Error e ->
          fail e )

let refused name d text msg =
  test name (fun () ->
      equal (result unit string)
        (Error ("column 0: " ^ msg))
        (Result.map ignore (decode d (Some text))) )

let pp_int64 ppf i = Format.fprintf ppf "%Ld" i

let pp_float ppf f = Format.fprintf ppf "%h" f

let bits a b = Int64.equal (Int64.bits_of_float a) (Int64.bits_of_float b)

let time = Fixture.time

let int64_ok text v = ok text Decoder.int64 text v pp_int64 Int64.equal

let float_ok text v = ok text Decoder.float text v pp_float bits

let time_ok name text t = ok name Decoder.ptime text t Ptime.pp Ptime.equal

let two_ints c =
  let a = Decoder.next c Decoder.int64 in
  let b = Decoder.next c Decoder.int64 in
  (a, b)

let tests =
  group "Decoder"
    [ group "int64"
        [ int64_ok "0" 0L
        ; int64_ok "-9223372036854775808" Int64.min_int
        ; int64_ok "9223372036854775807" Int64.max_int
        ; refused "out of range" Decoder.int64 "9223372036854775808"
            "expected an integer"
        ; refused "an underscore" Decoder.int64 "1_0" "expected an integer"
        ; refused "hex" Decoder.int64 "0x10" "expected an integer"
        ; refused "a plus sign" Decoder.int64 "+1" "expected an integer"
        ; refused "a space" Decoder.int64 " 1" "expected an integer"
        ; refused "empty" Decoder.int64 "" "expected an integer"
        ; refused "a sign only" Decoder.int64 "-" "expected an integer"
        ; refused "an unsigned prefix" Decoder.int64 "0u1" "expected an integer"
        ]
    ; group "int"
        [ ok "max_int" Decoder.int "4611686018427387903" max_int
            Format.pp_print_int Int.equal
        ; ok "min_int" Decoder.int "-4611686018427387904" min_int
            Format.pp_print_int Int.equal
        ; refused "max_int + 1" Decoder.int "4611686018427387904"
            "expected an integer in the range of int"
        ; refused "min_int - 1" Decoder.int "-4611686018427387905"
            "expected an integer in the range of int"
        ; ok "-0" Decoder.int "-0" 0 Format.pp_print_int Int.equal
        ; ok "1234567" Decoder.int "1234567" 1234567 Format.pp_print_int
            Int.equal
        ; ok "-1234567" Decoder.int "-1234567" (-1234567) Format.pp_print_int
            Int.equal
        ; refused "empty" Decoder.int ""
            "expected an integer in the range of int"
        ; refused "a sign only" Decoder.int "-"
            "expected an integer in the range of int"
        ; refused "a plus sign" Decoder.int "+1"
            "expected an integer in the range of int"
        ; refused "a letter at the end" Decoder.int "12a"
            "expected an integer in the range of int"
        ; refused "a letter after the sign" Decoder.int "-a"
            "expected an integer in the range of int"
        ; refused "an underscore" Decoder.int "1_0"
            "expected an integer in the range of int" ]
    ; group "float"
        [ float_ok "1.5" 1.5
        ; float_ok "-0" (-0.0)
        ; float_ok "1e15" 1e15
        ; float_ok "100000000000000" 1e14
        ; float_ok "0.001" 0.001
        ; float_ok "0.0001" 1e-4
        ; float_ok "1e-5" 1e-5
        ; float_ok "1E3" 1e3
        ; float_ok "1e+3" 1e3
        ; float_ok "1.7976931348623157e308" Float.max_float
        ; float_ok "2.2250738585072014e-308" Float.min_float
        ; float_ok "5e-324" 5e-324
        ; refused "nan" Decoder.float "nan" "expected a DOUBLE"
        ; refused "inf" Decoder.float "inf" "expected a DOUBLE"
        ; refused "an infinite result" Decoder.float "1e999" "expected a DOUBLE"
        ; refused "hex" Decoder.float "0x1p3" "expected a DOUBLE"
        ; refused "an underscore" Decoder.float "1_0" "expected a DOUBLE"
        ; refused "a space" Decoder.float " 1" "expected a DOUBLE"
        ; refused "no digit after the point" Decoder.float "1."
            "expected a DOUBLE"
        ; refused "no digit before the point" Decoder.float ".5"
            "expected a DOUBLE"
        ; refused "no digit in the exponent" Decoder.float "1e"
            "expected a DOUBLE"
        ; refused "a sign only in the exponent" Decoder.float "1e-"
            "expected a DOUBLE"
        ; refused "text after the number" Decoder.float "1.5x"
            "expected a DOUBLE"
        ; refused "empty" Decoder.float "" "expected a DOUBLE" ]
    ; group "bool"
        [ ok "0" Decoder.bool "0" false Format.pp_print_bool Bool.equal
        ; ok "1" Decoder.bool "1" true Format.pp_print_bool Bool.equal
        ; ok "-1" Decoder.bool "-1" true Format.pp_print_bool Bool.equal
        ; ok "2" Decoder.bool "2" true Format.pp_print_bool Bool.equal
        ; refused "text" Decoder.bool "x" "expected a boolean (an integer)" ]
    ; group "string and bytes"
        [ ok "UTF-8" Decoder.string "\xc3\xa9" "\xc3\xa9" Format.pp_print_string
            String.equal
        ; refused "not UTF-8" Decoder.string "a\xff" "expected UTF-8 text"
        ; refused "U+110000" Decoder.string "\xf4\x90\x80\x80"
            "expected UTF-8 text"
        ; refused "the lead byte F5" Decoder.string "\xf5\x80\x80\x80"
            "expected UTF-8 text"
        ; refused "the lead byte FF" Decoder.string "\xff" "expected UTF-8 text"
        ; refused "a 5-byte form" Decoder.string "\xf8\x88\x80\x80\x80"
            "expected UTF-8 text"
        ; refused "a 6-byte form" Decoder.string "\xfc\x84\x80\x80\x80\x80"
            "expected UTF-8 text"
        ; refused "a truncated last sequence" Decoder.string "a\xe2\x82"
            "expected UTF-8 text"
        ; refused "a surrogate" Decoder.string "\xed\xa0\x80"
            "expected UTF-8 text"
        ; ok "U+10FFFF" Decoder.string "\xf4\x8f\xbf\xbf" "\xf4\x8f\xbf\xbf"
            Format.pp_print_string String.equal
        ; ok "bytes" Decoder.bytes "a\xff\000"
            (Bytes.of_string "a\xff\000")
            (fun ppf b -> Format.pp_print_string ppf (Bytes.to_string b))
            Bytes.equal ]
    ; group "ptime"
        [ time_ok "no fraction" "2026-11-06 12:34:56"
            (time (2026, 11, 6) (12, 34, 56))
        ; time_ok "one digit" "2026-11-06 12:34:56.5"
            (time ~ps:500_000_000_000L (2026, 11, 6) (12, 34, 56))
        ; time_ok "two digits" "2026-11-06 12:34:56.12"
            (time ~ps:120_000_000_000L (2026, 11, 6) (12, 34, 56))
        ; time_ok "three digits" "2026-11-06 12:34:56.123"
            (time ~ps:123_000_000_000L (2026, 11, 6) (12, 34, 56))
        ; time_ok "four digits" "2026-11-06 12:34:56.1234"
            (time ~ps:123_400_000_000L (2026, 11, 6) (12, 34, 56))
        ; time_ok "five digits" "2026-11-06 12:34:56.12345"
            (time ~ps:123_450_000_000L (2026, 11, 6) (12, 34, 56))
        ; time_ok "six digits" "2026-11-06 12:34:56.123456"
            (time ~ps:123_456_000_000L (2026, 11, 6) (12, 34, 56))
        ; time_ok "the second 59" "1000-01-01 23:59:59.999999"
            (time ~ps:999_999_000_000L (1000, 1, 1) (23, 59, 59))
        ; refused "the zero date" Decoder.ptime "0000-00-00 00:00:00"
            "expected a DATETIME"
        ; refused "an invalid day" Decoder.ptime "2026-02-30 00:00:00"
            "expected a DATETIME"
        ; refused "the second 60" Decoder.ptime "2026-12-31 23:59:60"
            "expected a DATETIME"
        ; refused "seven digits" Decoder.ptime "2026-11-06 12:34:56.1234567"
            "expected a DATETIME"
        ; refused "a point with no digit" Decoder.ptime "2026-11-06 12:34:56."
            "expected a DATETIME"
        ; refused "a T" Decoder.ptime "2026-11-06T12:34:56"
            "expected a DATETIME"
        ; refused "a short year" Decoder.ptime "226-11-06 12:34:56"
            "expected a DATETIME"
        ; refused "a letter in the fraction" Decoder.ptime
            "2026-11-06 12:34:56.12a" "expected a DATETIME"
        ; refused "a letter in the date" Decoder.ptime "2026-1a-06 12:34:56"
            "expected a DATETIME"
        ; refused "a comma for the point" Decoder.ptime "2026-11-06 12:34:56,5"
            "expected a DATETIME"
        ; refused "a letter at the start of the month" Decoder.ptime
            "2026-a1-06 12:34:56" "expected a DATETIME"
        ; refused "a dash for the second colon" Decoder.ptime
            "2026-11-06 12:34-56" "expected a DATETIME" ]
    ; group "decimal"
        [ ok "-12.50" Decoder.decimal "-12.50"
            (Option.get (Decimal.of_string "-12.50"))
            Decimal.pp Decimal.equal
        ; refused "an exponent" Decoder.decimal "1e5" "expected a DECIMAL" ]
    ; group "NULL"
        [ test "NULL in a column that is not optional" (fun () ->
              equal (result unit string)
                (Error "column 0: NULL in a column that is not optional")
                (Result.map ignore (decode Decoder.int64 None)) )
        ; test "option gives None for NULL" (fun () ->
              equal
                (result (option int64) string)
                (Ok None)
                (decode (Decoder.option Decoder.int64) None) )
        ; test "option decodes a value" (fun () ->
              equal
                (result (option int64) string)
                (Ok (Some 5L))
                (decode (Decoder.option Decoder.int64) (Some "5")) )
        ; test "option keeps the error of the column" (fun () ->
              equal
                (result (option int64) string)
                (Error "column 0: expected an integer")
                (decode (Decoder.option Decoder.int64) (Some "x")) ) ]
    ; group "run"
        [ test "reads the columns in order" (fun () ->
              equal
                (result (pair int64 int64) string)
                (Ok (1L, 2L))
                (Decoder.run two_ints [|Some "1"; Some "2"|]) )
        ; test "more columns than the row" (fun () ->
              equal
                (result (pair int64 int64) string)
                (Error "the row has 1 columns, the decoder read 2")
                (Decoder.run two_ints [|Some "1"|]) )
        ; test "fewer columns than the row" (fun () ->
              equal
                (result (pair int64 int64) string)
                (Error "the row has 3 columns, the decoder read 2")
                (Decoder.run two_ints [|Some "1"; Some "2"; Some "3"|]) )
        ; test "the column of an error" (fun () ->
              equal
                (result (pair int64 int64) string)
                (Error "column 1: expected an integer")
                (Decoder.run two_ints [|Some "1"; Some "x"|]) )
        ; test "another exception goes up" (fun () ->
              raises Exit (fun () -> Decoder.run (fun _ -> raise Exit) [||]) )
        ; test "next after run raises Invalid_argument" (fun () ->
              let kept = ref None in
              ignore (Decoder.run (fun c -> kept := Some c) [||]) ;
              raises
                (Invalid_argument
                   "Decoder.next: the run of this cursor returned" ) (fun () ->
                  Decoder.next (Option.get !kept) Decoder.int64 ) )
        ; test "next after an exception raises Invalid_argument" (fun () ->
              let kept = ref None in
              ( try
                  ignore
                    (Decoder.run
                       (fun c ->
                         kept := Some c ;
                         raise Exit )
                       [||] )
                with Exit -> () ) ;
              raises
                (Invalid_argument
                   "Decoder.next: the run of this cursor returned" ) (fun () ->
                  Decoder.next (Option.get !kept) Decoder.int64 ) )
        ; test "next after a column error raises Invalid_argument" (fun () ->
              let kept = ref None in
              ignore
                (Decoder.run
                   (fun c ->
                     kept := Some c ;
                     Decoder.next c Decoder.int64 )
                   [|Some "x"|] ) ;
              raises
                (Invalid_argument
                   "Decoder.next: the run of this cursor returned" ) (fun () ->
                  Decoder.next (Option.get !kept) Decoder.int64 ) ) ]
    ; test "map" (fun () ->
          let d = Decoder.map succ Decoder.int in
          equal (result int string) (Ok 2)
            (Decoder.run (fun c -> Decoder.next c d) [|Some "1"|]) ;
          equal (result int string)
            (Error "column 0: expected an integer in the range of int")
            (Decoder.run (fun c -> Decoder.next c d) [|Some "x"|]) ) ]
