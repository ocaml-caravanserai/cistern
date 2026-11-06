(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* Literal.add, with values from DESIGN.md, "Literals". *)

open Windtrap
open Cistern_core

(* [literal v] is the text that [Literal.add] appends to an empty buffer. *)
let literal v =
  let b = Buffer.create 16 in
  Result.map (fun () -> Buffer.contents b) (Literal.add b v)

let time = Fixture.time

let gives name v text =
  test name (fun () -> equal (result string string) (Ok text) (literal (v ())))

(* A refused value gives [Error msg], and the buffer has no change. *)
let refused name v msg =
  test name (fun () ->
      let b = Buffer.create 16 in
      Buffer.add_string b "SELECT" ;
      equal (result unit string) (Error msg) (Literal.add b (v ())) ;
      equal string "SELECT" (Buffer.contents b) )

let decimal = Fixture.decimal

let tests =
  group "Literal"
    [ group "Null and Int"
        [ gives "Null" (fun () -> Value.Null) "NULL"
        ; gives "0" (fun () -> Value.Int 0L) "0"
        ; gives "-1" (fun () -> Value.Int (-1L)) "-1"
        ; gives "max_int"
            (fun () -> Value.Int Int64.max_int)
            "9223372036854775807"
        ; gives "min_int"
            (fun () -> Value.Int Int64.min_int)
            "-9223372036854775808" ]
    ; group "Float"
        [ gives "1.5" (fun () -> Value.Float 1.5) "1.5e0"
        ; gives "-0.0" (fun () -> Value.Float (-0.0)) "-0e0"
        ; gives "0.0" (fun () -> Value.Float 0.0) "0e0"
        ; gives "0.1" (fun () -> Value.Float 0.1) "1e-1"
        ; gives "1e300" (fun () -> Value.Float 1e300) "1e300"
        ; gives "max_float"
            (fun () -> Value.Float Float.max_float)
            "1.7976931348623157e308"
        ; gives "the smallest subnormal" (fun () -> Value.Float 5e-324) "5e-324"
        ; gives "123.456" (fun () -> Value.Float 123.456) "1.23456e2"
        ; gives "1.0" (fun () -> Value.Float 1.0) "1e0"
        ; gives "100.0" (fun () -> Value.Float 100.0) "1e2"
        ; gives "-2.5e-10" (fun () -> Value.Float (-2.5e-10)) "-2.5e-10"
        ; gives "2^-1017, one digit more than the shortest decimal"
            (fun () -> Value.Float (Float.ldexp 1.0 (-1017)))
            "7.1202363472230444e-307"
        ; refused "NaN"
            (fun () -> Value.Float Float.nan)
            "a NaN is not a SQL value"
        ; refused "infinity"
            (fun () -> Value.Float Float.infinity)
            "an infinity is not a SQL value"
        ; refused "-infinity"
            (fun () -> Value.Float Float.neg_infinity)
            "an infinity is not a SQL value" ]
    ; group "Text"
        [ gives "empty" (fun () -> Value.Text "") "_utf8mb4 X''"
        ; gives "a quote and a backslash"
            (fun () -> Value.Text "a'b\\")
            "_utf8mb4 X'6127625C'"
        ; gives "NUL" (fun () -> Value.Text "\000") "_utf8mb4 X'00'"
        ; gives "a 4-byte character"
            (fun () -> Value.Text "\xf0\x9f\x90\xab")
            "_utf8mb4 X'F09F90AB'"
        ; refused "a byte that is not UTF-8"
            (fun () -> Value.Text "ab\xff")
            "the text is not valid UTF-8"
        ; refused "a surrogate"
            (fun () -> Value.Text "\xed\xa0\x80")
            "the text is not valid UTF-8"
        ; refused "an overlong NUL"
            (fun () -> Value.Text "\xc0\x80")
            "the text is not valid UTF-8"
        ; refused "U+110000"
            (fun () -> Value.Text "\xf4\x90\x80\x80")
            "the text is not valid UTF-8"
        ; refused "the lead byte F5"
            (fun () -> Value.Text "\xf5\x80\x80\x80")
            "the text is not valid UTF-8"
        ; refused "the lead byte FF"
            (fun () -> Value.Text "\xff")
            "the text is not valid UTF-8"
        ; refused "a 5-byte form"
            (fun () -> Value.Text "\xf8\x88\x80\x80\x80")
            "the text is not valid UTF-8"
        ; refused "a 6-byte form"
            (fun () -> Value.Text "\xfc\x84\x80\x80\x80\x80")
            "the text is not valid UTF-8"
        ; refused "a truncated last sequence"
            (fun () -> Value.Text "a\xe2\x82")
            "the text is not valid UTF-8"
        ; gives "U+10FFFF"
            (fun () -> Value.Text "\xf4\x8f\xbf\xbf")
            "_utf8mb4 X'F48FBFBF'"
        ; gives "600 bytes, more than two chunks"
            (fun () -> Value.Text (String.make 300 'a' ^ String.make 300 'b'))
            ( "_utf8mb4 X'"
            ^ String.concat "" (List.init 300 (fun _ -> "61"))
            ^ String.concat "" (List.init 300 (fun _ -> "62"))
            ^ "'" ) ]
    ; group "Bytes"
        [ gives "empty" (fun () -> Value.Bytes "") "X''"
        ; gives "256 and 257 bytes"
            (fun () -> Value.Bytes (String.make 256 '\x01' ^ "\xfe"))
            ("X'" ^ String.concat "" (List.init 256 (fun _ -> "01")) ^ "FE'")
        ; gives "each nibble"
            (fun () -> Value.Bytes "\000\x0f\xf0\xff\x5a")
            "X'000FF0FF5A'" ]
    ; group "Datetime"
        [ gives "truncated, not rounded"
            (fun () ->
              Value.Datetime
                (time ~ps:999_999_999_999L (2026, 11, 6) (12, 34, 56)) )
            "'2026-11-06 12:34:56.999999'"
        ; gives "microseconds"
            (fun () ->
              Value.Datetime (time ~ps:1_000_000L (1970, 1, 1) (0, 0, 0)) )
            "'1970-01-01 00:00:00.000001'"
        ; gives "the first time of 1000"
            (fun () -> Value.Datetime (time (1000, 1, 1) (0, 0, 0)))
            "'1000-01-01 00:00:00.000000'"
        ; gives "Ptime.max"
            (fun () -> Value.Datetime Ptime.max)
            "'9999-12-31 23:59:59.999999'"
        ; refused "the last picosecond of 999"
            (fun () ->
              Value.Datetime
                (time ~ps:999_999_999_999L (999, 12, 31) (23, 59, 59)) )
            "the timestamp is before the year 1000"
        ; refused "Ptime.min"
            (fun () -> Value.Datetime Ptime.min)
            "the timestamp is before the year 1000" ]
    ; group "Decimal"
        [ gives "the text" (fun () -> decimal "-12.50") "-12.50"
        ; gives "65 digits, 30 after the point"
            (fun () -> decimal (String.make 35 '9' ^ "." ^ String.make 30 '9'))
            (String.make 35 '9' ^ "." ^ String.make 30 '9')
        ; refused "31 digits after the point"
            (fun () -> decimal ("1." ^ String.make 31 '9'))
            "the decimal has more than 30 digits after the point"
        ; refused "38 digits after the point (a MariaDB value)"
            (fun () -> decimal ("0." ^ String.make 38 '1'))
            "the decimal has more than 30 digits after the point" ] ]
