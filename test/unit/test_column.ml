(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* Column: the map of the column types of the text protocol to Value.t. *)

open Windtrap
open Cistern_core
module Column = Cistern_mysql.Column

let column ?(charset = 255) ?(flags = 0) typ : Cistern_mysql.Message.column =
  {name= "c"; charset; length= 0; typ; flags; decimals= 0}

(* The value of one column, from Column.row. *)
let decode c text = Result.map (fun row -> row.(0)) (Column.row [|c|] [|text|])

let value = Fixture.value

let tests =
  group "Column"
    [ cases "values"
        ~id:(fun (n, _, _, _) -> n)
        [ ("NULL in an INT", (fun () -> column 3), None, fun () -> Value.Null)
        ; ("the type NULL", (fun () -> column 6), Some "x", fun () -> Value.Null)
        ; ("TINYINT(1)", (fun () -> column 1), Some "1", fun () -> Value.Int 1L)
        ; ( "SMALLINT"
          , (fun () -> column 2)
          , Some "-2"
          , fun () -> Value.Int (-2L) )
        ; ("INT", (fun () -> column 3), Some "3", fun () -> Value.Int 3L)
        ; ("MEDIUMINT", (fun () -> column 9), Some "9", fun () -> Value.Int 9L)
        ; ("YEAR", (fun () -> column 13), Some "2024", fun () -> Value.Int 2024L)
        ; ( "BIGINT"
          , (fun () -> column 8)
          , Some "-9223372036854775808"
          , fun () -> Value.Int Int64.min_int )
        ; ( "INT UNSIGNED"
          , (fun () -> column ~flags:0x20 3)
          , Some "4294967295"
          , fun () -> Value.Int 4294967295L )
        ; ( "BIGINT UNSIGNED at Int64.max_int"
          , (fun () -> column ~flags:0x21 8)
          , Some "9223372036854775807"
          , fun () -> Value.Int Int64.max_int )
        ; ( "BIGINT UNSIGNED above Int64.max_int"
          , (fun () -> column ~flags:0x20 8)
          , Some "9223372036854775808"
          , fun () -> Fixture.decimal "9223372036854775808" )
        ; ( "the largest BIGINT UNSIGNED"
          , (fun () -> column ~flags:0x20 8)
          , Some "18446744073709551615"
          , fun () -> Fixture.decimal "18446744073709551615" )
        ; ("FLOAT", (fun () -> column 4), Some "1.5", fun () -> Value.Float 1.5)
        ; ( "DOUBLE"
          , (fun () -> column 5)
          , Some "-0"
          , fun () -> Value.Float (-0.0) )
        ; ( "DECIMAL"
          , (fun () -> column 246)
          , Some "-1.50"
          , fun () -> Fixture.decimal "-1.50" )
        ; ( "the old DECIMAL"
          , (fun () -> column 0)
          , Some "2"
          , fun () -> Fixture.decimal "2" )
        ; ( "DATETIME"
          , (fun () -> column 12)
          , Some "2024-01-02 03:04:05.5"
          , fun () ->
              Value.Datetime
                (Fixture.time ~ps:500_000_000_000L (2024, 1, 2) (3, 4, 5)) )
        ; ( "TIMESTAMP"
          , (fun () -> column 7)
          , Some "1970-01-01 00:00:01"
          , fun () -> Value.Datetime (Fixture.time (1970, 1, 1) (0, 0, 1)) )
        ; ( "DATE"
          , (fun () -> column 10)
          , Some "2024-01-02"
          , fun () -> Value.Text "2024-01-02" )
        ; ( "TIME"
          , (fun () -> column 11)
          , Some "-838:59:59"
          , fun () -> Value.Text "-838:59:59" )
        ; ( "the type 14"
          , (fun () -> column ~charset:63 14)
          , Some "2024-01-02"
          , fun () -> Value.Text "2024-01-02" )
        ; ( "BIT, also with a text character set"
          , (fun () -> column 16)
          , Some "\x01\xff"
          , fun () -> Value.Bytes "\x01\xff" )
        ; ( "GEOMETRY, also with a text character set"
          , (fun () -> column 255)
          , Some "\x00\x01"
          , fun () -> Value.Bytes "\x00\x01" )
        ; ( "JSON of MySQL, binary"
          , (fun () -> column ~charset:63 245)
          , Some "{\"a\": 1}"
          , fun () -> Value.Text "{\"a\": 1}" )
        ; ( "JSON of MariaDB"
          , (fun () -> column ~charset:1070 252)
          , Some "[1]"
          , fun () -> Value.Text "[1]" )
        ; ( "VARCHAR"
          , (fun () -> column 253)
          , Some "caf\xc3\xa9"
          , fun () -> Value.Text "caf\xc3\xa9" )
        ; ( "VARBINARY"
          , (fun () -> column ~charset:63 253)
          , Some "\xff"
          , fun () -> Value.Bytes "\xff" )
        ; ("CHAR", (fun () -> column 254), Some "a", fun () -> Value.Text "a")
        ; ( "BLOB"
          , (fun () -> column ~charset:63 252)
          , Some "\x00"
          , fun () -> Value.Bytes "\x00" )
        ; ("ENUM", (fun () -> column 247), Some "a", fun () -> Value.Text "a")
        ; ("SET", (fun () -> column 248), Some "a,b", fun () -> Value.Text "a,b")
        ; ( "the old VARCHAR"
          , (fun () -> column ~charset:63 15)
          , Some "\xfe"
          , fun () -> Value.Bytes "\xfe" )
        ; ( "an unknown type"
          , (fun () -> column 200)
          , Some "x"
          , fun () -> Value.Text "x" )
        ; ( "an unknown type, binary"
          , (fun () -> column ~charset:63 200)
          , Some "\xff"
          , fun () -> Value.Bytes "\xff" ) ]
        (fun (_, c, text, v) ->
          equal (result value string) (Ok (v ())) (decode (c ()) text) )
    ; cases "values that fail"
        ~id:(fun (n, _, _, _) -> n)
        [ ( "a zero date"
          , (fun () -> column 12)
          , "0000-00-00 00:00:00"
          , "expected a DATETIME" )
        ; ( "text that is not UTF-8"
          , (fun () -> column 253)
          , "\xff"
          , "expected UTF-8 text" )
        ; ( "JSON that is not UTF-8"
          , (fun () -> column ~charset:63 245)
          , "\xff"
          , "expected UTF-8 text" )
        ; ( "a FLOAT with no digits"
          , (fun () -> column 4)
          , "nan"
          , "expected a DOUBLE" )
        ; ( "a BIGINT above Int64.max_int"
          , (fun () -> column 8)
          , "9223372036854775808"
          , "expected an integer" ) ]
        (fun (_, c, text, msg) ->
          equal (result value string)
            (Error ("column 0: " ^ msg))
            (decode (c ()) (Some text)) )
    ; test "a row" (fun () ->
          equal
            (result (array value) string)
            (Ok [|Value.Int 1L; Value.Null; Value.Text "x"|])
            (Column.row
               [|column 3; column 3; column 253|]
               [|Some "1"; None; Some "x"|] ) ;
          equal
            (result (array value) string)
            (Error "column 1: expected an integer")
            (Column.row [|column 3; column 3|] [|Some "1"; Some "x"|]) ) ]
