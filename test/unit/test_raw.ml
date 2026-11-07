(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* Raw: the fragments and their text (DESIGN.md, "Literals"). *)

open Windtrap
open Cistern_core

let text r = Raw.to_string r

let tests =
  let open Raw in
  group "Raw"
    [ cases "the text of fragments" ~id:fst
        [ ("SQL text alone", fun () -> (sql "SELECT 1", "SELECT 1"))
        ; ( "a literal has a space before and after it"
          , fun () -> (sql "LIMIT" ++ v (Int 5L), "LIMIT 5 ") )
        ; ("an empty SQL text", fun () -> (sql "", ""))
        ; ("NULL", fun () -> (v Null, " NULL "))
        ; ( "values and text in order"
          , fun () ->
              ( sql "SELECT" ++ v (Text "a'b") ++ sql "," ++ v (Bytes "\x00")
                ++ sql "FROM t"
              , "SELECT _utf8mb4 X'612762' , X'00' FROM t" ) )
        ; ( "the grouping does not change the text"
          , fun () -> (sql "a" ++ (v (Int (-1L)) ++ sql "b"), "a -1 b") )
        ; ( "a float and a decimal"
          , fun () ->
              (v (Float 1.5) ++ v (Fixture.decimal "-0.50"), " 1.5e0  -0.50 ")
          ) ]
        (fun (_, f) ->
          let r, expected = f () in
          equal (result string string) (Ok expected) (text r) )
    ; test "redacted" (fun () ->
          equal string "SELECT ? , ?  FROM t WHERE a = ? "
            (redacted
               ( sql "SELECT" ++ v (Text "secret") ++ sql ","
               ++ (v (Float Float.nan) ++ sql " FROM t WHERE a =")
               ++ v (Bytes "x") ) ) ;
          equal string "" (redacted (sql "")) )
    ; cases "first_word" ~id:fst
        [ ("a word", fun () -> (sql "SELECT 1", Some "SELECT"))
        ; ( "leading white space"
          , fun () -> (sql " \t\r\n commit", Some "commit") )
        ; ( "the word ends at a value"
          , fun () -> (sql "SET" ++ v (Int 1L), Some "SET") )
        ; ( "a word over two fragments"
          , fun () -> (sql "SE" ++ sql "T x", Some "SET") )
        ; ("a longer word", fun () -> (sql "SETTINGS", Some "SETTINGS"))
        ; ("a value first", fun () -> (v (Int 1L) ++ sql "SELECT", None))
        ; ("white space, then a value", fun () -> (sql "  " ++ v Null, None))
        ; ("a comment first", fun () -> (sql "/* c */ COMMIT", None))
        ; ("parentheses first", fun () -> (sql "(SELECT 1)", None))
        ; ("an underscore first", fun () -> (sql "_x", None))
        ; ("the word ends at a digit", fun () -> (sql "ab1", Some "ab"))
        ; ("empty", fun () -> (sql "", None))
        ; ("white space only", fun () -> (sql " \n", None))
        ; ("a vertical tab first", fun () -> (sql "\x0bCOMMIT", None))
        ; ("a form feed first", fun () -> (sql "\x0cCOMMIT", None)) ]
        (fun (_, f) ->
          let r, expected = f () in
          equal (option string) expected (first_word r) )
    ; cases "a value with no literal" ~id:fst
        [ ( "NaN"
          , fun () ->
              (sql "SELECT" ++ v (Float Float.nan), "a NaN is not a SQL value")
          )
        ; ( "the first refused value gives the message"
          , fun () ->
              ( sql "SELECT" ++ v (Text "\xff") ++ v (Float Float.infinity)
              , "the text is not valid UTF-8" ) ) ]
        (fun (_, f) ->
          let r, expected = f () in
          equal (result string string) (Error expected) (text r) ) ]
