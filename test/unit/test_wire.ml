(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* Wire: the integers, the length-encoded values and the NUL strings of the
   protocol, at each boundary of their forms. *)

open Windtrap
module W = Cistern_mysql.Wire

let c = W.cursor

(* The result of [f] on a cursor of [s], or the name of the exception. *)
let run f s =
  match f (c s) with
  | v ->
      Ok v
  | exception W.Truncated ->
      Error "Truncated"
  | exception W.Malformed m ->
      Error ("Malformed: " ^ m)

let lenenc_text = function W.Null -> "NULL" | Value n -> Int64.to_string n

let tests =
  group "Wire"
    [ test "integers" (fun () ->
          let k =
            c
              "\x01\x02\x03\x04\x05\x06\x07\x08\x09\x0a\x0b\x0c\x0d\x0e\x0f\x10\x11\x12"
          in
          equal int 0x01 (W.uint8 k) ;
          equal int 0x0302 (W.uint16 k) ;
          equal int 0x060504 (W.uint24 k) ;
          equal int 0x0a090807 (W.uint32 k) ;
          equal int64 0x1211100f0e0d0c0bL (W.int64 k) ;
          equal int 0 (W.remaining k) )
    ; test "unsigned values" (fun () ->
          equal int 0xFFFF (W.uint16 (c "\xff\xff")) ;
          equal int 0xFFFFFF (W.uint24 (c "\xff\xff\xff")) ;
          equal int 0xFFFF_FFFF (W.uint32 (c "\xff\xff\xff\xff")) ;
          equal int64 (-1L) (W.int64 (c (String.make 8 '\xff'))) )
    ; cases "a read past the end: one byte short"
        ~id:(fun (n, _, _) -> n)
        [ ("uint8", "", fun s -> ignore (W.uint8 s))
        ; ("uint16", "\x01", fun s -> ignore (W.uint16 s))
        ; ("uint24", "\x01\x02", fun s -> ignore (W.uint24 s))
        ; ("uint32", "\x01\x02\x03", fun s -> ignore (W.uint32 s))
        ; ("int64", "\x01\x02\x03\x04\x05\x06\x07", fun s -> ignore (W.int64 s))
        ; ( "bytes"
          , "\x01\x02\x03\x04\x05\x06\x07"
          , fun s -> ignore (W.bytes s 8) )
        ; ("skip", "\x01\x02\x03\x04\x05\x06\x07", fun s -> W.skip s 8) ]
        (fun (_, input, f) ->
          equal (result unit string) (Error "Truncated") (run f input) )
    ; test "bytes, skip and rest" (fun () ->
          let k = c "abcdef" in
          equal string "ab" (W.bytes k 2) ;
          W.skip k 1 ;
          equal int 3 (W.remaining k) ;
          equal string "" (W.bytes k 0) ;
          equal string "def" (W.rest k) ;
          equal string "" (W.rest k) ;
          equal (result string string) (Error "Truncated")
            (run (fun k -> W.bytes k (-1)) "ab") )
    ; test "string_nul" (fun () ->
          let k = c "ab\000\000cd" in
          equal string "ab" (W.string_nul k) ;
          equal string "" (W.string_nul k) ;
          equal (result string string) (Error "Truncated")
            (run W.string_nul "cd") ;
          equal string "cd" (W.rest k) )
    ; cases "lenenc" ~id:fst
        [ ("\x00", Ok "0")
        ; ("\xfa", Ok "250")
        ; ("\xfb", Ok "NULL")
        ; ("\xfc\xfb\x00", Ok "251")
        ; ("\xfc\xff\xff", Ok "65535")
        ; ("\xfd\x00\x00\x01", Ok "65536")
        ; ("\xfd\xff\xff\xff", Ok "16777215")
        ; ("\xfe\x00\x00\x00\x01\x00\x00\x00\x00", Ok "16777216")
        ; ("\xfe\xff\xff\xff\xff\xff\xff\xff\xff", Ok "-1")
        ; ("\xff", Error "Malformed: a length-encoded integer starts with 0xFF")
        ; ("\xfc\x01", Error "Truncated")
        ; ("\xfd\x01\x02", Error "Truncated")
        ; ("\xfe\x01\x02\x03\x04\x05\x06\x07", Error "Truncated")
        ; ("", Error "Truncated") ]
        (fun (s, expected) ->
          equal (result string string) expected
            (Result.map lenenc_text (run W.lenenc s)) )
    ; cases "lenenc_string" ~id:fst
        [ ("\x03abc", Ok (Some "abc"))
        ; ("\x00", Ok (Some ""))
        ; ("\xfb", Ok None)
        ; ("\x04abc", Error "Truncated")
        ; ("\xfe\xff\xff\xff\xff\xff\xff\xff\xff", Error "Truncated")
        ; ("\xfe\x00\x00\x00\x00\x00\x00\x00\x80", Error "Truncated")
        ; ("\xfe\x00\x00\x00\x00\x00\x00\x00\x40", Error "Truncated")
        ; ("\xfc\x03\x00abcd", Ok (Some "abc")) ]
        (fun (s, expected) ->
          equal (result (option string) string) expected (run W.lenenc_string s) )
    ; test "lenenc_string at the exact end" (fun () ->
          let k = c "\x02ab" in
          equal (option string) (Some "ab") (W.lenenc_string k) ;
          equal int 0 (W.remaining k) )
    ; cases "write_lenenc"
        ~id:(fun (n, _) -> Int64.to_string n)
        [ (0L, "\x00")
        ; (250L, "\xfa")
        ; (251L, "\xfc\xfb\x00")
        ; (65535L, "\xfc\xff\xff")
        ; (65536L, "\xfd\x00\x00\x01")
        ; (16777215L, "\xfd\xff\xff\xff")
        ; (16777216L, "\xfe\x00\x00\x00\x01\x00\x00\x00\x00")
        ; (-1L, "\xfe\xff\xff\xff\xff\xff\xff\xff\xff") ]
        (fun (n, expected) ->
          equal string expected (Wire_io.written (fun w -> W.write_lenenc w n)) )
    ; test "write_lenenc_string and write_string_nul" (fun () ->
          equal string "\x03abc"
            (Wire_io.written (fun w -> W.write_lenenc_string w "abc")) ;
          equal string "ab\000"
            (Wire_io.written (fun w -> W.write_string_nul w "ab")) ;
          raises (Invalid_argument "Wire.write_string_nul: a NUL") (fun () ->
              Wire_io.written (fun w -> W.write_string_nul w "a\000b") ) )
    ; cases "peer_text"
        ~id:(fun (n, _, _, _) -> n)
        [ ("a plain text", 64, "caching_sha2_password", "caching_sha2_password")
        ; ("at the limit", 3, "abc", "abc")
        ; ("one byte more", 3, "abcd", "\"abc\"...")
        ; ("empty", 64, "", "\"\"")
        ; ("a space stays", 64, "a b", "a b")
        ; ("a quote and a backslash", 64, "a\"b\\c", "\"a\\x22b\\x5cc\"")
        ; ( "a newline, an ESC and a byte above 127"
          , 64
          , "x\n\x1b[31m\xc3\xa9"
          , "\"x\\x0a\\x1b[31m\\xc3\\xa9\"" )
        ; ( "5 KB with control bytes, at the limit of 200"
          , 200
          , String.concat "" (List.init 300 (fun _ -> "evil\n\x1b XY "))
          , "\""
            ^ String.concat "" (List.init 20 (fun _ -> "evil\\x0a\\x1b XY "))
            ^ "\"..." ) ]
        (fun (_, limit, s, expected) ->
          equal string expected (W.peer_text ~limit s) )
    ; cases "peer_text_ends"
        ~id:(fun (n, _, _, _) -> n)
        [ ("at the limit: as peer_text", 3, "abcdef", "abcdef")
        ; ("at the limit with a newline", 3, "ab\ncd", "\"ab\\x0acd\"")
        ; ("one byte more: the head and the tail", 3, "abcdefg", "abc...efg")
        ; ( "control bytes in the two parts"
          , 3
          , "\n\x1bxyz-----uv\x00"
          , "\"\\x0a\\x1bx\"...\"uv\\x00\"" )
        ; ("halves of 2", 2, "abcdefgh", "ab...gh") ]
        (fun (_, half, s, expected) ->
          equal string expected (W.peer_text_ends ~half s) ) ]
