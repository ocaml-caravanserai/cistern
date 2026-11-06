(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* Cistern_names: the rules of names, the fold, the hash and the names of
   indexes. The expected hashes come from a Python reference of FNV-1a 32. *)

open Windtrap
module N = Cistern_names

let rep n s = String.concat "" (List.init n (fun _ -> s))

let c n = String.make n 'c'

(* Each row: a name, and the rule in the message, or "" for Ok. *)
let identifiers =
  [ (rep 64 "a", "")
  ; (rep 65 "a", "has more than 64 characters")
  ; (rep 64 "é", "")
  ; (rep 65 "é", "has more than 64 characters")
  ; (rep 64 "€", "")
  ; ("\001", "")
  ; ("a\u{FFFF}", "")
  ; ("a\u{1D11E}", "has a character outside U+0001 to U+FFFF")
  ; ("a\000b", "has a character outside U+0001 to U+FFFF")
  ; ("a`b", "has a backquote")
  ; ("\xff", "is not valid UTF-8")
  ; ("a\xed\xa0\x80", "is not valid UTF-8")
  ; ("a ", "ends with white space")
  ; ("a\t", "ends with white space")
  ; ("a\n", "ends with white space")
  ; ("a\011", "ends with white space")
  ; ("a\012", "ends with white space")
  ; ("a\r", "ends with white space")
  ; (" a", "")
  ; ("a\u{3000}", "")
  ; ("a\u{85}", "")
  ; ("user_id", "") ]

let message rule name =
  if rule = "" then Ok ()
  else if String.starts_with ~prefix:"has a character outside" rule then
    Error
      (Printf.sprintf "the name %s %s (the servers keep names in utf8mb3)"
         (N.quote name) rule )
  else Error (Printf.sprintf "the name %s %s" (N.quote name) rule)

let unit_result = result unit string

let too_long_file name =
  Error
    (Printf.sprintf
       "the table name %s is too long for a file name: each character other \
        than [0-9A-Za-z_] counts as 5 bytes, and the limit is 251"
       (N.quote name) )

(* Each row: a table name, and true if valid_table_name accepts it. The names
   pass valid_identifier. *)
let tables =
  [ (rep 64 "a", true)
  ; (rep 64 "€", false)
  ; (rep 50 "é" ^ "a", true)
  ; (rep 50 "é" ^ "aa", false)
  ; (rep 50 "/" ^ "_", true)
  ; (rep 50 "/" ^ "_9", false)
  ; (rep 47 "€" ^ rep 17 "Z", false)
  ; (rep 46 "€" ^ rep 18 "Z", true)
  ; ("todo", true) ]

(* Each row: table, column, kind, the expected name. *)
let index_names =
  [ ("user", "email", N.Key, "user_email_key")
  ; ("todo", "user_id", Idx, "todo_user_id_idx")
  ; ("todo", "user_id", Fk, "todo_user_id_fk")
  ; ("t", c 58, Idx, "t_" ^ c 58 ^ "_idx")
  ; ("t", c 59, Idx, "t_" ^ c 49 ^ "_idx_7d6d610b")
  ; ("t", c 59, Fk, "t_" ^ c 59 ^ "_fk")
  ; ("t", c 60, Fk, "t_" ^ c 50 ^ "_fk_24f71f76")
  ; ("t", "é", Fk, "t_é_fk_b015357a")
  ; ("t", "é", Key, "t_é_key_75bc16f8")
  ; (rep 60 "é", "id", Idx, rep 51 "é" ^ "_idx_d17edb53")
  ; ("t", c 55 ^ "jfscoge", Idx, "t_" ^ c 49 ^ "_idx_b5257082")
  ; ("t", c 55 ^ "ijgfiz", Idx, "t_" ^ c 49 ^ "_idx_b5257082") ]

(* Each row: a string, and its quote. The expected values come from the
   prototype of the review. *)
let quotes =
  [ ("café", {|"café"|})
  ; ("x ", {|"x "|})
  ; ("€€", {|"€€"|})
  ; ("a\000b", {|"a\u{0}b"|})
  ; ("\xff", {|"\xff"|})
  ; ("a\xed\xa0\x80", {|"a\xed\xa0\x80"|})
  ; ("\xe2\x82", {|"\xe2\x82"|})
  ; ("a\u{85}", {|"a\u{85}"|})
  ; ("\t", {|"\u{9}"|})
  ; ("\x1f ~", {|"\u{1F} ~"|})
  ; ("\x7f", {|"\u{7F}"|})
  ; ("\u{9F}\u{A0}", "\"\\u{9F}\u{A0}\"")
  ; ("a\"b", {|"a\"b"|})
  ; ("a\\b", {|"a\\b"|})
  ; ("a`b\u{1D11E}", {|"a`b𝄞"|})
  ; ("", {|""|}) ]

let tests =
  group "Cistern_names"
    [ cases "quote"
        ~id:(fun (s, _) -> Printf.sprintf "%S" s)
        quotes
        (fun (s, expected) -> equal string expected (N.quote s))
    ; test "a message with a control character" (fun () ->
          equal unit_result
            (Error
               {|the name "a\u{0}b" has a character outside U+0001 to U+FFFF (the servers keep names in utf8mb3)|}
            )
            (N.valid_identifier "a\000b") )
    ; test "valid_table_name refuses the prefix #mysql50#" (fun () ->
          equal unit_result
            (Error
               {|the table name "#mysql50#t" starts with #mysql50#, which the servers keep for the file names of old tables|}
            )
            (N.valid_table_name "#mysql50#t") ;
          equal unit_result (Ok ()) (N.valid_table_name "#MYSQL50#t") ;
          equal unit_result (Ok ()) (N.valid_table_name "#mysql50") ;
          equal unit_result (Ok ()) (N.valid_table_name "a#mysql50#t") ;
          equal unit_result (Ok ()) (N.valid_identifier "#mysql50#t") )
    ; cases "valid_identifier"
        ~id:(fun (n, _) -> Printf.sprintf "%S" n)
        identifiers
        (fun (name, rule) ->
          equal unit_result (message rule name) (N.valid_identifier name) )
    ; test "valid_identifier refuses an empty name" (fun () ->
          equal unit_result (Error "the name is empty") (N.valid_identifier "") )
    ; cases "valid_table_name"
        ~id:(fun (n, _) -> Printf.sprintf "%S" n)
        tables
        (fun (name, ok) ->
          equal unit_result (Ok ()) (N.valid_identifier name) ;
          equal unit_result
            (if ok then Ok () else too_long_file name)
            (N.valid_table_name name) )
    ; test "valid_table_name keeps the message of valid_identifier" (fun () ->
          equal unit_result
            (N.valid_identifier (rep 65 "a"))
            (N.valid_table_name (rep 65 "a")) )
    ; test "fold changes only the ASCII letters" (fun () ->
          equal string "email_É_9" (N.fold "Email_É_9") )
    ; cases "fnv1a32"
        ~id:(fun (n, _) -> Printf.sprintf "%S" n)
        [ ("", 0x811c9dc5)
        ; ("a", 0xe40c292c)
        ; ("foobar", 0xbf9cf968)
        ; ("todo_user_id_fk", 0x7998173b)
        ; ("é", 0x1e9de8c1)
        ; ("t_" ^ c 55 ^ "jfscoge_idx", 0xb5257082)
        ; ("t_" ^ c 55 ^ "ijgfiz_idx", 0xb5257082) ]
        (fun (s, h) -> equal int h (N.fnv1a32 s))
    ; cases "index_name"
        ~id:(fun (t, c, _, e) -> Printf.sprintf "%S %S %S" t c e)
        index_names
        (fun (table, column, kind, expected) ->
          let name = N.index_name ~table ~column kind in
          equal string expected name ;
          equal unit_result (Ok ()) (N.valid_identifier name) ) ]
