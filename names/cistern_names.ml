(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

let quote s =
  let b = Buffer.create (String.length s) in
  Buffer.add_char b '"' ;
  let rec go i =
    if i < String.length s then (
      let d = String.get_utf_8_uchar s i in
      let n = Uchar.utf_decode_length d in
      let u = Uchar.to_int (Uchar.utf_decode_uchar d) in
      if not (Uchar.utf_decode_is_valid d) then
        String.iter
          (fun c -> Printf.bprintf b "\\x%02x" (Char.code c))
          (String.sub s i n)
      else if u = Char.code '"' || u = Char.code '\\' then (
        Buffer.add_char b '\\' ;
        Buffer.add_char b (Char.chr u) )
      else if u < 0x20 || (u >= 0x7F && u < 0xA0) then
        Printf.bprintf b "\\u{%X}" u
      else Buffer.add_string b (String.sub s i n) ;
      go (i + n) )
  in
  go 0 ; Buffer.add_char b '"' ; Buffer.contents b

(* The number of characters of [name], or the rule that a character breaks. *)
let characters name =
  let n = String.length name in
  let rec scan i count =
    if i >= n then Ok count
    else
      let d = String.get_utf_8_uchar name i in
      let u = Uchar.to_int (Uchar.utf_decode_uchar d) in
      if not (Uchar.utf_decode_is_valid d) then Error "is not valid UTF-8"
      else if u < 1 || u > 0xFFFF then
        Error
          "has a character outside U+0001 to U+FFFF (the servers keep names in \
           utf8mb3)"
      else if u = Char.code '`' then Error "has a backquote"
      else scan (i + Uchar.utf_decode_length d) (count + 1)
  in
  scan 0 0

let valid_identifier name =
  let fault rule = Error (Printf.sprintf "the name %s %s" (quote name) rule) in
  match characters name with
  | Error rule ->
      fault rule
  | Ok 0 ->
      Error "the name is empty"
  | Ok count when count > 64 ->
      fault "has more than 64 characters"
  | Ok _ when Char.Ascii.is_white name.[String.length name - 1] ->
      fault "ends with white space"
  | Ok _ ->
      Ok ()

(* The bytes of the encoded file name, with 5 bytes for each character that is
   not in [0-9A-Za-z_]. A continuation byte of UTF-8 adds nothing. *)
let file_bytes name =
  String.fold_left
    (fun n c ->
      if Char.Ascii.is_alphanum c || c = '_' then n + 1
      else if Char.code c land 0xC0 = 0x80 then n
      else n + 5 )
    0 name

let valid_table_name name =
  match valid_identifier name with
  | Error _ as e ->
      e
  | Ok () when String.starts_with ~prefix:"#mysql50#" name ->
      Error
        (Printf.sprintf
           "the table name %s starts with #mysql50#, which the servers keep \
            for the file names of old tables"
           (quote name) )
  | Ok () when file_bytes name > 251 ->
      Error
        (Printf.sprintf
           "the table name %s is too long for a file name: each character \
            other than [0-9A-Za-z_] counts as 5 bytes, and the limit is 251"
           (quote name) )
  | Ok () ->
      Ok ()

let fold = String.lowercase_ascii

let fnv1a32 s =
  String.fold_left
    (fun h c -> Int.logand (Int.logxor h (Char.code c) * 0x01000193) 0xffffffff)
    0x811c9dc5 s

type kind = Key | Idx | Fk

let kind_text = function Key -> "key" | Idx -> "idx" | Fk -> "fk"

(* The first [k] characters of the valid UTF-8 string [s]. *)
let cut s k =
  let n = String.length s in
  let rec go i k =
    if k = 0 || i >= n then i
    else go (i + Uchar.utf_decode_length (String.get_utf_8_uchar s i)) (k - 1)
  in
  String.sub s 0 (go 0 k)

let index_name ~table ~column kind =
  let base = table ^ "_" ^ column in
  let full = base ^ "_" ^ kind_text kind in
  if String.for_all Char.Ascii.is_valid full && String.length full <= 64 then
    full
  else
    let suffix = Printf.sprintf "_%s_%08x" (kind_text kind) (fnv1a32 full) in
    cut base (64 - String.length suffix) ^ suffix
