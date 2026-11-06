(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* Cistern_names against references that count the bytes of UTF-8 with no
   decoder, and the properties of index_name. *)

open Windtrap
module N = Cistern_names

(* Characters that a valid name can have: 1, 2 and 3 bytes of UTF-8, and white
   space that is not ASCII. *)
let good = ["a"; "Z"; "_"; "0"; "-"; "é"; "€"; "\u{FFFF}"; "\u{3000}"; "\001"]

(* Characters that break a rule at some positions. *)
let bad = ["`"; " "; "\t"; "\r"; "\000"; "\u{1D11E}"; "\xff"; "\xe2\x82"]

let word_of alphabet lo hi =
  Gen.map (String.concat "")
    (Gen.list ~size:(Gen.int_range lo hi) (Gen.of_list alphabet))

(* A name that passes valid_identifier. *)
let ident = word_of good 1 64

(* Near names, and any text. *)
let candidate =
  Gen.one_of
    [ word_of (good @ bad) 0 70
    ; Gen.map (fun (w, s) -> w ^ s) (Gen.pair ident (Gen.of_list bad))
    ; Prop_gen.text ]

(* The characters of valid UTF-8: the bytes that do not continue a sequence. *)
let leads s =
  String.fold_left
    (fun n c -> n + Bool.to_int (Char.code c land 0xC0 <> 0x80))
    0 s

let reference s =
  let n = String.length s in
  Prop_gen.valid_utf_8 s && n > 0
  && (not
        (String.exists
           (fun c -> c = '\000' || c = '`' || Char.code c >= 0xF0)
           s ) )
  && leads s <= 64
  && not (Char.Ascii.is_white s.[n - 1])

(* The file bytes from the code points. *)
let file_bytes s =
  let rec go i acc =
    if i >= String.length s then acc
    else
      let d = String.get_utf_8_uchar s i in
      let u = Uchar.to_int (Uchar.utf_decode_uchar d) in
      let word =
        u < 128 && (Char.Ascii.is_alphanum (Char.chr u) || u = Char.code '_')
      in
      go (i + Uchar.utf_decode_length d) (acc + if word then 1 else 5)
  in
  go 0 0

let kind = Gen.of_list [N.Key; Idx; Fk]

let kind_text = function N.Key -> "key" | Idx -> "idx" | Fk -> "fk"

(* ASCII names with lengths near the limit: a full name of 50 to 70
   characters. *)
let ascii_pair =
  let open Gen in
  let* t = int_range 1 40 in
  let* n = int_range 50 70 in
  let c = Int.max 1 (n - t - 5) in
  pair (word_of ["a"; "Z"; "_"; "0"] t t) (word_of ["b"; "Y"; "_"; "9"] c c)

let names_pair = Gen.one_of [ascii_pair; Gen.pair ident ident]

let is_hex8 s =
  String.length s = 8
  && String.for_all (function '0' .. '9' | 'a' .. 'f' -> true | _ -> false) s

(* The specification of index_name (cistern_names.mli): a valid name of at most
   64 characters. It is the full name <table>_<column>_<kind> if this name has
   only ASCII characters and at most 64 characters. Else it ends with _<kind>_<8
   hex digits of the hash of the full name>, after a prefix of <table>_<column>
   that keeps as many characters as fit. *)
let form table column kind name =
  let base = table ^ "_" ^ column in
  let full = base ^ "_" ^ kind_text kind in
  let n = String.length name in
  let tail = "_" ^ kind_text kind ^ "_" in
  let hashed () =
    let p = n - 8 - String.length tail in
    p > 0
    && is_hex8 (String.sub name (n - 8) 8)
    && String.sub name p (String.length tail) = tail
    && int_of_string ("0x" ^ String.sub name (n - 8) 8) = N.fnv1a32 full
    && String.starts_with ~prefix:(String.sub name 0 p) base
    && (p = String.length base || leads name = 64)
  in
  Result.is_ok (N.valid_identifier name)
  && leads name <= 64
  &&
  if String.for_all Char.Ascii.is_valid full && leads full <= 64 then
    name = full
  else hashed ()

let tests =
  group "Cistern_names"
    [ prop "valid_identifier agrees with the reference" ~count:2000 candidate
        (fun s ->
          equal bool (reference s) (Result.is_ok (N.valid_identifier s)) )
    ; prop "valid_table_name agrees with the reference" ~count:1000
        (Gen.one_of
           [candidate; ident; Gen.map (fun w -> "#mysql50#" ^ w) ident] )
        (fun s ->
          equal bool
            ( reference s
            && file_bytes s <= 251
            && not (String.starts_with ~prefix:"#mysql50#" s) )
            (Result.is_ok (N.valid_table_name s)) )
    ; prop "index_name has the form of its specification" ~count:2000
        (Gen.pair names_pair kind) (fun ((table, column), kind) ->
          is_true (form table column kind (N.index_name ~table ~column kind)) )
    ; prop "index_name differs for names with the same first 55 characters"
        ~count:1000
        (Gen.quad (word_of good 55 55) (word_of good 1 9) (word_of good 1 9)
           kind )
        (fun (start, a, b, kind) ->
          assume (a <> b) ;
          is_false
            ( N.index_name ~table:"t" ~column:(start ^ a) kind
            = N.index_name ~table:"t" ~column:(start ^ b) kind ) ) ]
