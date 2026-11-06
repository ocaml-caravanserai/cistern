(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

type t = Mysql | Mariadb

(* True if [part] is in [s]. *)
let contains s part =
  let n = String.length s and k = String.length part in
  let rec at i = i + k <= n && (String.sub s i k = part || at (i + 1)) in
  at 0

(* The end of the digits of [s] from [i]: [i] if there is no digit. *)
let digits s i =
  let rec go k =
    if k < String.length s && Char.Ascii.is_digit s.[k] then go (k + 1) else k
  in
  go i

(* True if [s] starts with <digits>.<digits>.<digits>. *)
let numbered s =
  let n = String.length s in
  let part i = if digits s i > i then Some (digits s i) else None in
  let dot = function
    | Some j when j < n && s.[j] = '.' ->
        part (j + 1)
    | _ ->
        None
  in
  Option.is_some (dot (dot (part 0)))

let of_version ~version ~version_comment =
  if not (numbered version) then
    Error "the server version does not start with <major>.<minor>.<patch>"
  else if
    contains (String.lowercase_ascii version) "mariadb"
    || contains (String.lowercase_ascii version_comment) "mariadb"
  then Ok Mariadb
  else Ok Mysql

let collation = function
  | Mysql ->
      "utf8mb4_0900_bin"
  | Mariadb ->
      "utf8mb4_nopad_bin"

let for_share = function
  | Mysql ->
      "FOR SHARE"
  | Mariadb ->
      "LOCK IN SHARE MODE"
