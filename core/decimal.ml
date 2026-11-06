(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

type t = string

(* [digits s i j] is true if each character of [s] from [i] to [j - 1] is a
   decimal digit. *)
let digits s i j =
  let rec go k = k >= j || (Char.Ascii.is_digit s.[k] && go (k + 1)) in
  go i

let of_string s =
  let n = String.length s in
  let first = if String.starts_with ~prefix:"-" s then 1 else 0 in
  let point = Option.value (String.index_from_opt s first '.') ~default:n in
  let int_digits = point - first in
  let frac_digits = if point = n then 0 else n - point - 1 in
  let frac_ok =
    point = n
    || (frac_digits >= 1 && frac_digits <= 38 && digits s (point + 1) n)
  in
  if
    int_digits >= 1
    && int_digits + frac_digits <= 65
    && digits s first point && frac_ok
  then Some s
  else None

let to_string d = d

let equal = String.equal

let pp = Format.pp_print_string
