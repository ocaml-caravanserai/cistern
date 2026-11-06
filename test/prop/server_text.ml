(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* The text that a server sends for a column, for the property tests of the
   decoders. The model of DOUBLE follows the my_gcvt function of MySQL and
   MariaDB: the shortest digits that read back, a positional form for a decimal
   exponent from -4 to 14, else d[.ddd]e<exp> with no "+" and with "-" only for
   a negative exponent, and "-0" for -0.0. Phase 1.1 verifies these forms on the
   servers. *)

(* The shortest digits of [f] and its decimal exponent, from %.{p}e. *)
let digits_and_exponent f =
  let rec go p =
    let s = Printf.sprintf "%.*e" p (Float.abs f) in
    if Float.equal (float_of_string s) (Float.abs f) then
      let e = String.index s 'e' in
      let digits =
        String.concat "" (String.split_on_char '.' (String.sub s 0 e))
      in
      (digits, int_of_string (String.sub s (e + 1) (String.length s - e - 1)))
    else go (p + 1)
  in
  go 0

let double f =
  let sign = if Float.sign_bit f then "-" else "" in
  if f = 0.0 then sign ^ "0"
  else
    let d, e = digits_and_exponent f in
    let n = String.length d in
    let body =
      if e >= 15 || e < -4 then
        (if n = 1 then d else String.sub d 0 1 ^ "." ^ String.sub d 1 (n - 1))
        ^ "e" ^ string_of_int e
      else if e < 0 then "0." ^ String.make (-e - 1) '0' ^ d
      else if n <= e + 1 then d ^ String.make (e + 1 - n) '0'
      else String.sub d 0 (e + 1) ^ "." ^ String.sub d (e + 1) (n - e - 1)
    in
    sign ^ body

(* DATETIME(6): six digits after the point. *)
let datetime t =
  let (y, mo, d), ((h, mi, s), _) = Ptime.to_date_time ~tz_offset_s:0 t in
  let _, ps = Ptime.Span.to_d_ps (Ptime.frac_s t) in
  Printf.sprintf "%04d-%02d-%02d %02d:%02d:%02d.%06Ld" y mo d h mi s
    (Int64.div ps 1_000_000L)
