(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* The text that a server sends for a column, for the property tests of the
   decoders and for the claim C10 of the integration suite. The model of DOUBLE:
   the shortest digits that read back; a positional form for a decimal exponent
   from -15 to 14, and for the exponent 15 with 17 digits; else d[.ddd]e<exp>
   with no "+" and with "-" only for a negative exponent; and "-0" for -0.0. The
   pinned MySQL 8.4 and MariaDB 11.8 sent these forms for 5090 values on
   2026-11-07, and claim C10 tests them again. *)

(* The shortest digits of [f] and the decimal exponent of the first digit. For
   each length p + 1, the digits of %.{p}e, and the digits one unit more and one
   unit less in the last place: the shortest digits that read back are not
   always the correctly rounded ones (2^-1017 has 7.120236347223045e-307, and
   %.15e gives 7.120236347223044e-307). The servers send the shortest digits. *)
let digits_and_exponent f =
  let a = Float.abs f in
  let rec go p =
    let s = Printf.sprintf "%.*e" p a in
    let e = String.index s 'e' in
    let m =
      Int64.of_string
        (String.concat "" (String.split_on_char '.' (String.sub s 0 e)))
    in
    let exp = int_of_string (String.sub s (e + 1) (String.length s - e - 1)) in
    (* The value of [m] is m × 10^(exp - p). *)
    let candidate m =
      if Int64.compare m 0L <= 0 then None
      else
        let d = Int64.to_string m in
        let first = exp - p + String.length d - 1 in
        let text = d ^ "e" ^ string_of_int (exp - p) in
        if Float.equal (float_of_string text) a then
          (* No zero at the end of the digits. *)
          let rec strip k =
            if k > 1 && d.[k - 1] = '0' then strip (k - 1) else k
          in
          Some (String.sub d 0 (strip (String.length d)), first)
        else None
    in
    match List.find_map candidate [m; Int64.succ m; Int64.pred m] with
    | Some r ->
        r
    | None ->
        go (p + 1)
  in
  go 0

let double f =
  let sign = if Float.sign_bit f then "-" else "" in
  if f = 0.0 then sign ^ "0"
  else
    let d, e = digits_and_exponent f in
    let n = String.length d in
    let body =
      if not ((e >= -15 && e <= 14) || (e = 15 && n = 17)) then
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
