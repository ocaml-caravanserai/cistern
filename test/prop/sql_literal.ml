(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* Readers of the literals that Literal.add makes, for the property tests. Each
   reader refuses text with another form, so that a property sees a change of
   the form. They do not use the library. *)

let digits s = s <> "" && String.for_all Char.Ascii.is_digit s

let unsigned s =
  if String.starts_with ~prefix:"-" s then String.sub s 1 (String.length s - 1)
  else s

let between ~prefix ~suffix s =
  let n = String.length s
  and p = String.length prefix
  and q = String.length suffix in
  if n >= p + q && String.starts_with ~prefix s && String.ends_with ~suffix s
  then Some (String.sub s p (n - p - q))
  else None

(* -?[0-9]+ *)
let int s = if digits (unsigned s) then Int64.of_string_opt s else None

(* -?d(.d+)?e-?d+, with no leading zero in the exponent. *)
let float s =
  match String.index_opt s 'e' with
  | None ->
      None
  | Some e ->
      let mantissa = unsigned (String.sub s 0 e) in
      let exp = unsigned (String.sub s (e + 1) (String.length s - e - 1)) in
      let mantissa_ok =
        match String.split_on_char '.' mantissa with
        | [d] ->
            String.length d = 1 && digits d
        | [d; f] ->
            String.length d = 1 && digits d && digits f
        | _ ->
            false
      in
      let exp_ok = digits exp && (exp = "0" || exp.[0] <> '0') in
      if mantissa_ok && exp_ok then float_of_string_opt s else None

let hex_digit c =
  match c with
  | '0' .. '9' ->
      Some (Char.code c - 48)
  | 'A' .. 'F' ->
      Some (Char.code c - 55)
  | _ ->
      None

(* An even number of uppercase hex digits. *)
let unhex h =
  let n = String.length h in
  if n mod 2 <> 0 then None
  else
    let b = Bytes.create (n / 2) in
    let rec go i =
      if i >= n / 2 then Some (Bytes.to_string b)
      else
        match (hex_digit h.[2 * i], hex_digit h.[(2 * i) + 1]) with
        | Some x, Some y ->
            Bytes.set b i (Char.chr ((x * 16) + y)) ;
            go (i + 1)
        | _ ->
            None
    in
    go 0

let text s = Option.bind (between ~prefix:"_utf8mb4 X'" ~suffix:"'" s) unhex

let bytes s = Option.bind (between ~prefix:"X'" ~suffix:"'" s) unhex

(* 'YYYY-MM-DD HH:MM:SS.ffffff' *)
let datetime s =
  let ok =
    String.length s = 28
    && s.[0] = '\''
    && s.[27] = '\''
    && s.[5] = '-'
    && s.[8] = '-'
    && s.[11] = ' '
    && s.[14] = ':'
    && s.[17] = ':'
    && s.[20] = '.'
  in
  let num i n =
    let d = String.sub s i n in
    if digits d then Some (int_of_string d) else None
  in
  if not ok then None
  else
    match
      (num 1 4, num 6 2, num 9 2, num 12 2, num 15 2, num 18 2, num 21 6)
    with
    | Some y, Some mo, Some d, Some h, Some mi, Some sec, Some us ->
        Option.bind
          (Ptime.of_date_time ((y, mo, d), ((h, mi, sec), 0)))
          (fun t ->
            Ptime.add_span t
              (Option.get
                 (Ptime.Span.of_d_ps
                    (0, Int64.mul (Int64.of_int us) 1_000_000L) ) ) )
    | _ ->
        None
