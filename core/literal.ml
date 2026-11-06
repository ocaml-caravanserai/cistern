(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* The text of the hex digits of [s], in chunks of 256 bytes, so that a long
   value needs no large temporary string. *)
let add_hex b s =
  let digits = "0123456789ABCDEF" in
  let chunk = (256 [@mutate off "the size of a chunk changes no output"]) in
  let n = String.length s in
  let scratch =
    Bytes.create
      ((2 * Int.min chunk n) [@mutate off "a larger scratch changes no output"])
  in
  let rec from i =
    if i < n then begin
      let k = Int.min chunk (n - i) in
      for j = 0 to k - 1 do
        let c = Char.code s.[i + j] in
        Bytes.set scratch (2 * j) digits.[c lsr 4] ;
        Bytes.set scratch ((2 * j) + 1) digits.[c land 15]
      done ;
      Buffer.add_subbytes b scratch 0 (2 * k) ;
      from (i + k)
    end
  in
  from 0

(* The C function of Printf for floats. The runtime runs it in the "C" locale
   (DESIGN.md, "Literals"). *)
external format_float : string -> float -> string = "caml_format_float"

let formats =
  [| "%.0e"
   ; "%.1e"
   ; "%.2e"
   ; "%.3e"
   ; "%.4e"
   ; "%.5e"
   ; "%.6e"
   ; "%.7e"
   ; "%.8e"
   ; "%.9e"
   ; "%.10e"
   ; "%.11e"
   ; "%.12e"
   ; "%.13e"
   ; "%.14e"
   ; "%.15e"
   ; "%.16e" |]

(* The text of [%.{p}e] for the first p from 0 that reads back to the bits of
   [f]. For p = 16 the text has 17 significant digits, and it always reads back,
   thus the search ends there. *)
let shortest f =
  let bits = Int64.bits_of_float f in
  let rec find p =
    let s = format_float formats.(p) f in
    if Int64.equal (Int64.bits_of_float (float_of_string s)) bits then s
    else find (p + 1)
  in
  find 0

(* The literal of a finite float (DESIGN.md, "Literals"), from the text
   [-]d[<point>ddd]e(+|-)dd of [shortest]. The code takes the sign, the digits
   and the exponent, skips one byte for the point, and writes its own point. The
   point is one byte, and no locale can change the literal, because the OCaml
   runtime formats in the "C" locale (runtime/floats.c, USE_LOCALE). *)
let add_float b f =
  let s = shortest f in
  let n = String.length s in
  let e = String.index s 'e' in
  let first = if s.[0] = '-' then 1 else 0 in
  Buffer.add_substring b s 0 (first + 1) ;
  if e > first + 1 then begin
    Buffer.add_char b '.' ;
    Buffer.add_substring b s (first + 2) (e - first - 2)
  end ;
  Buffer.add_char b 'e' ;
  if s.[e + 1] = '-' then Buffer.add_char b '-' ;
  (* printf writes at least two digits of the exponent. *)
  let rec digits i = if i < n - 1 && s.[i] = '0' then digits (i + 1) else i in
  let i = digits (e + 2) in
  Buffer.add_substring b s i (n - i)

(* [n] with [width] digits and leading zeros. *)
let rec add_padded b width n =
  if width > 0 then begin
    add_padded b (width - 1) (n / 10) ;
    Buffer.add_char b (Char.chr (48 + (n mod 10)))
  end

let add_datetime b t =
  let (y, mo, d), ((h, mi, s), _) = Ptime.to_date_time ~tz_offset_s:0 t in
  let _, ps = Ptime.Span.to_d_ps (Ptime.frac_s t) in
  Buffer.add_char b '\'' ;
  add_padded b 4 y ;
  Buffer.add_char b '-' ;
  add_padded b 2 mo ;
  Buffer.add_char b '-' ;
  add_padded b 2 d ;
  Buffer.add_char b ' ' ;
  add_padded b 2 h ;
  Buffer.add_char b ':' ;
  add_padded b 2 mi ;
  Buffer.add_char b ':' ;
  add_padded b 2 s ;
  Buffer.add_char b '.' ;
  add_padded b 6 (Int64.to_int (Int64.div ps 1_000_000L)) ;
  Buffer.add_char b '\''

(* True if a decimal has more than 30 digits after the point. MySQL cuts a
   longer literal. *)
let too_long s =
  match String.index_opt s '.' with
  | None ->
      false
  | Some i ->
      String.length s - i - 1 > 30

let add b v =
  let put s = Buffer.add_string b s ; Ok () in
  match (v : Value.t) with
  | Null ->
      put "NULL"
  | Int i ->
      put (Int64.to_string i)
  | Float f when Float.is_nan f ->
      Error "a NaN is not a SQL value"
  | Float f when not (Float.is_finite f) ->
      Error "an infinity is not a SQL value"
  | Float f ->
      add_float b f ; Ok ()
  | Text s when not (String.is_valid_utf_8 s) ->
      Error "the text is not valid UTF-8"
  | Text s ->
      Buffer.add_string b "_utf8mb4 X'" ;
      add_hex b s ;
      put "'"
  | Bytes s ->
      Buffer.add_string b "X'" ; add_hex b s ; put "'"
  (* Ptime.max is in the year 9999, thus only the lower limit can fail. *)
  | Datetime t when Ptime.to_year ~tz_offset_s:0 t < 1000 ->
      Error "the timestamp is before the year 1000"
  | Datetime t ->
      add_datetime b t ; Ok ()
  | Decimal d when too_long (Decimal.to_string d) ->
      Error "the decimal has more than 30 digits after the point"
  | Decimal d ->
      put (Decimal.to_string d)
