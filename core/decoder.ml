(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

type 'a t = string option -> ('a, string) result

let column expected read = function
  | None ->
      Error "NULL in a column that is not optional"
  | Some s -> (
    match read s with Some v -> Ok v | None -> Error ("expected " ^ expected) )

(* The index of the first character of [s] at [i] or after it that is not a
   decimal digit. *)
let skip_digits s i =
  let rec go k =
    if k < String.length s && Char.Ascii.is_digit s.[k] then go (k + 1) else k
  in
  go i

let first_digit s = if String.starts_with ~prefix:"-" s then 1 else 0

(* [-?[0-9]*]. Int64.of_string_opt refuses "" and "-". *)
let integer s =
  if skip_digits s (first_digit s) = String.length s then Int64.of_string_opt s
  else None

(* [-?[0-9]+] as an int, or None if it is out of the range of int. The digits
   add up as a negative number, because min_int has no positive opposite. *)
let small_integer s =
  let n = String.length s in
  let first = first_digit s in
  let rec go i acc =
    if i = n then Some acc
    else
      match s.[i] with
      | '0' .. '9' ->
          let d = Char.code s.[i] - 48 in
          if acc < (min_int + d) / 10 then None else go (i + 1) ((acc * 10) - d)
      | _ ->
          None
  in
  if n = first then None
  else
    Option.bind (go first 0) (fun neg ->
        if first = 1 then Some neg
        else if neg = min_int then None
        else Some (-neg) )

(* The end of [-?[0-9]+(\.[0-9]+)?([eE][-+]?[0-9]+)?] at the start of [s], or
   max_int. *)
let float_end s =
  let n = String.length s in
  (* The end of one or more digits at [i], or max_int if there is no digit.
     max_int is past the end, thus no later check accepts it. *)
  let digits i =
    let j = skip_digits s i in
    if j > i then j else max_int
  in
  let i = digits (first_digit s) in
  let i = if i < n && s.[i] = '.' then digits (i + 1) else i in
  if i < n && (s.[i] = 'e' || s.[i] = 'E') then
    let j = i + 1 in
    digits (if j < n && (s.[j] = '-' || s.[j] = '+') then j + 1 else j)
  else i

let real s =
  if float_end s <> String.length s then None
  else
    Option.bind (float_of_string_opt s) (fun f ->
        if Float.is_finite f then Some f else None )

(* The value of the [k] digits of [s] at [i], or -1. *)
let number s i k =
  let rec go j acc =
    if j = i + k then acc
    else
      match s.[j] with
      | '0' .. '9' ->
          go (j + 1) ((acc * 10) + Char.code s.[j] - 48)
      | _ ->
          -1
  in
  go i 0

(* YYYY-MM-DD HH:MM:SS, then an optional point and 1 to 6 digits. *)
let time s =
  let n = String.length s in
  let sep i c = s.[i] = c in
  if
    n < 19
    || not
         ( sep 4 '-' && sep 7 '-' && sep 10 ' ' && sep 13 ':' && sep 16 ':'
         && (n = 19 || (n >= 21 && n <= 26 && sep 19 '.')) )
  then None
  else
    let y = number s 0 4 and mo = number s 5 2 and d = number s 8 2 in
    let h = number s 11 2 and mi = number s 14 2 and sec = number s 17 2 in
    (* The fraction, with zeros to 6 digits: microseconds. *)
    let us =
      if n = 19 then 0
      else
        number s 20 (n - 20) * [|100_000; 10_000; 1_000; 100; 10; 1|].(n - 21)
    in
    (* Ptime moves the second 60 to the next minute. A server never sends it.
       number gives -1 for a field that is not digits. *)
    if
      Int.min (Int.min (Int.min y mo) (Int.min d h)) (Int.min mi us) < 0
      || sec < 0 || sec > 59
    then None
    else
      Option.bind
        (Ptime.of_date_time ((y, mo, d), ((h, mi, sec), 0)))
        (fun t ->
          Ptime.add_span t
            (Ptime.Span.v (0, Int64.mul (Int64.of_int us) 1_000_000L)) )

let int64 = column "an integer" integer

let int = column "an integer in the range of int" small_integer

let float = column "a DOUBLE" real

let bool =
  column "a boolean (an integer)" (fun s ->
      Option.map (fun i -> not (Int64.equal i 0L)) (integer s) )

let string =
  column "UTF-8 text" (fun s ->
      if String.is_valid_utf_8 s then Some s else None )

let bytes = column "bytes" (fun s -> Some (Bytes.of_string s))

let ptime = column "a DATETIME" time

let decimal = column "a DECIMAL" Decimal.of_string

let option d = function
  | None ->
      Ok None
  | Some _ as c ->
      Result.map Option.some (d c)

type cursor = {row: string option array; mutable read: int; mutable live: bool}

exception Failed of string

let next c d =
  if not c.live then invalid_arg "Decoder.next: the run of this cursor returned" ;
  let i = c.read in
  if i >= Array.length c.row then
    raise
      (Failed
         (Printf.sprintf "the row has %d columns, the decoder read %d"
            (Array.length c.row) (i + 1) ) ) ;
  c.read <- i + 1 ;
  match d c.row.(i) with
  | Ok v ->
      v
  | Error msg ->
      raise (Failed (Printf.sprintf "column %d: %s" i msg))

let run f row =
  let c = {row; read= 0; live= true} in
  match f c with
  | v ->
      c.live <- false ;
      if c.read = Array.length row then Ok v
      else
        Error
          (Printf.sprintf "the row has %d columns, the decoder read %d"
             (Array.length row) c.read )
  | exception Failed msg ->
      c.live <- false ;
      Error msg
  | exception e ->
      let bt = Printexc.get_raw_backtrace () in
      c.live <- false ;
      Printexc.raise_with_backtrace e bt
