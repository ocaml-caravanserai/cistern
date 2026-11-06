(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* The literals of Literal.add read back to the same value, and no value from a
   user is ever between quotes (DESIGN.md, "Literals"). *)

open Windtrap
open Cistern_core

let literal v =
  let b = Buffer.create 16 in
  Result.map (fun () -> Buffer.contents b) (Literal.add b v)

let bits f = Int64.bits_of_float f

let same_bits a b = Int64.equal (bits a) (bits b)

(* The float literal of the first version of Literal, with Printf and strings.
   Literal.add must give the same text. *)
let oracle f =
  let text p =
    let s = Printf.sprintf "%.*e" p f in
    let e = String.rindex s 'e' in
    let digits =
      String.of_seq
        (Seq.filter Char.Ascii.is_digit (String.to_seq (String.sub s 0 e)))
    in
    let exp = int_of_string (String.sub s (e + 1) (String.length s - e - 1)) in
    let mantissa =
      if String.length digits = 1 then digits
      else
        String.sub digits 0 1 ^ "."
        ^ String.sub digits 1 (String.length digits - 1)
    in
    (if Float.sign_bit f then "-" else "") ^ mantissa ^ "e" ^ string_of_int exp
  in
  let same s = same_bits (float_of_string s) f in
  Option.get
    (Seq.find_map
       (fun p -> if same (text p) then Some (text p) else None)
       (Seq.ints 0) )

let last_of_999 =
  Option.get
    (Ptime.add_span
       (Option.get (Ptime.of_date_time ((999, 12, 31), ((23, 59, 59), 0))))
       (Option.get (Ptime.Span.of_d_ps (0, 999_999_999_999L))) )

let first_of_1000 =
  Option.get (Ptime.of_date_time ((1000, 1, 1), ((0, 0, 0), 0)))

(* Decimal text that Decimal.of_string accepts: 1 to 65 digits, and up to 38
   after the point, at most 65 in total. *)
let decimal_text =
  let open Gen in
  let* i = int_range 1 65 in
  let* f = int_range 0 (Int.min 38 (65 - i)) in
  let* a = Prop_gen.digits i in
  let+ b = Prop_gen.digits f in
  if b = "" then a else a ^ "." ^ b

(* The parts of [s] between quotes. *)
let quoted s =
  List.filteri (fun i _ -> i mod 2 = 1) (String.split_on_char '\'' s)

let timestamp_part q = String.length q = 26

let hex_part q =
  String.for_all (fun c -> Char.Ascii.is_digit c || (c >= 'A' && c <= 'F')) q

let value =
  Gen.one_of
    [ Gen.constant Value.Null
    ; Gen.map (fun i -> Value.Int i) Gen.int64
    ; Gen.map (fun f -> Value.Float f) Gen.any_float
    ; Gen.map (fun s -> Value.Text s) Prop_gen.text
    ; Gen.map (fun s -> Value.Bytes s) Gen.string
    ; Gen.map (fun t -> Value.Datetime t) Prop_gen.time ]

let tests =
  group "Literal"
    [ prop "Int reads back" ~count:300
        ~examples:[Int64.min_int; Int64.max_int; 0L; -1L] Gen.int64 (fun i ->
          equal (option int64) (Some i)
            (Option.bind
               (Result.to_option (literal (Value.Int i)))
               Sql_literal.int ) )
    ; prop "a finite Float reads back to the same bits, NaN and infinities fail"
        ~count:300
        ~examples:
          [ -0.0
          ; 0.0
          ; Float.min_float
          ; Float.max_float
          ; Float.epsilon
          ; 5e-324
          ; Float.ldexp 1.0 (-1017)
          ; Float.nan
          ; Float.infinity
          ; Float.neg_infinity ]
        Gen.any_float
        (fun f ->
          match literal (Value.Float f) with
          | Error _ ->
              is_false (Float.is_finite f)
          | Ok s -> (
              is_true (Float.is_finite f) ;
              match Sql_literal.float s with
              | Some g ->
                  is_true (same_bits f g)
              | None ->
                  failf "the literal %S has another form" s ) )
    ; prop "a Float literal is the text of the oracle" ~count:300
        ~examples:
          [ 0.1
          ; 1.5
          ; -0.0
          ; Float.ldexp 1.0 (-1017)
          ; Float.max_float
          ; 5e-324
          ; 1e300 ]
        Gen.any_float
        (fun f ->
          assume (Float.is_finite f) ;
          equal (result string string) (Ok (oracle f)) (literal (Value.Float f)) )
    ; prop
        "a Float literal is the shortest correctly rounded %.{p}e form that \
         reads back"
        ~count:300
        ~examples:[0.1; 1.5; Float.ldexp 1.0 (-1017); Float.max_float; 5e-324]
        Gen.any_float
        (fun f ->
          assume (Float.is_finite f) ;
          match literal (Value.Float f) with
          | Error e ->
              fail e
          | Ok s ->
              let mantissa = String.sub s 0 (String.index s 'e') in
              let n =
                String.length
                  (String.concat ""
                     (String.split_on_char '.' (Sql_literal.unsigned mantissa)) )
              in
              for p = 0 to n - 2 do
                is_false
                  (same_bits f (float_of_string (Printf.sprintf "%.*e" p f)))
              done )
    ; prop
        "Text reads back if it is valid UTF-8 (the reference), and fails if not"
        ~count:500
        ~examples:
          [ ""
          ; "'"
          ; "\\"
          ; "\000"
          ; "a'b\\"
          ; "\xf0\x9f\x90\xab"
          ; "\xf4\x8f\xbf\xbf"
          ; "\xed\xa0\x80"
          ; "\xc0\x80"
          ; "\xf4\x90\x80\x80"
          ; "\xf5\x80\x80\x80"
          ; "\xf8\x88\x80\x80\x80"
          ; "\xfc\x84\x80\x80\x80\x80"
          ; "a\xe2\x82" ] Prop_gen.text (fun s ->
          match literal (Value.Text s) with
          | Ok l ->
              is_true (Prop_gen.valid_utf_8 s) ;
              equal (option string) (Some s) (Sql_literal.text l)
          | Error _ ->
              is_false (Prop_gen.valid_utf_8 s) )
    ; prop "Bytes reads back" ~count:300 Gen.string (fun s ->
          equal (option string) (Some s)
            (Option.bind
               (Result.to_option (literal (Value.Bytes s)))
               Sql_literal.bytes ) )
    ; prop
        "a Datetime reads back truncated to 1 us, before the year 1000 it fails"
        ~count:300 ~examples:[Ptime.min; Ptime.max; last_of_999; first_of_1000]
        Prop_gen.time (fun t ->
          match literal (Value.Datetime t) with
          | Ok l ->
              is_true (Ptime.to_year t >= 1000) ;
              is_true
                (Option.equal Ptime.equal
                   (Some (Ptime.truncate ~frac_s:6 t))
                   (Sql_literal.datetime l) )
          | Error _ ->
              is_true (Ptime.to_year t < 1000) )
    ; prop
        "a Decimal gives its text if it has at most 30 digits after the point"
        ~count:300
        ~examples:
          [ String.make 35 '9' ^ "." ^ String.make 30 '9'
          ; "1." ^ String.make 31 '9'
          ; String.make 27 '9' ^ "." ^ String.make 38 '9' ]
        decimal_text
        (fun s ->
          let after =
            match String.index_opt s '.' with
            | None ->
                0
            | Some i ->
                String.length s - i - 1
          in
          let d = Option.get (Decimal.of_string s) in
          match literal (Value.Decimal d) with
          | Ok l ->
              is_true (after <= 30) ;
              equal string s l
          | Error _ ->
              is_true (after > 30) )
    ; prop "no value from a user is between quotes" ~count:300 value (fun v ->
          match literal v with
          | Error _ ->
              ()
          | Ok l ->
              is_true
                ( String.fold_left (fun n c -> if c = '\'' then n + 1 else n) 0 l
                  mod 2
                = 0 ) ;
              List.iter
                (fun q ->
                  match v with
                  | Value.Datetime _ ->
                      is_true (timestamp_part q)
                  | _ ->
                      is_true (hex_part q) )
                (quoted l) ) ]
