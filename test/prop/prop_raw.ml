(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* Raw.to_string: each value reads back from its place in the text, with the
   readers of Sql_literal, which do not use the library. *)

open Windtrap
open Cistern_core

let same_bits a b = Int64.equal (Int64.bits_of_float a) (Int64.bits_of_float b)

(* [reads_back x text]: [text] is the literal of [x]. *)
let reads_back (x : Value.t) text =
  match x with
  | Null ->
      text = "NULL"
  | Int i ->
      Sql_literal.int text = Some i
  | Float f ->
      Option.fold ~none:false ~some:(same_bits f) (Sql_literal.float text)
  | Text s ->
      Sql_literal.text text = Some s
  | Bytes s ->
      Sql_literal.bytes text = Some s
  | Datetime t ->
      Option.equal Ptime.equal
        (Some (Ptime.truncate ~frac_s:6 t))
        (Sql_literal.datetime text)
  | Decimal d ->
      text = Decimal.to_string d

let tests =
  group "Raw"
    [ prop "a value reads back between the SQL texts" ~count:500
        Prop_literal.value (fun x ->
          let open Raw in
          match to_string (sql "SELECT" ++ v x ++ sql "FROM t") with
          | Error _ ->
              (* The literal of the value fails alone too. *)
              is_true (Result.is_error (Prop_literal.literal x))
          | Ok s ->
              let n = String.length s in
              is_true (String.starts_with ~prefix:"SELECT " s) ;
              is_true (String.ends_with ~suffix:" FROM t" s) ;
              is_true (reads_back x (String.sub s 7 (n - 14))) )
    ; prop "the grouping does not change the text" ~count:200
        Gen.(triple Prop_literal.value Prop_literal.value Prop_literal.value)
        (fun (a, b, c) ->
          let open Raw in
          equal (result string string)
            (to_string (v a ++ (sql "x" ++ v b) ++ v c))
            (to_string (v a ++ sql "x" ++ (v b ++ v c))) ) ]
