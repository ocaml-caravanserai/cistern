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

(* [contains s sub]: [sub] is a part of [s]. *)
let contains s sub =
  let n = String.length s and k = String.length sub in
  let rec at i = i + k <= n && (String.sub s i k = sub || at (i + 1)) in
  at 0

(* A random ++ tree: its leaves in order (sql fragments with no "?", and
   values), and a list of numbers that chooses each split. *)
let leaf =
  Gen.one_of
    [ Gen.map (fun x -> `V x) Prop_literal.value
    ; Gen.map (fun s -> `Sql s) (Gen.of_list ["a"; " b "; "SELECT"]) ]

let tree_gen =
  Gen.pair
    (Gen.map (fun (l, ls) -> l :: ls) (Gen.pair leaf (Gen.list leaf)))
    (Gen.list (Gen.int_range 0 1000))

(* The tree of [leaves]: the head of [shape] chooses the split of the list in
   two non-empty parts, the left part takes the rest of [shape], and the right
   part takes it again (shifted), so that each side can nest. *)
let rec build leaves shape =
  match leaves with
  | [] ->
      Raw.sql ""
  | [`Sql s] ->
      Raw.sql s
  | [`V x] ->
      Raw.v x
  | _ ->
      let n = List.length leaves in
      let k, rest =
        match shape with
        | s :: rest ->
            (1 + (s mod (n - 1)), rest)
        | [] ->
            (n / 2, [])
      in
      let left = List.filteri (fun i _ -> i < k) leaves in
      let right = List.filteri (fun i _ -> i >= k) leaves in
      let rest' = match rest with _ :: r -> r | [] -> [] in
      Raw.(build left rest ++ build right rest')

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
    ; prop "redacted holds no value, and one ? for each literal" ~count:300
        tree_gen (fun (leaves, shape) ->
          let r = build leaves shape in
          let text = Raw.redacted r in
          (* Each sql fragment stays, and each value becomes " ? ", at each
             level of ++. *)
          equal string
            (String.concat ""
               (List.map (function `Sql s -> s | `V _ -> " ? ") leaves) )
            text ;
          equal int
            (List.length
               (List.filter (function `V _ -> true | _ -> false) leaves) )
            (List.length (String.split_on_char '?' text) - 1) ;
          List.iter
            (function
              | `V x -> (
                match Prop_literal.literal x with
                | Ok lit when String.length lit >= 3 ->
                    is_false ~msg:"a literal in the redacted text"
                      (contains text lit)
                | _ ->
                    () )
              | `Sql _ ->
                  () )
            leaves )
    ; prop "the grouping does not change the text" ~count:200
        Gen.(triple Prop_literal.value Prop_literal.value Prop_literal.value)
        (fun (a, b, c) ->
          let open Raw in
          equal (result string string)
            (to_string (v a ++ (sql "x" ++ v b) ++ v c))
            (to_string (v a ++ sql "x" ++ (v b ++ v c))) ) ]
