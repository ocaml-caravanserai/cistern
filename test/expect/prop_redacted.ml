(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* The redacted copy (DESIGN.md, "Shared state and the query hook"): for any
   statement of the compiler, the SQL is the redacted copy with one literal in
   place of each "?", the literals are the literals of the values of the
   statement, and the parentheses are balanced. *)

open Windtrap
open Cistern_core
open Cistern_sql

let sfield ?(key = false) name : Schema.field =
  { name
  ; column= name
  ; scalar= Int64
  ; nullable= true
  ; key
  ; auto= false
  ; unique= false
  ; index= false }

let model : Schema.model =
  { name= "m"
  ; table= "m"
  ; fields= [sfield "k" ~key:true; sfield "a"; sfield "b"]
  ; relations= [] }

let cols = List.map (fun (f : Schema.field) -> f.column) model.fields

let col c = {Model_ast.table= "m"; column= c}

(* Values with simple literals: an integer, text and bytes. *)
let value =
  let open Gen in
  one_of
    [ map (fun i -> Value.Int i) int64
    ; map (fun s -> Value.Text s) Prop_text.utf8
    ; map (fun s -> Value.Bytes s) string ]

let column = Gen.of_list cols

let cmp = Gen.of_list Model_ast.[Eq; Ne; Lt; Gt; Le; Ge]

let rec filter depth =
  let open Gen in
  let atom =
    one_of
      [ (let+ o = cmp and+ c = column and+ v = value in
         Model_ast.Cmp (o, col c, v) )
      ; (let+ c = column and+ v = value in
         Model_ast.Null_safe_eq (col c, v) )
      ; map (fun c -> Model_ast.Is_null (col c)) column
      ; map (fun c -> Model_ast.Is_not_null (col c)) column
      ; (let+ c = column and+ vs = list ~size:(int_range 0 3) value in
         Model_ast.In (col c, vs) )
      ; (let+ c = column and+ p = Prop_text.utf8 in
         Model_ast.Like (col c, p) )
      ; map (fun b -> Model_ast.Bool b) bool ]
  in
  if depth = 0 then atom
  else
    let sub = filter (depth - 1) in
    let some = list ~size:(int_range 0 3) sub in
    frequency
      [ (3, atom)
      ; (1, map (fun es -> Model_ast.And es) some)
      ; (1, map (fun es -> Model_ast.Or es) some)
      ; (1, map (fun e -> Model_ast.Not e) sub) ]

(* The values of a filter. *)
let rec values = function
  | Model_ast.Cmp (_, _, v) | Null_safe_eq (_, v) ->
      [v]
  | Is_null _ | Is_not_null _ | Bool _ ->
      []
  | In (_, vs) ->
      vs
  | Like (_, p) ->
      [Value.Text p]
  | And es | Or es ->
      List.concat_map values es
  | Not e ->
      values e

let query ~bulk =
  let open Gen in
  let+ where = list ~size:(int_range 0 3) (filter 2)
  and+ order =
    list ~size:(int_range 0 3) (pair column (of_list Compile.[Asc; Desc]))
  and+ limit = option (int_range 0 100)
  and+ offset = if bulk then constant None else option (int_range 0 100)
  and+ lock =
    if bulk then constant Ast.No_lock
    else of_list Ast.[No_lock; For_update; For_share]
  in
  { Compile.model
  ; where
  ; order= List.map (fun (c, d) -> (col c, d)) order
  ; limit
  ; offset
  ; lock }

let assignments =
  Gen.map
    (fun vs ->
      List.combine (List.filteri (fun i _ -> i < List.length vs) cols) vs )
    (Gen.list ~size:(Gen.int_range 1 3) value)

let where_values (q : Compile.query) = List.concat_map values q.where

(* A statement, and the values of its literals. *)
let statement =
  let open Gen in
  one_of
    [ (let+ q = query ~bulk:false
       and+ f = of_list Compile.[select; first; count; exists] in
       (f q, where_values q) )
    ; map (fun k -> (Compile.get model k, [k])) value
    ; map (fun cs -> (Compile.insert model cs, List.map snd cs)) assignments
    ; (let+ k = value and+ cs = assignments in
       (Compile.update model ~key:k cs, k :: List.map snd cs) )
    ; map (fun k -> (Compile.delete model ~key:k, [k])) value
    ; (let+ q = query ~bulk:true and+ cs = assignments in
       (Compile.update_all q cs, List.map snd cs @ where_values q) )
    ; map (fun q -> (Compile.delete_all q, where_values q)) (query ~bulk:true)
    ]

let hex c = Char.Ascii.is_digit c || (c >= 'A' && c <= 'F')

(* The length of the literal at [i] of [s], or 0. *)
let literal_at s i =
  let n = String.length s in
  let rec run p j = if j < n && p s.[j] then run p (j + 1) else j in
  let quoted j =
    if j + 1 < n && s.[j] = 'X' && s.[j + 1] = '\'' then
      let e = run hex (j + 2) in
      if e < n && s.[e] = '\'' then e + 1 - i else 0
    else 0
  in
  let prefix = "_utf8mb4 " in
  if String.starts_with ~prefix (String.sub s i (n - i)) then
    let l = quoted (i + String.length prefix) in
    if l = 0 then 0 else l
  else if i < n && s.[i] = 'X' then quoted i
  else
    let j = if i < n && s.[i] = '-' then i + 1 else i in
    let e = run Char.Ascii.is_digit j in
    if e > j then e - i else 0

(* The literals of [sql] at the places of the "?" of [redacted]. *)
let literals sql redacted =
  let parts = String.split_on_char '?' redacted in
  let rec go i acc = function
    | [] ->
        assert false
    | [last] ->
        if String.sub sql i (String.length sql - i) = last then
          Some (List.rev acc)
        else None
    | f :: rest ->
        let lf = String.length f in
        if i + lf <= String.length sql && String.sub sql i lf = f then
          let l = literal_at sql (i + lf) in
          if l = 0 then None
          else go (i + lf + l) (String.sub sql (i + lf) l :: acc) rest
        else None
  in
  go 0 [] parts

let balanced s =
  let depth = ref 0 and ok = ref true in
  String.iter
    (fun c ->
      if c = '(' then incr depth
      else if c = ')' then (
        decr depth ;
        if !depth < 0 then ok := false ) )
    s ;
  !ok && !depth = 0

let lit v =
  let b = Buffer.create 16 in
  ignore (Literal.add b v) ;
  Buffer.contents b

let tests =
  group "redacted"
    [ prop "the SQL is the redacted copy with the literals" ~count:2000 statement
        (fun (r, vs) ->
          match r with
          | Error m ->
              fail m
          | Ok a ->
              List.iter
                (fun d ->
                  let sql = Print.to_sql d a and red = Print.redacted d a in
                  match literals sql red with
                  | None ->
                      fail (Printf.sprintf "%s\n%s" sql red)
                  | Some ls ->
                      equal (list string)
                        (List.sort compare (List.map lit vs))
                        (List.sort compare ls) ;
                      equal bool true (balanced red) ;
                      equal bool true (balanced sql) )
                [Dialect.Mysql; Mariadb] ) ]
