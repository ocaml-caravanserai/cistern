(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* Expr: each filter is two-valued and selects the rows of its OCaml rule
   (DESIGN.md, "Filters"). The reference evaluates the model AST with the
   three-valued logic of SQL, over rows in OCaml. Thus the rows of [not p] are
   the complement of the rows of [p], and [&&] and [||] act as in OCaml. Phase
   2f runs the same rules on the servers. *)

open Windtrap
open Cistern_core

type row = {id: int64; n: int64 option; name: string; nick: string option}

let sfield ?(nullable = false) ?(key = false) name scalar : Schema.field =
  { name
  ; column= name
  ; scalar
  ; nullable
  ; key
  ; auto= false
  ; unique= false
  ; index= false }

let f_id = sfield "id" Int64 ~key:true

let f_n = sfield "n" Int64 ~nullable:true

let f_name = sfield "name" String

let f_nick = sfield "nick" String ~nullable:true

let model : Schema.model =
  {name= "r"; table= "r"; fields= [f_id; f_n; f_name; f_nick]; relations= []}

let opt f = function None -> Value.Null | Some x -> f x

let int i = Value.Int i

let text s = Value.Text s

let id =
  Field.make ~model ~field:f_id ~encode:int ~decode:Decoder.int64
    ~get:(fun r -> r.id)
    ~set:(fun r v -> {r with id= v})

let n =
  Field.make ~model ~field:f_n ~encode:(opt int)
    ~decode:(Decoder.option Decoder.int64)
    ~get:(fun r -> r.n)
    ~set:(fun r v -> {r with n= v})

let name =
  Field.make ~model ~field:f_name ~encode:text ~decode:Decoder.string
    ~get:(fun r -> r.name)
    ~set:(fun r v -> {r with name= v})

let nick =
  Field.make ~model ~field:f_nick ~encode:(opt text)
    ~decode:(Decoder.option Decoder.string)
    ~get:(fun r -> r.nick)
    ~set:(fun r v -> {r with nick= v})

let fields = Field.[Any id; Any n; Any name; Any nick]

(* The reference: SQL with three-valued logic. *)

type tv = T | F | U

let of_bool b = if b then T else F

let cell r (c : Model_ast.col) =
  match
    List.find
      (fun (Field.Any f) -> String.equal (Field.column f) c.column)
      fields
  with
  | Field.Any f ->
      Field.encode f (Field.get f r)

let compare_values a b =
  match (a, b) with
  | Value.Int a, Value.Int b ->
      Int64.compare a b
  | Text a, Text b ->
      (* The binary collation of the columns. *)
      String.compare a b
  | _ ->
      failwith "compare_values"

(* LIKE with the escape character "!". *)
let like s p =
  let rec go i j =
    if j = String.length p then i = String.length s
    else
      match p.[j] with
      | '%' ->
          go i (j + 1) || (i < String.length s && go (i + 1) j)
      | '_' ->
          i < String.length s && go (i + 1) (j + 1)
      | '!' ->
          i < String.length s && s.[i] = p.[j + 1] && go (i + 1) (j + 2)
      | c ->
          i < String.length s && s.[i] = c && go (i + 1) (j + 1)
  in
  go 0 0

let rec eval r = function
  | Model_ast.Cmp (op, c, v) -> (
    match (cell r c, v) with
    | Value.Null, _ | _, Value.Null ->
        U
    | x, v ->
        let d = compare_values x v in
        of_bool
          ( match op with
          | Eq ->
              d = 0
          | Ne ->
              d <> 0
          | Lt ->
              d < 0
          | Gt ->
              d > 0
          | Le ->
              d <= 0
          | Ge ->
              d >= 0 ) )
  | Null_safe_eq (c, v) -> (
    match (cell r c, v) with
    | Value.Null, Value.Null ->
        T
    | Value.Null, _ | _, Value.Null ->
        F
    | x, v ->
        of_bool (compare_values x v = 0) )
  | Is_null c ->
      of_bool (cell r c = Value.Null)
  | Is_not_null c ->
      of_bool (cell r c <> Value.Null)
  | In (c, vs) -> (
    match cell r c with
    | Value.Null ->
        U
    | x ->
        let values = List.filter (fun v -> v <> Value.Null) vs in
        if List.exists (fun v -> compare_values x v = 0) values then T
        else if List.length values < List.length vs then U
        else F )
  | Like (c, p) -> (
    match cell r c with Value.Text s -> of_bool (like s p) | _ -> U )
  | And l ->
      let vs = List.map (eval r) l in
      if List.mem F vs then F else if List.mem U vs then U else T
  | Or l ->
      let vs = List.map (eval r) l in
      if List.mem T vs then T else if List.mem U vs then U else F
  | Not e -> (
    match eval r e with T -> F | F -> T | U -> U )
  | Bool b ->
      of_bool b

(* A filter as data, so that the generator can print it. *)

type op = Eq | Ne | Lt | Gt | Le | Ge

type spec =
  | Id of op * int64
  | N of op * int64 option
  | Name of op * string
  | Nick of op * string option
  | N_is_null
  | Nick_is_null
  | Id_in of int64 list
  | N_in of int64 option list
  | Nick_in of string option list
  | Name_starts of string
  | Name_contains of string
  | Nick_starts of string
  | Nick_contains of string
  | And of spec * spec
  | Or of spec * spec
  | Not of spec
  | All of spec list
  | Any of spec list

let op_text = function
  | Eq ->
      "==."
  | Ne ->
      "<>."
  | Lt ->
      "<."
  | Gt ->
      ">."
  | Le ->
      "<=."
  | Ge ->
      ">=."

let o f = function None -> "None" | Some x -> "Some " ^ f x

let l f xs = "[" ^ String.concat "; " (List.map f xs) ^ "]"

let rec spec_text = function
  | Id (op, v) ->
      Printf.sprintf "id %s %LdL" (op_text op) v
  | N (op, v) ->
      Printf.sprintf "n %s %s" (op_text op) (o (Printf.sprintf "%LdL") v)
  | Name (op, v) ->
      Printf.sprintf "name %s %S" (op_text op) v
  | Nick (op, v) ->
      Printf.sprintf "nick %s %s" (op_text op) (o (Printf.sprintf "%S") v)
  | N_is_null ->
      "is_null n"
  | Nick_is_null ->
      "is_null nick"
  | Id_in vs ->
      "in_ id " ^ l (Printf.sprintf "%LdL") vs
  | N_in vs ->
      "in_ n " ^ l (o (Printf.sprintf "%LdL")) vs
  | Nick_in vs ->
      "in_ nick " ^ l (o (Printf.sprintf "%S")) vs
  | Name_starts s ->
      Printf.sprintf "starts_with name %S" s
  | Name_contains s ->
      Printf.sprintf "contains name %S" s
  | Nick_starts s ->
      Printf.sprintf "starts_with_opt nick %S" s
  | Nick_contains s ->
      Printf.sprintf "contains_opt nick %S" s
  | And (a, b) ->
      Printf.sprintf "(%s && %s)" (spec_text a) (spec_text b)
  | Or (a, b) ->
      Printf.sprintf "(%s || %s)" (spec_text a) (spec_text b)
  | Not a ->
      Printf.sprintf "not (%s)" (spec_text a)
  | All xs ->
      "and_ " ^ l spec_text xs
  | Any xs ->
      "or_ " ^ l spec_text xs

let compare_op op f v =
  let open Expr.Ops in
  match op with
  | Eq ->
      f ==. v
  | Ne ->
      f <>. v
  | Lt ->
      f <. v
  | Gt ->
      f >. v
  | Le ->
      f <=. v
  | Ge ->
      f >=. v

let rec to_expr s =
  let open Expr.Ops in
  match s with
  | Id (op, v) ->
      compare_op op id v
  | N (op, v) ->
      compare_op op n v
  | Name (op, v) ->
      compare_op op name v
  | Nick (op, v) ->
      compare_op op nick v
  | N_is_null ->
      is_null n
  | Nick_is_null ->
      is_null nick
  | Id_in vs ->
      in_ id vs
  | N_in vs ->
      in_ n vs
  | Nick_in vs ->
      in_ nick vs
  | Name_starts p ->
      starts_with name p
  | Name_contains p ->
      contains name p
  | Nick_starts p ->
      starts_with_opt nick p
  | Nick_contains p ->
      contains_opt nick p
  | And (a, b) ->
      to_expr a && to_expr b
  | Or (a, b) ->
      to_expr a || to_expr b
  | Not a ->
      not (to_expr a)
  | All xs ->
      Expr.and_ (List.map to_expr xs)
  | Any xs ->
      Expr.or_ (List.map to_expr xs)

(* The OCaml rule of DESIGN.md, "Filters": [==.] and [<>.] are the equality of
   OCaml, also for [None]. An order and a pattern never select [None]. *)
let order op c =
  match op with
  | Eq ->
      c = 0
  | Ne ->
      c <> 0
  | Lt ->
      c < 0
  | Gt ->
      c > 0
  | Le ->
      c <= 0
  | Ge ->
      c >= 0

let opt_rule op compare x v =
  match op with
  | Eq ->
      x = v
  | Ne ->
      x <> v
  | _ -> (
    match (x, v) with Some x, Some v -> order op (compare x v) | _ -> false )

let contains_text s p =
  let ls = String.length s and lp = String.length p in
  let rec at i = i + lp <= ls && (String.sub s i lp = p || at (i + 1)) in
  at 0

let rec rule s r =
  match s with
  | Id (op, v) ->
      order op (Int64.compare r.id v)
  | N (op, v) ->
      opt_rule op Int64.compare r.n v
  | Name (op, v) ->
      order op (String.compare r.name v)
  | Nick (op, v) ->
      opt_rule op String.compare r.nick v
  | N_is_null ->
      r.n = None
  | Nick_is_null ->
      r.nick = None
  | Id_in vs ->
      List.mem r.id vs
  | N_in vs ->
      List.mem r.n vs
  | Nick_in vs ->
      List.mem r.nick vs
  | Name_starts p ->
      String.starts_with ~prefix:p r.name
  | Name_contains p ->
      contains_text r.name p
  | Nick_starts p ->
      Option.fold ~none:false ~some:(String.starts_with ~prefix:p) r.nick
  | Nick_contains p ->
      Option.fold ~none:false ~some:(fun s -> contains_text s p) r.nick
  | And (a, b) ->
      rule a r && rule b r
  | Or (a, b) ->
      rule a r || rule b r
  | Not a ->
      not (rule a r)
  | All xs ->
      List.for_all (fun x -> rule x r) xs
  | Any xs ->
      List.exists (fun x -> rule x r) xs

(* The generators: few values, so that the filters match some rows. The texts
   hold the characters of LIKE. *)

let int_v = Gen.int64_range 0L 3L

let text_v =
  Gen.of_list [""; "a"; "ab"; "b"; "a%"; "a_b"; "axb"; "!"; "a!b"; "%"]

let op = Gen.of_list [Eq; Ne; Lt; Gt; Le; Ge]

let eq_op = Gen.of_list [Eq; Ne]

let some g = Gen.map Option.some g

(* An order with [None] raises, thus the generator gives [None] only to [==.]
   and [<>.]. *)
let opt_cmp mk g =
  let open Gen in
  one_of
    [ (let+ o = op and+ v = g in
       mk o (Some v) )
    ; (let+ o = eq_op and+ v = option g in
       mk o v ) ]

let atom =
  let open Gen in
  one_of
    [ (let+ o = op and+ v = int_v in
       Id (o, v) )
    ; opt_cmp (fun o v -> N (o, v)) int_v
    ; (let+ o = op and+ v = text_v in
       Name (o, v) )
    ; opt_cmp (fun o v -> Nick (o, v)) text_v
    ; constant N_is_null
    ; constant Nick_is_null
    ; map (fun vs -> Id_in vs) (list ~size:(int_range 0 3) int_v)
    ; map (fun vs -> N_in vs) (list ~size:(int_range 0 3) (option int_v))
    ; map (fun vs -> Nick_in vs) (list ~size:(int_range 0 3) (option text_v))
    ; map (fun p -> Name_starts p) text_v
    ; map (fun p -> Name_contains p) text_v
    ; map (fun p -> Nick_starts p) text_v
    ; map (fun p -> Nick_contains p) text_v ]

let rec filter depth =
  let open Gen in
  if depth = 0 then atom
  else
    let sub = filter (depth - 1) in
    frequency
      [ (3, atom)
      ; (2, map2 (fun a b -> And (a, b)) sub sub)
      ; (2, map2 (fun a b -> Or (a, b)) sub sub)
      ; (2, map (fun a -> Not a) sub)
      ; (1, map (fun xs -> All xs) (list ~size:(int_range 0 3) sub))
      ; (1, map (fun xs -> Any xs) (list ~size:(int_range 0 3) sub)) ]

and map2 f a b =
  let open Gen in
  let+ x = a and+ y = b in
  f x y

let row =
  let open Gen in
  let+ i = int_v and+ v = option int_v and+ s = text_v and+ k = option text_v in
  {id= i; n= v; name= s; nick= k}

let row_text r =
  Printf.sprintf "{id=%Ld; n=%s; name=%S; nick=%s}" r.id
    (o (Printf.sprintf "%Ld") r.n)
    r.name
    (o (Printf.sprintf "%S") r.nick)

let case =
  Gen.with_pp
    (fun ppf (s, rows) ->
      Format.fprintf ppf "%s on %s" (spec_text s) (l row_text rows) )
    (Gen.pair (filter 3) (Gen.list ~size:(Gen.int_range 1 8) row))

let tests =
  group "Expr"
    [ prop "each filter is two-valued and follows its OCaml rule" ~count:3000
        case (fun (s, rows) ->
          let e = Expr.ast (to_expr s) in
          List.iter
            (fun r ->
              if eval r e <> of_bool (rule s r) then
                fail (Printf.sprintf "%s on %s" (spec_text s) (row_text r)) )
            rows )
    ; prop "the rows of not p are the complement of the rows of p" ~count:1000
        case (fun (s, rows) ->
          let p = Expr.ast (to_expr s) and np = Expr.ast (to_expr (Not s)) in
          List.iter
            (fun r ->
              if eval r p = T = (eval r np = T) then
                fail (Printf.sprintf "%s on %s" (spec_text s) (row_text r)) )
            rows ) ]
