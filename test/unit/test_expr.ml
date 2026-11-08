(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* Field, Expr and Update (DESIGN.md, "Core user types" and "Filters"): the
   model AST of each operator, as text. *)

open Windtrap
open Cistern_core

type row = {id: int64; n: int64 option; name: string; nick: string option}

let sfield ?(nullable = false) ?(key = false) ?column name scalar : Schema.field
    =
  { name
  ; column= Option.value column ~default:name
  ; scalar
  ; nullable
  ; key
  ; auto= false
  ; unique= false
  ; index= false }

let f_id = sfield "id" Int64 ~key:true

let f_n = sfield "n" Int64 ~nullable:true ~column:"num"

let f_name = sfield "name" String

let f_nick = sfield "nick" String ~nullable:true

let model : Schema.model =
  {name= "r"; table= "rt"; fields= [f_id; f_n; f_name; f_nick]; relations= []}

let opt f = function None -> Value.Null | Some x -> f x

let id =
  Field.make ~model ~field:f_id
    ~encode:(fun i -> Value.Int i)
    ~decode:Decoder.int64
    ~get:(fun r -> r.id)
    ~set:(fun r v -> {r with id= v})

let n =
  Field.make ~model ~field:f_n
    ~encode:(opt (fun i -> Value.Int i))
    ~decode:(Decoder.option Decoder.int64)
    ~get:(fun r -> r.n)
    ~set:(fun r v -> {r with n= v})

let name =
  Field.make ~model ~field:f_name
    ~encode:(fun s -> Value.Text s)
    ~decode:Decoder.string
    ~get:(fun r -> r.name)
    ~set:(fun r v -> {r with name= v})

let nick =
  Field.make ~model ~field:f_nick
    ~encode:(opt (fun s -> Value.Text s))
    ~decode:(Decoder.option Decoder.string)
    ~get:(fun r -> r.nick)
    ~set:(fun r v -> {r with nick= v})

let r0 = {id= 1L; n= Some 5L; name= "a"; nick= None}

(* The text of a model AST. *)
let col (c : Model_ast.col) = Printf.sprintf "`%s`.`%s`" c.table c.column

let value = function
  | Value.Null ->
      "NULL"
  | Int i ->
      Int64.to_string i
  | Text s ->
      Printf.sprintf "'%s'" s
  | _ ->
      "?"

let cmp = function
  | Model_ast.Eq ->
      "="
  | Ne ->
      "<>"
  | Lt ->
      "<"
  | Gt ->
      ">"
  | Le ->
      "<="
  | Ge ->
      ">="

let rec text = function
  | Model_ast.Cmp (op, c, v) ->
      Printf.sprintf "%s %s %s" (col c) (cmp op) (value v)
  | Null_safe_eq (c, v) ->
      Printf.sprintf "%s <=> %s" (col c) (value v)
  | Is_null c ->
      col c ^ " IS NULL"
  | Is_not_null c ->
      col c ^ " IS NOT NULL"
  | In (c, vs) ->
      Printf.sprintf "%s IN (%s)" (col c)
        (String.concat ", " (List.map value vs))
  | Like (c, p) ->
      Printf.sprintf "%s LIKE '%s'" (col c) p
  | And l ->
      "(" ^ String.concat " AND " (List.map text l) ^ ")"
  | Or l ->
      "(" ^ String.concat " OR " (List.map text l) ^ ")"
  | Not e ->
      "NOT " ^ text e
  | Bool b ->
      if b then "TRUE" else "FALSE"

let q e = text (Expr.ast e)

(* Each row: a name, the filter, the text of its AST. *)
let rows =
  let open Expr.Ops in
  [ ( "== on a field that is not an option"
    , (fun () -> name ==. "a")
    , "`rt`.`name` = 'a'" )
  ; ( "<> on a field that is not an option"
    , (fun () -> name <>. "a")
    , "`rt`.`name` <> 'a'" )
  ; ("< on a field that is not an option", (fun () -> id <. 3L), "`rt`.`id` < 3")
  ; ("> on a field that is not an option", (fun () -> id >. 3L), "`rt`.`id` > 3")
  ; ( "<= on a field that is not an option"
    , (fun () -> id <=. 3L)
    , "`rt`.`id` <= 3" )
  ; ( ">= on a field that is not an option"
    , (fun () -> id >=. 3L)
    , "`rt`.`id` >= 3" )
  ; ("== Some", (fun () -> n ==. Some 1L), "`rt`.`num` <=> 1")
  ; ("== None", (fun () -> n ==. None), "`rt`.`num` IS NULL")
  ; ("is_null", (fun () -> is_null n), "`rt`.`num` IS NULL")
  ; ("<> Some", (fun () -> n <>. Some 1L), "NOT `rt`.`num` <=> 1")
  ; ("<> None", (fun () -> n <>. None), "`rt`.`num` IS NOT NULL")
  ; ( "< Some"
    , (fun () -> n <. Some 1L)
    , "(`rt`.`num` IS NOT NULL AND `rt`.`num` < 1)" )
  ; ( "> Some"
    , (fun () -> n >. Some 1L)
    , "(`rt`.`num` IS NOT NULL AND `rt`.`num` > 1)" )
  ; ( "<= Some"
    , (fun () -> n <=. Some 1L)
    , "(`rt`.`num` IS NOT NULL AND `rt`.`num` <= 1)" )
  ; ( ">= Some"
    , (fun () -> n >=. Some 1L)
    , "(`rt`.`num` IS NOT NULL AND `rt`.`num` >= 1)" )
  ; ( "in_ with no None"
    , (fun () -> in_ n [Some 1L; Some 2L])
    , "(`rt`.`num` IS NOT NULL AND `rt`.`num` IN (1, 2))" )
  ; ( "in_ with None and a value"
    , (fun () -> in_ n [Some 1L; None])
    , "(`rt`.`num` IN (1) OR `rt`.`num` IS NULL)" )
  ; ("in_ with only None", (fun () -> in_ n [None; None]), "`rt`.`num` IS NULL")
  ; ("in_ [] on an option", (fun () -> in_ n []), "FALSE")
  ; ( "in_ on a field that is not an option"
    , (fun () -> in_ id [1L; 2L; 1L])
    , "`rt`.`id` IN (1, 2, 1)" )
  ; ("in_ []", (fun () -> in_ id []), "FALSE")
  ; ( "starts_with escapes ! % _"
    , (fun () -> starts_with name "a!%_b")
    , "`rt`.`name` LIKE 'a!!!%!_b%'" )
  ; ( "contains escapes ! % _"
    , (fun () -> contains name "%x_")
    , "`rt`.`name` LIKE '%!%x!_%'" )
  ; ( "starts_with of the empty text"
    , (fun () -> starts_with name "")
    , "`rt`.`name` LIKE '%'" )
  ; ( "starts_with_opt"
    , (fun () -> starts_with_opt nick "a_")
    , "(`rt`.`nick` IS NOT NULL AND `rt`.`nick` LIKE 'a!_%')" )
  ; ( "contains_opt"
    , (fun () -> contains_opt nick "!")
    , "(`rt`.`nick` IS NOT NULL AND `rt`.`nick` LIKE '%!!%')" )
  ; ("and_ []", (fun () -> Expr.and_ []), "TRUE")
  ; ("or_ []", (fun () -> Expr.or_ []), "FALSE")
  ; ( "and_ of two"
    , (fun () -> Expr.and_ [id ==. 1L; name ==. "a"])
    , "(`rt`.`id` = 1 AND `rt`.`name` = 'a')" )
  ; ( "or_ of two"
    , (fun () -> Expr.or_ [id ==. 1L; name ==. "a"])
    , "(`rt`.`id` = 1 OR `rt`.`name` = 'a')" )
  ; ( "|| of &&"
    , (fun () -> id ==. 1L || (id ==. 2L && not (name ==. "a")))
    , "(`rt`.`id` = 1 OR (`rt`.`id` = 2 AND NOT `rt`.`name` = 'a'))" )
  ; ( "not of ||"
    , (fun () -> not (id ==. 1L || n <. Some 2L))
    , "NOT (`rt`.`id` = 1 OR (`rt`.`num` IS NOT NULL AND `rt`.`num` < 2))" )
  ; ( "(a || b) && c"
    , (fun () -> (id ==. 1L || id ==. 2L) && name ==. "a")
    , "((`rt`.`id` = 1 OR `rt`.`id` = 2) AND `rt`.`name` = 'a')" ) ]

let order_none =
  let open Expr.Ops in
  [ ("<.", fun () -> n <. None)
  ; (">.", fun () -> n >. None)
  ; ("<=.", fun () -> n <=. None)
  ; (">=.", fun () -> n >=. None) ]

let assignments u =
  List.map (fun (c, v) -> c ^ "=" ^ value v) (Update.assignments u)

let tests =
  group "Expr"
    [ cases "the AST of each operator"
        ~id:(fun (s, _, _) -> s)
        rows
        (fun (_, e, expected) -> equal string expected (q (e ())))
    ; cases "an order with None raises Invalid_argument" ~id:fst order_none
        (fun (op, e) ->
          raises
            (Invalid_argument
               (Printf.sprintf "Expr.( %s ): None on the field \"rt\".\"num\""
                  op ) )
            e )
    ; test "the readers of Field" (fun () ->
          equal string "rt" (Field.table n) ;
          equal string "num" (Field.column n) ;
          equal bool true (Field.scalar n = Schema.Int64) ;
          equal bool true (Field.scalar name = Schema.String) ;
          equal bool true (Field.nullable n) ;
          equal bool false (Field.nullable id) ;
          equal bool true (Field.is_key id) ;
          equal bool false (Field.is_key n) ;
          equal string "NULL" (value (Field.encode n None)) ;
          equal string "7" (value (Field.encode n (Some 7L))) ;
          equal
            (result (option int64) string)
            (Ok (Some 9L))
            (Decoder.run
               (fun c -> Decoder.next c (Field.decode n))
               [|Some "9"|] ) )
    ; test "Field.get and Field.set" (fun () ->
          equal int64 1L (Field.get id r0) ;
          equal (option int64) (Some 5L) (Field.get n r0) ;
          equal string "a" (Field.get name r0) ;
          equal (option string) None (Field.get nick r0) ;
          let r = Field.set n r0 None in
          equal (option int64) None r.n ;
          equal string "a" r.name ;
          let r = Field.set nick r (Some "z") in
          equal (option string) (Some "z") r.nick ;
          equal int64 1L r.id )
    ; test "Field.make refuses a field of another model" (fun () ->
          raises
            (Invalid_argument
               "Field.make: the field \"x\" is not a field of the model \"r\""
            ) (fun () ->
              Field.make ~model ~field:(sfield "x" Int64)
                ~encode:(fun i -> Value.Int i)
                ~decode:Decoder.int64
                ~get:(fun r -> r.id)
                ~set:(fun r v -> {r with id= v}) ) ;
          raises
            (Invalid_argument
               "Field.make: the field \"n\" is not a field of the model \"r\""
            ) (fun () ->
              Field.make ~model ~field:{f_n with nullable= false}
                ~encode:(opt (fun i -> Value.Int i))
                ~decode:(Decoder.option Decoder.int64)
                ~get:(fun r -> r.n)
                ~set:(fun r v -> {r with n= v}) ) )
    ; test "Update.assignments keeps the order" (fun () ->
          equal (list string) ["name='b'"] (assignments (Update.set name "b")) ;
          equal (list string)
            ["name='b'"; "num=NULL"; "nick='c'"]
            (assignments
               Update.(set name "b" & set n None & set nick (Some "c")) ) ;
          equal (list string)
            ["nick='c'"; "name='b'"; "num=3"]
            (assignments
               Update.(set nick (Some "c") & set name "b" & set n (Some 3L)) ) )
    ; test "Update.assignments refuses the key" (fun () ->
          raises (Invalid_argument "Update: the key \"rt\".\"id\" cannot change")
            (fun () -> Update.assignments Update.(set name "b" & set id 2L) ) )
    ; test "Update.assignments refuses a column set two times" (fun () ->
          raises
            (Invalid_argument
               "Update: the column \"rt\".\"name\" is set two times" )
            (fun () ->
              Update.assignments
                Update.(set name "b" & set n None & set name "c") ) )
    ; test "Update.apply sets each field in order" (fun () ->
          let r =
            Update.apply
              Update.(set name "b" & set n None & set nick (Some "c"))
              r0
          in
          equal string "b" r.name ;
          equal (option int64) None r.n ;
          equal (option string) (Some "c") r.nick ;
          equal int64 1L r.id )
    ; test "Update.apply refuses the key and a column set two times" (fun () ->
          raises (Invalid_argument "Update: the key \"rt\".\"id\" cannot change")
            (fun () -> Update.apply Update.(set name "b" & set id 2L) r0 ) ;
          raises
            (Invalid_argument
               "Update: the column \"rt\".\"name\" is set two times" )
            (fun () -> Update.apply Update.(set name "b" & set name "c") r0 ) )
    ]
