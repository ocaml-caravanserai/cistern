(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* The hand-written models of phase 2 (test/model): each field is the field of
   the schema with the same column, the decoder reads each column into its
   field, the drafts write only columns of the fields, and each setter sets its
   field. Then the checks of Model.update that need no statement. *)

open Windtrap
open Cistern_test_models
module Field = Cistern_core.Field
module Schema = Cistern_core.Schema
module Decoder = Cistern_core.Decoder
module Stmt = Cistern.Stmt

let models =
  [ ("user", User_todo.User.model)
  ; ("todo", User_todo.Todo.model)
  ; ("kinds", Kinds.Kinds.model)
  ; ("auto_only", Auto_only.Auto_only.model)
  ; ("person", Person.Person.model) ]

let scalar = function
  | Schema.Int64 ->
      "int64"
  | Int ->
      "int"
  | String ->
      "string"
  | Bool ->
      "bool"
  | Float ->
      "float"
  | Bytes ->
      "bytes"
  | Ptime ->
      "ptime"

(* A field as text: the table, the column, the scalar, nullable and the key. *)
let text table column s nullable key =
  Printf.sprintf "%s.%s %s%s%s" table column (scalar s)
    (if nullable then " null" else "")
    (if key then " key" else "")

let fields (module M : Cistern.MODEL) =
  List.map
    (fun (Field.Any f) ->
      text (Field.table f) (Field.column f) (Field.scalar f) (Field.nullable f)
        (Field.is_key f) )
    M.fields

let schema_fields (module M : Cistern.MODEL) =
  List.map
    (fun (f : Schema.field) ->
      text M.schema.table f.column f.scalar f.nullable f.key )
    M.schema.fields

let key_column (module M : Cistern.MODEL) = Field.column M.key

let user =
  { User_todo.id= 7L
  ; name= "Ann"
  ; email= "ann@example.com"
  ; nick= Some "an"
  ; age= Some 33
  ; active= true }

let t0 = Option.get (Ptime.of_float_s 1700000000.)

let kinds_row =
  [| Some "5"
   ; Some "-6"
   ; None
   ; Some "s"
   ; Some "so"
   ; Some "0"
   ; Some "1"
   ; Some "1.5"
   ; None
   ; Some "\x00y"
   ; Some "yo"
   ; Some "2023-11-14 22:13:20.123456"
   ; None |]

let draft_columns (cols : (string * Cistern.Value.t) list) = List.map fst cols

let tests =
  group "Models"
    [ cases "each field is the field of the schema with its column" ~id:fst
        models (fun (_, m) -> equal (list string) (schema_fields m) (fields m) )
    ; cases "the key is the key field of the schema" ~id:fst models
        (fun (_, (module M : Cistern.MODEL)) ->
          equal string
            (List.find (fun (f : Schema.field) -> f.key) M.schema.fields).column
            (key_column (module M)) )
    ; test "the schemas pass Schema.check" (fun () ->
          equal
            (result unit (list string))
            (Ok ())
            (Schema.check
               [ User_todo.Cistern_fields__user.User.schema
               ; User_todo.Cistern_fields__user.Todo.schema
               ; Kinds.Cistern_fields__kinds.Kinds.Schema.cistern__model
               ; Auto_only.Cistern_fields__auto_only.Auto_only.Schema
                 .cistern__model
               ; Person.Cistern_fields__person.Person.Schema.cistern__model ] ) )
    ; test "person: each column goes to its field, and NULL" (fun () ->
          match
            Decoder.run Person.Cistern_model__person.decode
              [|Some "3"; Some "a@b"; None; Some "Oslo"; Some "40"; Some "0"|]
          with
          | Ok r ->
              equal bool true
                ( r
                = { Person.id= 3L
                  ; email= "a@b"
                  ; nick= None
                  ; city= "Oslo"
                  ; age= Some 40
                  ; active= false } )
          | Error m ->
              fail m )
    ; test "person: the draft, its default and its columns" (fun () ->
          let d = Person.Person.draft ~email:"e" ~city:"c" () in
          equal bool true d.active ;
          equal (option string) None d.nick ;
          equal bool false
            (Person.Person.draft ~email:"e" ~city:"c" ~active:false ()).active ;
          equal (list string)
            ["email"; "nick_name"; "city"; "age"; "active"]
            (List.map fst (Person.Cistern_model__person.encode_draft d)) ;
          let r = Person.Cistern_model__person.of_draft d (Some 8L) in
          equal int64 8L r.id ;
          equal string "c" r.city ;
          equal bool true r.active )
    ; test "person: get_by_nick takes the value with no option" (fun () ->
          equal (list string)
            [ "SELECT `people`.`id`, `people`.`email`, `people`.`nick_name`, \
               `people`.`city`, `people`.`age`, `people`.`active` FROM \
               `people` WHERE `people`.`nick_name` = _utf8mb4 X'6E' LIMIT 1" ]
            (Cistern.Stmt.to_sql Cistern_core.Dialect.Mysql
               (Person.Person.get_by_nick "n") ) )
    ; test "user: each column goes to its field" (fun () ->
          match
            Decoder.run User_todo.Cistern_model__user.decode
              [| Some "7"
               ; Some "Ann"
               ; Some "ann@example.com"
               ; Some "an"
               ; Some "33"
               ; Some "1" |]
          with
          | Ok r ->
              equal bool true (r = user)
          | Error m ->
              fail m )
    ; test "user: NULL in the nullable fields" (fun () ->
          match
            Decoder.run User_todo.Cistern_model__user.decode
              [|Some "7"; Some "Ann"; Some "e"; None; None; Some "0"|]
          with
          | Ok r ->
              equal bool true
                ( r
                = {user with email= "e"; nick= None; age= None; active= false}
                )
          | Error m ->
              fail m )
    ; test "todo: each column goes to its field" (fun () ->
          match
            Decoder.run User_todo.Cistern_model__todo.decode
              [|Some "3"; Some "7"; Some "Milk"; Some "1"; Some "n"|]
          with
          | Ok r ->
              equal bool true
                ( r
                = { User_todo.id= 3L
                  ; user_id= 7L
                  ; title= "Milk"
                  ; done_= true
                  ; note= Some "n" } )
          | Error m ->
              fail m )
    ; test "kinds: each column goes to its field" (fun () ->
          match Decoder.run Kinds.Cistern_model__kinds.decode kinds_row with
          | Ok r ->
              equal bool true
                ( r
                = { Kinds.id= 5L
                  ; i= -6
                  ; i_opt= None
                  ; s= "s"
                  ; s_opt= Some "so"
                  ; b= false
                  ; b_opt= Some true
                  ; f= 1.5
                  ; f_opt= None
                  ; y= Bytes.of_string "\x00y"
                  ; y_opt= Some (Bytes.of_string "yo")
                  ; t=
                      Option.get
                        ( Ptime.of_float_s 1700000000.123456
                        |> Option.map (Ptime.truncate ~frac_s:6) )
                  ; t_opt= None } )
          | Error m ->
              fail m )
    ; test "auto_only: the key" (fun () ->
          match
            Decoder.run Auto_only.Cistern_model__auto_only.decode [|Some "9"|]
          with
          | Ok r ->
              equal int64 9L r.id
          | Error m ->
              fail m )
    ; test "a Decode message holds no text of a column" (fun () ->
          match
            Decoder.run User_todo.Cistern_model__user.decode
              [|Some "SECRET"; Some "a"; Some "b"; None; None; Some "1"|]
          with
          | Ok _ ->
              fail "decoded"
          | Error m ->
              equal bool false
                (List.exists
                   (fun i -> String.sub m i 6 = "SECRET")
                   (List.init (max 0 (String.length m - 5)) Fun.id) ) ;
              equal string "column 0: expected an integer" m )
    ; test "the columns of the drafts are columns of the fields" (fun () ->
          let columns (module M : Cistern.MODEL) =
            List.map (fun (Field.Any f) -> Field.column f) M.fields
          in
          let subset l m = List.for_all (fun c -> List.mem c (columns m)) l in
          equal (list string)
            ["name"; "email"; "nick"; "age"; "active"]
            (draft_columns
               (User_todo.Cistern_model__user.encode_draft
                  (User_todo.User.draft ~name:"a" ~email:"b" ()) ) ) ;
          equal bool true
            (subset
               (draft_columns
                  (User_todo.Cistern_model__user.encode_draft
                     (User_todo.User.draft ~name:"a" ~email:"b" ()) ) )
               User_todo.User.model ) ;
          equal (list string)
            ["user_id"; "title"; "done"; "note"]
            (draft_columns
               (User_todo.Cistern_model__todo.encode_draft
                  (User_todo.Todo.draft ~title:"t" ()) ) ) ;
          equal bool true
            (subset
               (draft_columns
                  (User_todo.Cistern_model__todo.encode_draft
                     (User_todo.Todo.draft ~title:"t" ()) ) )
               User_todo.Todo.model ) ;
          let kd =
            Kinds.Kinds.draft ~id:1L ~i:1 ~s:"" ~b:true ~f:0. ~y:Bytes.empty
              ~t:t0 ()
          in
          equal bool true
            (subset
               (draft_columns (Kinds.Cistern_model__kinds.encode_draft kd))
               Kinds.Kinds.model ) ;
          equal (list string) []
            (draft_columns (Auto_only.Cistern_model__auto_only.encode_draft ())) )
    ; test "the defaults of the drafts" (fun () ->
          let d = User_todo.User.draft ~name:"a" ~email:"b" () in
          equal bool true d.active ;
          let t = User_todo.Todo.draft ~title:"t" () in
          equal bool false t.done_ ;
          equal (option int64) None t.user_id ;
          equal (option int64) (Some 4L)
            (User_todo.Cistern_drafts__user.Set_todo.user_id t 4L).user_id )
    ; test "user: Field.set of each field" (fun () ->
          let module Q = User_todo.User.Q in
          equal bool true
            ( Field.set Q.id user 1L
            |> (fun r -> Field.set Q.name r "B")
            |> (fun r -> Field.set Q.email r "c")
            |> (fun r -> Field.set Q.nick r None)
            |> (fun r -> Field.set Q.age r (Some 1))
            |> (fun r -> Field.set Q.active r false)
            = { User_todo.id= 1L
              ; name= "B"
              ; email= "c"
              ; nick= None
              ; age= Some 1
              ; active= false } ) )
    ; test "todo: Field.set of each field" (fun () ->
          let module Q = User_todo.Todo.Q in
          let r =
            {User_todo.id= 1L; user_id= 2L; title= "t"; done_= false; note= None}
          in
          equal bool true
            ( Field.set Q.id r 5L
            |> (fun r -> Field.set Q.user_id r 6L)
            |> (fun r -> Field.set Q.title r "u")
            |> (fun r -> Field.set Q.done_ r true)
            |> (fun r -> Field.set Q.note r (Some "n"))
            = { User_todo.id= 5L
              ; user_id= 6L
              ; title= "u"
              ; done_= true
              ; note= Some "n" } ) )
    ; test "kinds: of_draft of a draft record cuts each Ptime.t" (fun () ->
          let ns = Option.get (Ptime.of_float_s 1700000000.1234567) in
          let us = Ptime.truncate ~frac_s:6 ns in
          let d : Kinds.Kinds.Drafts.kinds =
            { id= 1L
            ; i= 1
            ; i_opt= None
            ; s= ""
            ; s_opt= None
            ; b= true
            ; b_opt= None
            ; f= 0.
            ; f_opt= None
            ; y= Bytes.empty
            ; y_opt= None
            ; t= ns
            ; t_opt= Some ns }
          in
          let r = Kinds.Cistern_model__kinds.of_draft d None in
          equal bool false (Ptime.equal ns us) ;
          equal bool true (Ptime.equal r.t us) ;
          equal bool true (Option.equal Ptime.equal r.t_opt (Some us)) ;
          let r =
            Kinds.Cistern_model__kinds.of_draft {d with t_opt= None} None
          in
          equal bool true (r.t_opt = None) )
    ; test "kinds: Field.set of each field, and the cut of a Ptime.t" (fun () ->
          let module Q = Kinds.Kinds.Q in
          let ns = Option.get (Ptime.of_float_s 1700000000.1234567) in
          let us = Ptime.truncate ~frac_s:6 ns in
          let d =
            Kinds.Kinds.draft ~id:1L ~i:1 ~s:"" ~b:true ~f:0. ~y:Bytes.empty
              ~t:ns ~t_opt:ns ()
          in
          let r = Kinds.Cistern_model__kinds.of_draft d None in
          let r =
            Field.set Q.id r 2L
            |> (fun r -> Field.set Q.i r 3)
            |> (fun r -> Field.set Q.i_opt r (Some 4))
            |> (fun r -> Field.set Q.s r "x")
            |> (fun r -> Field.set Q.s_opt r (Some "y"))
            |> (fun r -> Field.set Q.b r false)
            |> (fun r -> Field.set Q.b_opt r (Some false))
            |> (fun r -> Field.set Q.f r 2.5)
            |> (fun r -> Field.set Q.f_opt r (Some 3.5))
            |> (fun r -> Field.set Q.y r (Bytes.of_string "z"))
            |> (fun r -> Field.set Q.y_opt r None)
            |> (fun r -> Field.set Q.t r ns)
            |> fun r -> Field.set Q.t_opt r None
          in
          equal bool true
            ( r
            = { Kinds.id= 2L
              ; i= 3
              ; i_opt= Some 4
              ; s= "x"
              ; s_opt= Some "y"
              ; b= false
              ; b_opt= Some false
              ; f= 2.5
              ; f_opt= Some 3.5
              ; y= Bytes.of_string "z"
              ; y_opt= None
              ; t= us
              ; t_opt= None } ) ;
          equal bool true
            (Option.equal Ptime.equal (Field.set Q.t_opt r (Some ns)).t_opt
               (Some us) ) )
    ; test "person: Field.get and Field.set of each field, one at a time"
        (fun () ->
          let module Q = Person.Person.Q in
          let r =
            { Person.id= 1L
            ; email= "e"
            ; nick= Some "n"
            ; city= "c"
            ; age= Some 2
            ; active= true }
          in
          equal int64 1L (Field.get Q.id r) ;
          equal string "e" (Field.get Q.email r) ;
          equal (option string) (Some "n") (Field.get Q.nick r) ;
          equal string "c" (Field.get Q.city r) ;
          equal (option int) (Some 2) (Field.get Q.age r) ;
          equal bool true (Field.get Q.active r) ;
          equal bool true (Field.set Q.id r 9L = {r with id= 9L}) ;
          equal bool true (Field.set Q.email r "f" = {r with email= "f"}) ;
          equal bool true (Field.set Q.nick r None = {r with nick= None}) ;
          equal bool true (Field.set Q.city r "d" = {r with city= "d"}) ;
          equal bool true (Field.set Q.age r None = {r with age= None}) ;
          equal bool true (Field.set Q.active r false = {r with active= false}) )
    ; test "kinds: Field.get of each field" (fun () ->
          let module Q = Kinds.Kinds.Q in
          let t0 = Option.get (Ptime.of_float_s 1.) in
          let t1 = Option.get (Ptime.of_float_s 2.) in
          let r =
            { Kinds.id= 1L
            ; i= 2
            ; i_opt= Some 3
            ; s= "s"
            ; s_opt= Some "so"
            ; b= true
            ; b_opt= Some false
            ; f= 1.5
            ; f_opt= Some 2.5
            ; y= Bytes.of_string "y"
            ; y_opt= Some (Bytes.of_string "yo")
            ; t= t0
            ; t_opt= Some t1 }
          in
          equal int64 1L (Field.get Q.id r) ;
          equal int 2 (Field.get Q.i r) ;
          equal (option int) (Some 3) (Field.get Q.i_opt r) ;
          equal string "s" (Field.get Q.s r) ;
          equal (option string) (Some "so") (Field.get Q.s_opt r) ;
          equal bool true (Field.get Q.b r) ;
          equal (option bool) (Some false) (Field.get Q.b_opt r) ;
          equal float_exact 1.5 (Field.get Q.f r) ;
          equal (option float_exact) (Some 2.5) (Field.get Q.f_opt r) ;
          equal string "y" (Bytes.to_string (Field.get Q.y r)) ;
          equal (option string) (Some "yo")
            (Option.map Bytes.to_string (Field.get Q.y_opt r)) ;
          equal bool true (Ptime.equal t0 (Field.get Q.t r)) ;
          equal bool true
            (Option.equal Ptime.equal (Some t1) (Field.get Q.t_opt r)) )
    ; test "auto_only: Field.get" (fun () ->
          equal int64 7L
            (Field.get Auto_only.Cistern_fields__auto_only.Auto_only.id {id= 7L}) )
    ; test "auto_only: Field.set" (fun () ->
          equal int64 3L
            (Field.set Auto_only.Cistern_fields__auto_only.Auto_only.id {id= 1L}
               3L )
              .id )
    ; test "Model.update joins the list before its check" (fun () ->
          let module Q = User_todo.User.Q in
          let up us =
            Cistern.Private.Model.update
              (module User_todo.Cistern_model__user)
              us user
          in
          raises
            (Invalid_argument
               "Update: the column \"user\".\"name\" is set two times" )
            (fun () -> up Cistern.Update.[set Q.name "a"; set Q.name "b"] ) ;
          raises
            (Invalid_argument "Update: the key \"user\".\"id\" cannot change")
            (fun () -> up Cistern.Update.[set Q.name "a"; set Q.id 2L] ) ;
          equal bool true
            ( match Stmt.Private.pure_value (up []) with
            | Some v ->
                v () = user
            | None ->
                false ) ;
          equal bool true
            (Stmt.Private.pure_value (up Cistern.Update.[set Q.name "a"]) = None) )
    ; cases "push_schema: lock_wait_timeout"
        ~id:(fun (ns, _) -> Int64.to_string ns ^ " ns")
        [ (30_000_000_000L, 29)
        ; (2_000_000_001L, 2)
        ; (2_000_000_000L, 1)
        ; (1_000_000_001L, 1)
        ; (1_000_000_000L, 1)
        ; (500_000_000L, 1)
        ; (0L, 1)
        ; (-3_000_000_000L, 1) ]
        (fun (ns, n) ->
          equal int n
            (Cistern.Private.Schema_ops.lock_wait_timeout
               ~remaining:(Weft.Span.of_ns ns) ) ) ]
