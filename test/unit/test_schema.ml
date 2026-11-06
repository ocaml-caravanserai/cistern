(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* Schema: the column types, the indexes and the foreign keys, with the user and
   todo models of DESIGN.md ("Model definition"). *)

open Windtrap
open Cistern_core
open Schema

let field ?column ?(nullable = false) ?(key = false) ?(auto = false)
    ?(unique = false) ?(index = false) name scalar =
  { name
  ; column= Option.value column ~default:name
  ; scalar
  ; nullable
  ; key
  ; auto
  ; unique
  ; index }

let model ?table ?(relations = []) name fields =
  {name; table= Option.value table ~default:name; fields; relations}

let id = field "id" Int64 ~key:true ~auto:true

let user =
  model "user"
    [id; field "name" String; field "email" String ~unique:true]
    ~relations:[Has_many {name= "todos"; target= "todo"}]

let todo =
  model "todo"
    [ id
    ; field "user_id" Int64
    ; field "title" String
    ; field "done_" Bool ~column:"done" ]
    ~relations:
      [ Belongs_to
          {name= "user"; target= "user"; key= "user_id"; references= "id"} ]

let belongs_to key = Belongs_to {name= "r"; target= "t"; key; references= "id"}

let type_text = function
  | Bigint ->
      "BIGINT"
  | Varchar_255 ->
      "VARCHAR(255)"
  | Mediumtext ->
      "MEDIUMTEXT"
  | Tinyint_1 ->
      "TINYINT(1)"
  | Double ->
      "DOUBLE"
  | Varbinary_255 ->
      "VARBINARY(255)"
  | Mediumblob ->
      "MEDIUMBLOB"
  | Datetime_6 ->
      "DATETIME(6)"

(* Each row: a field, the relations of its model, the expected type. *)
let types =
  [ (field "a" Int64, [], "BIGINT")
  ; (field "a" Int, [], "BIGINT")
  ; (field "a" Bool, [], "TINYINT(1)")
  ; (field "a" Float, [], "DOUBLE")
  ; (field "a" Ptime, [], "DATETIME(6)")
  ; (field "a" String, [], "MEDIUMTEXT")
  ; (field "a" String ~nullable:true, [], "MEDIUMTEXT")
  ; (field "a" String ~key:true, [], "VARCHAR(255)")
  ; (field "a" String ~unique:true, [], "VARCHAR(255)")
  ; (field "a" String ~index:true, [], "VARCHAR(255)")
  ; (field "a" String, [belongs_to "a"], "VARCHAR(255)")
  ; ( field "a" String
    , [belongs_to "b"; Has_many {name= "h"; target= "a"}]
    , "MEDIUMTEXT" )
  ; (field "a" Bytes, [], "MEDIUMBLOB")
  ; (field "a" Bytes ~key:true, [], "VARBINARY(255)")
  ; (field "a" Bytes ~unique:true, [], "VARBINARY(255)")
  ; (field "a" Bytes ~index:true, [], "VARBINARY(255)")
  ; (field "a" Bytes, [belongs_to "a"], "VARBINARY(255)") ]

let index_text (i : index) =
  Printf.sprintf "%s %s (%s)"
    ( match i.kind with
    | Primary ->
        "Primary"
    | Unique ->
        "Unique"
    | Plain ->
        "Plain" )
    i.name i.column

let fk_text (k : foreign_key) =
  Printf.sprintf "%s: %s -> %s.%s" k.name k.column k.target_table
    k.target_column

let indexes_of m = List.map index_text (indexes m)

let tests =
  group "Schema"
    [ cases "column_type"
        ~id:(fun (i, _, _, t) -> Printf.sprintf "%d %s" i t)
        (List.mapi (fun i (f, r, t) -> (i, f, r, t)) types)
        (fun (_, f, relations, expected) ->
          let m = model "t" [f] ~relations in
          equal string expected (type_text (column_type m f)) )
    ; test "indexes of user and todo" (fun () ->
          equal (list string)
            ["Primary PRIMARY (id)"; "Unique user_email_key (email)"]
            (indexes_of user) ;
          equal (list string)
            ["Primary PRIMARY (id)"; "Plain todo_user_id_idx (user_id)"]
            (indexes_of todo) )
    ; test "indexes in the order of the fields, with the column names"
        (fun () ->
          let m =
            model "t" ~table:"tab"
              [ field "c" String ~index:true ~column:"cc"
              ; field "b" Int ~unique:true ~column:"bb"
              ; id
              ; field "a" Int ]
          in
          equal (list string)
            [ "Primary PRIMARY (id)"
            ; "Plain tab_cc_idx (cc)"
            ; "Unique tab_bb_key (bb)" ]
            (indexes_of m) )
    ; test "a Belongs_to key with index has one index" (fun () ->
          let m =
            model "t"
              [id; field "u" Int64 ~index:true]
              ~relations:[belongs_to "u"]
          in
          equal (list string)
            ["Primary PRIMARY (id)"; "Plain t_u_idx (u)"]
            (indexes_of m) )
    ; test "a unique Belongs_to key has no plain index" (fun () ->
          let m =
            model "t"
              [id; field "u" Int64 ~unique:true]
              ~relations:[belongs_to "u"]
          in
          equal (list string)
            ["Primary PRIMARY (id)"; "Unique t_u_key (u)"]
            (indexes_of m) )
    ; test "a Belongs_to on the key has no plain index" (fun () ->
          let m =
            model "t" [id]
              ~relations:
                [ Belongs_to
                    {name= "r"; target= "p"; key= "id"; references= "id"} ]
          in
          equal (list string) ["Primary PRIMARY (id)"] (indexes_of m) )
    ; test "a long index name gets the hash" (fun () ->
          let column = String.make 59 'c' in
          let m = model "t" [id; field column Int ~index:true] in
          equal (list string)
            [ "Primary PRIMARY (id)"
            ; Printf.sprintf "Plain t_%s_idx_7d6d610b (%s)" (String.make 49 'c')
                column ]
            (indexes_of m) )
    ; test "foreign_keys of todo" (fun () ->
          equal (list string)
            ["todo_user_id_fk: user_id -> user.id"]
            (List.map fk_text (foreign_keys [user; todo] todo)) ;
          equal (list string) []
            (List.map fk_text (foreign_keys [user; todo] user)) )
    ; test "foreign_keys use the column and table names" (fun () ->
          let a =
            model "a" ~table:"ta" [field "k" Int64 ~key:true ~column:"kk"]
          in
          let b =
            model "b" ~table:"tb"
              [id; field "x" Int64 ~column:"xx"; field "y" Int64]
              ~relations:
                [ Belongs_to {name= "r1"; target= "a"; key= "x"; references= "k"}
                ; Has_many {name= "h"; target= "a"}
                ; Belongs_to
                    {name= "r2"; target= "b"; key= "y"; references= "id"} ]
          in
          equal (list string)
            ["tb_xx_fk: xx -> ta.kk"; "tb_y_fk: y -> tb.id"]
            (List.map fk_text (foreign_keys [a; b] b)) )
    ; test "foreign_keys skip a Belongs_to that check refuses" (fun () ->
          let m =
            model "t"
              [id; field "u" Int64]
              ~relations:
                [ Belongs_to
                    {name= "a"; target= "none"; key= "u"; references= "id"}
                ; Belongs_to
                    {name= "b"; target= "t"; key= "none"; references= "id"}
                ; Belongs_to
                    {name= "c"; target= "t"; key= "u"; references= "none"} ]
          in
          equal (list string) [] (List.map fk_text (foreign_keys [m] m)) ) ]
