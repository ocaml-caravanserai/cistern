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

(* The tests of the types, the indexes and the foreign keys. *)
let shapes =
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
  ; test "indexes in the order of the fields, with the column names" (fun () ->
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
          model "t" [id; field "u" Int64 ~index:true] ~relations:[belongs_to "u"]
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
              [Belongs_to {name= "r"; target= "p"; key= "id"; references= "id"}]
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
              ; Belongs_to {name= "r2"; target= "b"; key= "y"; references= "id"}
              ]
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
              ; Belongs_to {name= "c"; target= "t"; key= "u"; references= "none"}
              ]
        in
        equal (list string) [] (List.map fk_text (foreign_keys [m] m)) ) ]

(* The rows of check. *)

let simple ?table ?relations name = model ?table ?relations name [id]

let to_ p key = Belongs_to {name= "to_" ^ p; target= p; key; references= "id"}

let rule_of = function Ok () -> "" | Error e -> e

let c n = String.make n 'c'

(* Each row: a name, the models, the expected messages. *)
let checks =
  [ ("user and todo", [user; todo], [])
  ; ( "an optional Belongs_to"
    , [ simple "p"
      ; model "a" [id; field "u" Int64 ~nullable:true] ~relations:[to_ "p" "u"]
      ]
    , [] )
  ; ( "rule 1: model names"
    , [simple "a"; simple "a" ~table:"b"]
    , ["2 models have the name \"a\""] )
  ; ( "rule 1: table names"
    , [ simple "a" ~table:"User"
      ; simple "b" ~table:"user"
      ; simple "c" ~table:"USER" ]
    , [ "the tables \"User\", \"user\", \"USER\" of the models \"a\", \"b\", \
         \"c\" are equal with no regard to ASCII case"
      ; "model \"a\": the table \"User\" has an uppercase ASCII letter \
         (version 1 permits only lowercase table names)"
      ; "model \"c\": the table \"USER\" has an uppercase ASCII letter \
         (version 1 permits only lowercase table names)" ] )
  ; ( "rule 2: table name"
    , [simple "a" ~table:(String.concat "" (List.init 64 (fun _ -> "€")))]
    , [ "model \"a\": "
        ^ rule_of
            (Cistern_names.valid_table_name
               (String.concat "" (List.init 64 (fun _ -> "€"))) ) ] )
  ; ( "rule 2: an uppercase ASCII letter in a table name"
    , [simple "a" ~table:"userA"; simple "b" ~table:"Z"]
    , [ "model \"a\": the table \"userA\" has an uppercase ASCII letter \
         (version 1 permits only lowercase table names)"
      ; "model \"b\": the table \"Z\" has an uppercase ASCII letter (version 1 \
         permits only lowercase table names)" ] )
  ; ( "rule 2: a table name with a non-ASCII uppercase letter"
    , [simple "a" ~table:"\xc3\x89t\xc3\xa9"]
    , [] )
  ; ( "rule 2: column name"
    , [model "a" [id; field "x" Int ~column:"x "]]
    , ["model \"a\", field \"x\": the name \"x \" ends with white space"] )
  ; ( "rule 3: no key"
    , [model "a" [field "x" Int]]
    , ["model \"a\": no field has [@key]"] )
  ; ("rule 3: no field", [model "a" []], ["model \"a\": no field has [@key]"])
  ; ( "rule 3: two keys"
    , [model "a" [id; field "x" Int ~key:true]]
    , ["model \"a\": 2 fields have [@key] (version 1 has one key)"] )
  ; ( "rule 3: an option key"
    , [model "a" [field "id" Int64 ~key:true ~nullable:true]]
    , ["model \"a\", field \"id\": the key is an option"] )
  ; ( "rule 3: a float key"
    , [model "a" [field "id" Float ~key:true]]
    , ["model \"a\", field \"id\": the key is a float"] )
  ; ( "rule 4: auto on a field that is not the key"
    , [model "a" [id; field "x" Int64 ~auto:true]]
    , ["model \"a\", field \"x\": [@auto] is only on the key"] )
  ; ( "rule 4: auto on an int key"
    , [model "a" [field "id" Int ~key:true ~auto:true]]
    , ["model \"a\", field \"id\": [@auto] is only on an int64 field"] )
  ; ( "rule 5: column names, and no rule 10"
    , [ model "a"
          [ id
          ; field "e1" String ~column:"Email" ~unique:true
          ; field "e2" String ~column:"email" ~unique:true
          ; field "e3" String ~column:"EMAIL" ~unique:true ] ]
    , [ "model \"a\": the columns \"Email\", \"email\", \"EMAIL\" are equal \
         with no regard to ASCII case" ] )
  ; ( "rule 5: é and É pass (the fold is ASCII only)"
    , [ model "a"
          [id; field "e1" String ~column:"é"; field "e2" String ~column:"É"] ]
    , [] )
  ; ( "rule 6: a unique key"
    , [model "a" [field "id" Int64 ~key:true ~unique:true]]
    , [ "model \"a\", field \"id\": the key has no [@unique] or [@index] (it \
         has an index)" ] )
  ; ( "rule 6: an indexed key"
    , [model "a" [field "id" Int64 ~key:true ~index:true]]
    , [ "model \"a\", field \"id\": the key has no [@unique] or [@index] (it \
         has an index)" ] )
  ; ( "rule 6: unique and index"
    , [model "a" [id; field "x" Int ~unique:true ~index:true]]
    , [ "model \"a\", field \"x\": [@unique] and [@index] together (the unique \
         index is an index)" ] )
  ; ( "rule 7: relation names"
    , [ simple "a"
          ~relations:
            [ Has_many {name= "r"; target= "b"}
            ; Has_many {name= "r"; target= "b"} ]
      ; model "b" [id; field "a_id" Int64] ~relations:[to_ "a" "a_id"] ]
    , ["model \"a\": the name \"r\" is used by 2 relations"] )
  ; ( "rule 7: a relation with the name of a field"
    , [ model "a"
          [id; field "u" Int64]
          ~relations:
            [Belongs_to {name= "u"; target= "a"; key= "u"; references= "id"}] ]
    , ["model \"a\": the name \"u\" is used by a relation and a field"] )
  ; ( "rule 7: two relations and a field"
    , [ model "a"
          [id; field "u" Int64]
          ~relations:
            [ Belongs_to {name= "u"; target= "a"; key= "u"; references= "id"}
            ; Has_many {name= "h"; target= "a"}
            ; Has_many {name= "u"; target= "a"} ] ]
    , ["model \"a\": the name \"u\" is used by 2 relations and a field"] )
  ; ( "rule 7: the names in the order of the relations"
    , [ simple "a"
          ~relations:
            [ Has_many {name= "z"; target= "b"}
            ; Has_many {name= "b"; target= "b"}
            ; Has_many {name= "z"; target= "b"}
            ; Has_many {name= "b"; target= "b"} ]
      ; model "b" [id; field "a_id" Int64] ~relations:[to_ "a" "a_id"] ]
    , [ "model \"a\": the name \"z\" is used by 2 relations"
      ; "model \"a\": the name \"b\" is used by 2 relations" ] )
  ; ( "rule 5 comes before rule 6"
    , [ model "a"
          [ id
          ; field "y" String ~unique:true ~index:true
          ; field "e1" String ~column:"E"
          ; field "e2" String ~column:"e" ] ]
    , [ "model \"a\": the columns \"E\", \"e\" are equal with no regard to \
         ASCII case"
      ; "model \"a\", field \"y\": [@unique] and [@index] together (the unique \
         index is an index)" ] )
  ; ( "rule 8: a Belongs_to on its own key"
    , [ simple "t"
          ~relations:
            [Belongs_to {name= "r"; target= "t"; key= "id"; references= "id"}]
      ]
    , [ "model \"t\", relation \"r\": the Belongs_to refers to its own key \
         (MySQL refuses this foreign key)" ] )
  ; ( "rule 8: a Belongs_to to its own model"
    , [ model "t"
          [id; field "parent" Int64 ~nullable:true]
          ~relations:[to_ "t" "parent"] ]
    , [] )
  ; ( "rule 8: two Belongs_to on one key, and no rule 11 message"
    , [ simple "p"
      ; simple "q"
      ; model "b" [id; field "x" Int64] ~relations:[to_ "p" "x"; to_ "q" "x"] ]
    , ["model \"b\": 2 Belongs_to use the key \"x\""] )
  ; ( "names in messages keep UTF-8, and escape controls"
    , [model "café" []; model "a\nb" ~table:"ab" []; simple "café" ~table:"c2"]
    , [ "2 models have the name \"café\""
      ; "model \"café\": no field has [@key]"
      ; "model \"a\\u{A}b\": no field has [@key]" ] )
  ; ( "rule 8: no key field"
    , [simple "a" ~relations:[to_ "a" "none"]]
    , [ "model \"a\", relation \"to_a\": the key \"none\" is not a field of \
         the model" ] )
  ; ( "rule 8: no target"
    , [model "a" [id; field "u" Int64] ~relations:[to_ "zz" "u"]]
    , [ "model \"a\", relation \"to_zz\": the target \"zz\" is not a model of \
         the list" ] )
  ; ( "rule 8: no key field and no target"
    , [simple "a" ~relations:[to_ "zz" "none"]]
    , [ "model \"a\", relation \"to_zz\": the key \"none\" is not a field of \
         the model"
      ; "model \"a\", relation \"to_zz\": the target \"zz\" is not a model of \
         the list" ] )
  ; ( "rule 8: references a field that is not the key"
    , [ model "a"
          [id; field "u" Int64]
          ~relations:
            [Belongs_to {name= "r"; target= "a"; key= "u"; references= "u"}] ]
    , [ "model \"a\", relation \"r\": references \"u\" is not the key \"id\" \
         of \"a\" (version 1 refers only to the key)" ] )
  ; ( "rule 8: no key field, and not the key"
    , [ model "a"
          [id; field "u" Int64]
          ~relations:
            [Belongs_to {name= "r"; target= "a"; key= "v"; references= "u"}] ]
    , [ "model \"a\", relation \"r\": the key \"v\" is not a field of the model"
      ; "model \"a\", relation \"r\": references \"u\" is not the key \"id\" \
         of \"a\" (version 1 refers only to the key)" ] )
  ; ( "rule 8: different types"
    , [simple "p"; model "a" [id; field "u" Int] ~relations:[to_ "p" "u"]]
    , [ "model \"a\", relation \"to_p\": the key \"u\" and the field \"id\" of \
         \"p\" have different types" ] )
  ; ( "rule 8: a target with no key"
    , [ model "p" [field "x" Int]
      ; model "a" [id; field "u" Int] ~relations:[to_ "p" "u"] ]
    , ["model \"p\": no field has [@key]"] )
  ; ( "rule 9: no target"
    , [simple "a" ~relations:[Has_many {name= "h"; target= "zz"}]]
    , [ "model \"a\", relation \"h\": the target \"zz\" is not a model of the \
         list" ] )
  ; ( "rule 9: no opposite"
    , [simple "a" ~relations:[Has_many {name= "h"; target= "b"}]; simple "b"]
    , ["model \"a\", relation \"h\": the model \"b\" has no Belongs_to to \"a\""]
    )
  ; ( "rule 9: two opposites"
    , [ simple "a" ~relations:[Has_many {name= "h"; target= "b"}]
      ; model "b"
          [id; field "x" Int64; field "y" Int64; field "z" Int64]
          ~relations:
            [ Belongs_to
                {name= "author"; target= "a"; key= "x"; references= "id"}
            ; Has_many {name= "self"; target= "b"}
            ; Belongs_to
                {name= "editor"; target= "a"; key= "y"; references= "id"}
            ; Belongs_to {name= "up"; target= "b"; key= "z"; references= "id"}
            ] ]
    , [ "model \"a\", relation \"h\": the model \"b\" has 2 Belongs_to to \
         \"a\" (version 1 has no via)" ] )
  ; ( "rule 10: a collision of the hash"
    , [ model "t"
          [ id
          ; field "x" Int ~index:true ~column:(c 55 ^ "jfscoge")
          ; field "y" Int ~index:true ~column:(c 55 ^ "ijgfiz") ] ]
    , [ Printf.sprintf
          "model \"t\": the index names \"t_%s_idx_b5257082\", \
           \"t_%s_idx_b5257082\" of the columns \"%sjfscoge\", \"%sijgfiz\" \
           are equal with no regard to ASCII case (a collision of the hash: \
           rename one column)"
          (c 49) (c 49) (c 55) (c 55) ] )
  ; ( "rule 10: a collision of the hash, with names that differ in case"
    , [ model "t"
          [ id
          ; field "x" Int ~index:true ~column:(c 55 ^ "bssxq")
          ; field "y" Int ~index:true ~column:(String.make 55 'C' ^ "uajpa") ]
      ]
    , [ Printf.sprintf
          "model \"t\": the index names \"t_%s_idx_62b26a16\", \
           \"t_%s_idx_62b26a16\" of the columns \"%sbssxq\", \"%suajpa\" are \
           equal with no regard to ASCII case"
          (c 49) (String.make 49 'C') (c 55) (String.make 55 'C') ] )
  ; ( "rule 10 does not run after rule 2"
    , [ model "t"
          [ id
          ; field "x" Int ~index:true ~column:(c 55 ^ "jfscoge")
          ; field "y" Int ~index:true ~column:(c 55 ^ "ijgfiz")
          ; field "z" Int ~column:"z`" ] ]
    , ["model \"t\", field \"z\": the name \"z`\" has a backquote"] )
  ; ( "rule 11: foreign key names"
    , [ simple "p"
      ; model "a" ~table:"t" [id; field "a_b" Int64] ~relations:[to_ "p" "a_b"]
      ; model "b" ~table:"t_a" [id; field "b" Int64] ~relations:[to_ "p" "b"] ]
    , [ "the foreign key names \"t_a_b_fk\", \"t_a_b_fk\" of the columns \
         \"t.a_b\", \"t_a.b\" are equal with no regard to ASCII case (a \
         constraint name is unique in the database)" ] )
  ; ( "rule 11: foreign key names that differ in case"
    , [ simple "p"
      ; model "a" ~table:"a_b" [id; field "c" Int64] ~relations:[to_ "p" "c"]
      ; model "b" ~table:"a" [id; field "B_c" Int64] ~relations:[to_ "p" "B_c"]
      ]
    , [ "the foreign key names \"a_b_c_fk\", \"a_B_c_fk\" of the columns \
         \"a_b.c\", \"a.B_c\" are equal with no regard to ASCII case (a \
         constraint name is unique in the database)" ] )
  ; ( "rule 11 does not run after rule 1"
    , [ simple "p"
      ; model "a" ~table:"x" [id; field "c" Int64] ~relations:[to_ "p" "c"]
      ; model "b" ~table:"x" [id; field "c" Int64] ~relations:[to_ "p" "c"] ]
    , [ "the tables \"x\", \"x\" of the models \"a\", \"b\" are equal with no \
         regard to ASCII case" ] )
  ; ( "rule 11 does not run for a model that breaks rule 2"
    , [ simple "p"
      ; model "a" ~table:"t" [id; field "a_b" Int64] ~relations:[to_ "p" "a_b"]
      ; model "b" ~table:"t_a"
          [id; field "b" Int64; field "z" Int ~column:"z`"]
          ~relations:[to_ "p" "b"] ]
    , ["model \"b\", field \"z\": the name \"z`\" has a backquote"] )
  ; ( "more faults, in order"
    , [ model "a" ~table:"a`"
          [field "x" Int ~auto:true; field "y" String ~unique:true ~index:true]
      ; simple "b" ~table:"b " ]
    , [ "model \"a\": the name \"a`\" has a backquote"
      ; "model \"a\": no field has [@key]"
      ; "model \"a\", field \"x\": [@auto] is only on the key"
      ; "model \"a\", field \"x\": [@auto] is only on an int64 field"
      ; "model \"a\", field \"y\": [@unique] and [@index] together (the unique \
         index is an index)"
      ; "model \"b\": the name \"b \" ends with white space" ] ) ]

let tests =
  group "Schema"
    ( shapes
    @ [ cases "check"
          ~id:(fun (n, _, _) -> n)
          checks
          (fun (_, models, expected) ->
            equal
              (result unit (list string))
              (if expected = [] then Ok () else Error expected)
              (check models) ) ] )
