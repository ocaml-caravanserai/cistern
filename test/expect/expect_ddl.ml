(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* The DDL of push_schema and the comparison of check_schema (DESIGN.md,
   "DDL"). *)

open Windtrap
open Cistern_core
open Cistern_sql

let field ?column ?(nullable = false) ?(key = false) ?(auto = false)
    ?(unique = false) ?(index = false) name scalar : Schema.field =
  { name
  ; column= Option.value column ~default:name
  ; scalar
  ; nullable
  ; key
  ; auto
  ; unique
  ; index }

let model ?table ?(relations = []) name fields : Schema.model =
  {name; table= Option.value table ~default:name; fields; relations}

let id = field "id" Int64 ~key:true ~auto:true

let user =
  model "user"
    [id; field "name" String ~unique:true; field "age" Int ~nullable:true]
    ~relations:[Has_many {name= "todos"; target= "todo"}]

let todo =
  model "todo"
    [ id
    ; field "user_id" Int64
    ; field "title" String
    ; field "done_" Bool ~column:"done"
    ; field "tag" String ~index:true ~nullable:true ]
    ~relations:
      [ Belongs_to
          {name= "user"; target= "user"; key= "user_id"; references= "id"} ]

(* Each scalar, nullable and not, with and with no index. *)
let kinds =
  model "kinds"
    [ field "code" String ~key:true
    ; field "i64" Int64
    ; field "i" Int ~nullable:true
    ; field "s" String
    ; field "s_idx" String ~index:true
    ; field "b" Bool ~nullable:true
    ; field "f" Float
    ; field "y" Bytes ~nullable:true
    ; field "y_key" Bytes ~unique:true
    ; field "t" Ptime ]

let long =
  model "long" ~table:(String.make 40 'l')
    [id; field (String.make 30 'c') Int64 ~unique:true]

let ddl d models m = Ddl.create_table d models m

let tests_ddl =
  [ test "user and todo on MySQL" (fun () ->
        expect
          (String.concat "\n"
             [ddl Mysql [user; todo] user; ddl Mysql [user; todo] todo] )
        @@ __POS_OF__
             {|
          CREATE TABLE IF NOT EXISTS `user` (`id` BIGINT NOT NULL AUTO_INCREMENT, `name` VARCHAR(255) NOT NULL, `age` BIGINT NULL, PRIMARY KEY (`id`), UNIQUE KEY `user_name_key` (`name`)) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_bin ROW_FORMAT=DYNAMIC
          CREATE TABLE IF NOT EXISTS `todo` (`id` BIGINT NOT NULL AUTO_INCREMENT, `user_id` BIGINT NOT NULL, `title` MEDIUMTEXT NOT NULL, `done` TINYINT(1) NOT NULL, `tag` VARCHAR(255) NULL, PRIMARY KEY (`id`), KEY `todo_user_id_idx` (`user_id`), KEY `todo_tag_idx` (`tag`), CONSTRAINT `todo_user_id_fk` FOREIGN KEY (`user_id`) REFERENCES `user` (`id`) ON DELETE RESTRICT) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_bin ROW_FORMAT=DYNAMIC
          |} )
  ; test "the collation of MariaDB" (fun () ->
        expect (ddl Mariadb [user; todo] todo)
        @@ __POS_OF__
             {| CREATE TABLE IF NOT EXISTS `todo` (`id` BIGINT NOT NULL AUTO_INCREMENT, `user_id` BIGINT NOT NULL, `title` MEDIUMTEXT NOT NULL, `done` TINYINT(1) NOT NULL, `tag` VARCHAR(255) NULL, PRIMARY KEY (`id`), KEY `todo_user_id_idx` (`user_id`), KEY `todo_tag_idx` (`tag`), CONSTRAINT `todo_user_id_fk` FOREIGN KEY (`user_id`) REFERENCES `user` (`id`) ON DELETE RESTRICT) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_nopad_bin ROW_FORMAT=DYNAMIC |} )
  ; test "each column type" (fun () ->
        expect (ddl Mysql [kinds] kinds)
        @@ __POS_OF__
             {| CREATE TABLE IF NOT EXISTS `kinds` (`code` VARCHAR(255) NOT NULL, `i64` BIGINT NOT NULL, `i` BIGINT NULL, `s` MEDIUMTEXT NOT NULL, `s_idx` VARCHAR(255) NOT NULL, `b` TINYINT(1) NULL, `f` DOUBLE NOT NULL, `y` MEDIUMBLOB NULL, `y_key` VARBINARY(255) NOT NULL, `t` DATETIME(6) NOT NULL, PRIMARY KEY (`code`), KEY `kinds_s_idx_idx` (`s_idx`), UNIQUE KEY `kinds_y_key_key` (`y_key`)) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_bin ROW_FORMAT=DYNAMIC |} )
  ; test "a long name gets the hash" (fun () ->
        expect (ddl Mysql [long] long)
        @@ __POS_OF__
             {| CREATE TABLE IF NOT EXISTS `llllllllllllllllllllllllllllllllllllllll` (`id` BIGINT NOT NULL AUTO_INCREMENT, `cccccccccccccccccccccccccccccc` BIGINT NOT NULL, PRIMARY KEY (`id`), UNIQUE KEY `llllllllllllllllllllllllllllllllllllllll_cccccccccc_key_98f18644` (`cccccccccccccccccccccccccccccc`)) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_bin ROW_FORMAT=DYNAMIC |} )
  ]

(* The rows of information_schema for the DDL of [m] on a dialect, as the probe
   A7 of the plan of phase 2 found them. *)
let rows_of d (m : Schema.model) =
  List.map
    (fun (f : Schema.field) ->
      let data_type, max_length, precision, text =
        match Schema.column_type m f with
        | Bigint ->
            ("bigint", None, None, false)
        | Varchar_255 ->
            ("varchar", Some 255, None, true)
        | Mediumtext ->
            ("mediumtext", Some 16777215, None, true)
        | Tinyint_1 ->
            ("tinyint", None, None, false)
        | Double ->
            ("double", None, None, false)
        | Varbinary_255 ->
            ("varbinary", Some 255, None, false)
        | Mediumblob ->
            ("mediumblob", Some 16777215, None, false)
        | Datetime_6 ->
            ("datetime", None, Some 6, false)
      in
      { Ddl.table= m.table
      ; column= f.column
      ; data_type
      ; nullable= f.nullable
      ; max_length
      ; precision
      ; collation= (if text then Some (Dialect.collation d) else None)
      ; unsigned= false
      ; auto_increment= f.auto } )
    m.fields

let change table column f rows =
  List.map
    (fun (r : Ddl.column) ->
      if r.table = table && r.column = column then f r else r )
    rows

let messages d models rows =
  match Ddl.compare d models rows with
  | [] ->
      "equal"
  | ms ->
      String.concat "\n" ms

let all d = rows_of d user @ rows_of d todo @ rows_of d kinds

let models = [user; todo; kinds]

let tests_compare =
  [ test "equal on the two dialects" (fun () ->
        expect
          (String.concat "\n"
             [ messages Mysql models (all Mysql)
             ; messages Mariadb models (all Mariadb) ] )
        @@ __POS_OF__ {|
          equal
          equal
          |} )
  ; test "the collation of the other dialect" (fun () ->
        expect (messages Mariadb models (all Mysql))
        @@ __POS_OF__
             {|
          "user"."name": the collation is "utf8mb4_0900_bin", the model needs utf8mb4_nopad_bin
          "todo"."title": the collation is "utf8mb4_0900_bin", the model needs utf8mb4_nopad_bin
          "todo"."tag": the collation is "utf8mb4_0900_bin", the model needs utf8mb4_nopad_bin
          "kinds"."code": the collation is "utf8mb4_0900_bin", the model needs utf8mb4_nopad_bin
          "kinds"."s": the collation is "utf8mb4_0900_bin", the model needs utf8mb4_nopad_bin
          "kinds"."s_idx": the collation is "utf8mb4_0900_bin", the model needs utf8mb4_nopad_bin
          |} )
  ; test "a missing table, a missing column and an extra column" (fun () ->
        let rows =
          List.filter
            (fun (r : Ddl.column) -> r.table <> "todo" && r.column <> "age")
            (all Mysql)
          @ [ {(List.hd (rows_of Mysql user)) with column= "extra"}
            ; {(List.hd (rows_of Mysql user)) with table= "other"} ]
        in
        expect (messages Mysql models rows)
        @@ __POS_OF__
             {|
          the table "user" has no column "age"
          the table "user" has the column "extra", which the model "user" does not have
          the table "todo" does not exist
          |} )
  ; test "the column names match with no regard to ASCII case" (fun () ->
        let rows =
          change "user" "name" (fun r -> {r with column= "NAME"}) (all Mysql)
        in
        expect (messages Mysql models rows) @@ __POS_OF__ {| equal |} )
  ; test "another type, with no other message" (fun () ->
        let rows =
          change "user" "name"
            (fun r ->
              { r with
                data_type= "mediumtext"
              ; max_length= Some 16777215
              ; nullable= true } )
            (all Mysql)
          |> change "kinds" "f" (fun r -> {r with data_type= "FLOAT"})
          |> change "kinds" "i64" (fun r -> {r with data_type= "BIGINT"})
        in
        expect (messages Mysql models rows)
        @@ __POS_OF__
             {|
          "user"."name": the type is "mediumtext", the model needs varchar
          "kinds"."f": the type is "FLOAT", the model needs double
          |} )
  ; test "the nullability" (fun () ->
        let rows =
          change "user" "name" (fun r -> {r with nullable= true}) (all Mysql)
          |> change "user" "age" (fun r -> {r with nullable= false})
        in
        expect (messages Mysql models rows)
        @@ __POS_OF__
             {|
          "user"."name": the column is NULL, the model needs NOT NULL
          "user"."age": the column is NOT NULL, the model needs NULL
          |} )
  ; test "the length" (fun () ->
        let rows =
          change "user" "name"
            (fun r -> {r with max_length= Some 50})
            (all Mysql)
          |> change "kinds" "y" (fun r -> {r with max_length= Some 65535})
          |> change "kinds" "i64" (fun r -> {r with max_length= Some 1})
        in
        expect (messages Mysql models rows)
        @@ __POS_OF__
             {|
          "user"."name": the length is 50, the model needs 255
          "kinds"."i64": the length is 1, the model needs none
          "kinds"."y": the length is 65535, the model needs 16777215
          |} )
  ; test "the precision" (fun () ->
        let rows =
          change "kinds" "t" (fun r -> {r with precision= Some 0}) (all Mysql)
          |> change "kinds" "f" (fun r -> {r with precision= Some 6})
        in
        expect (messages Mysql models rows)
        @@ __POS_OF__
             {|
          "kinds"."f": the precision is 6, the model needs none
          "kinds"."t": the precision is 0, the model needs 6
          |} )
  ; test "the collation" (fun () ->
        let rows =
          change "user" "name"
            (fun r -> {r with collation= Some "utf8mb4_0900_ai_ci"})
            (all Mysql)
          |> change "kinds" "y" (fun r ->
              {r with collation= Some "utf8mb4_0900_bin"} )
          |> change "kinds" "s" (fun r -> {r with collation= None})
        in
        expect (messages Mysql models rows)
        @@ __POS_OF__
             {|
          "user"."name": the collation is "utf8mb4_0900_ai_ci", the model needs utf8mb4_0900_bin
          "kinds"."s": the collation is none, the model needs utf8mb4_0900_bin
          "kinds"."y": the collation is "utf8mb4_0900_bin", the model needs none
          |} )
  ; test "an unsigned column" (fun () ->
        let rows =
          change "user" "id" (fun r -> {r with unsigned= true}) (all Mysql)
        in
        expect (messages Mysql models rows)
        @@ __POS_OF__ {| "user"."id": the column is unsigned |} )
  ; test "AUTO_INCREMENT" (fun () ->
        let rows =
          change "user" "id"
            (fun r -> {r with auto_increment= false})
            (all Mysql)
          |> change "kinds" "i64" (fun r -> {r with auto_increment= true})
        in
        expect (messages Mysql models rows)
        @@ __POS_OF__
             {|
          "user"."id": the column has no AUTO_INCREMENT, the model needs it ([@auto])
          "kinds"."i64": the column has AUTO_INCREMENT, the model has no [@auto]
          |} )
  ; test "no rows" (fun () ->
        expect (messages Mysql models [])
        @@ __POS_OF__
             {|
          the table "user" does not exist
          the table "todo" does not exist
          the table "kinds" does not exist
          |} )
  ]

let tests =
  group "DDL" [group "create_table" tests_ddl; group "compare" tests_compare]
