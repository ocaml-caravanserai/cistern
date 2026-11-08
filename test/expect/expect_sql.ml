(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* The SQL of each statement for MySQL and MariaDB, and its redacted copy
   (DESIGN.md, "Schema, AST and SQL"). Each test prints the SQL of MySQL, the
   SQL of MariaDB if it is not the same, and the redacted copy. *)

open Windtrap
open Cistern_core
open Cistern_sql

let sfield ?(nullable = false) ?(key = false) ?(auto = false) ?column name
    scalar : Schema.field =
  { name
  ; column= Option.value column ~default:name
  ; scalar
  ; nullable
  ; key
  ; auto
  ; unique= false
  ; index= false }

type row = {id: int64; name: string; n: int64 option; s: string option; x: float}

let f_id = sfield "id" Int64 ~key:true ~auto:true

let f_name = sfield "name" String

let f_n = sfield "n" Int64 ~nullable:true

let f_s = sfield "s" String ~nullable:true

let f_x = sfield "x" Float ~column:"done"

let t : Schema.model =
  {name= "t"; table= "t"; fields= [f_id; f_name; f_n; f_s; f_x]; relations= []}

let opt f = function None -> Value.Null | Some v -> f v

let id =
  Field.make ~model:t ~field:f_id
    ~encode:(fun i -> Value.Int i)
    ~decode:Decoder.int64
    ~get:(fun r -> r.id)
    ~set:(fun r v -> {r with id= v})

let name =
  Field.make ~model:t ~field:f_name
    ~encode:(fun s -> Value.Text s)
    ~decode:Decoder.string
    ~get:(fun r -> r.name)
    ~set:(fun r v -> {r with name= v})

let n =
  Field.make ~model:t ~field:f_n
    ~encode:(opt (fun i -> Value.Int i))
    ~decode:(Decoder.option Decoder.int64)
    ~get:(fun r -> r.n)
    ~set:(fun r v -> {r with n= v})

let s =
  Field.make ~model:t ~field:f_s
    ~encode:(opt (fun v -> Value.Text v))
    ~decode:(Decoder.option Decoder.string)
    ~get:(fun r -> r.s)
    ~set:(fun r v -> {r with s= v})

let x =
  Field.make ~model:t ~field:f_x
    ~encode:(fun f -> Value.Float f)
    ~decode:Decoder.float
    ~get:(fun r -> r.x)
    ~set:(fun r v -> {r with x= v})

(* A model with only an [@auto] key, and a model whose key is a string. *)
let auto_only : Schema.model =
  {name= "auto_only"; table= "auto_only"; fields= [f_id]; relations= []}

let coded : Schema.model =
  { name= "coded"
  ; table= "co`ded"
  ; fields= [sfield "code" String ~key:true; sfield "v" Int]
  ; relations= [] }

let col f = {Model_ast.table= Field.table f; column= Field.column f}

let q ?(where = []) ?(order = []) ?limit ?offset ?(lock = Ast.No_lock) () =
  {Compile.model= t; where= List.map Expr.ast where; order; limit; offset; lock}

let show r =
  match r with
  | Error msg ->
      "Error: " ^ msg
  | Ok a ->
      let my = Print.to_sql Mysql a and maria = Print.to_sql Mariadb a in
      let red = Print.redacted Mysql a in
      String.concat "\n"
        ( [my]
        @ (if maria = my then [] else ["MariaDB: " ^ maria])
        @ ["redacted: " ^ red]
        @
        let red2 = Print.redacted Mariadb a in
        if red2 = red then [] else ["redacted MariaDB: " ^ red2] )

let where e = show (Compile.select (q ~where:[e] ()))

let tests =
  let open Expr.Ops in
  group "SQL"
    [ test "select all" (fun () ->
          expect (show (Compile.select (q ())))
          @@ __POS_OF__
               {|
            SELECT `t`.`id`, `t`.`name`, `t`.`n`, `t`.`s`, `t`.`done` FROM `t`
            redacted: SELECT `t`.`id`, `t`.`name`, `t`.`n`, `t`.`s`, `t`.`done` FROM `t`
            |} )
    ; test "== on a field that is not an option" (fun () ->
          expect (where (name ==. "ab"))
          @@ __POS_OF__
               {|
            SELECT `t`.`id`, `t`.`name`, `t`.`n`, `t`.`s`, `t`.`done` FROM `t` WHERE `t`.`name` = _utf8mb4 X'6162'
            redacted: SELECT `t`.`id`, `t`.`name`, `t`.`n`, `t`.`s`, `t`.`done` FROM `t` WHERE `t`.`name` = ?
            |} )
    ; test "<> on a field that is not an option" (fun () ->
          expect (where (name <>. "ab"))
          @@ __POS_OF__
               {|
            SELECT `t`.`id`, `t`.`name`, `t`.`n`, `t`.`s`, `t`.`done` FROM `t` WHERE `t`.`name` <> _utf8mb4 X'6162'
            redacted: SELECT `t`.`id`, `t`.`name`, `t`.`n`, `t`.`s`, `t`.`done` FROM `t` WHERE `t`.`name` <> ?
            |} )
    ; test "the orders on a field that is not an option" (fun () ->
          expect
            (String.concat "\n"
               [ where (id <. 1L)
               ; where (id >. 2L)
               ; where (id <=. 3L)
               ; where (id >=. 4L) ] )
          @@ __POS_OF__
               {|
            SELECT `t`.`id`, `t`.`name`, `t`.`n`, `t`.`s`, `t`.`done` FROM `t` WHERE `t`.`id` < 1
            redacted: SELECT `t`.`id`, `t`.`name`, `t`.`n`, `t`.`s`, `t`.`done` FROM `t` WHERE `t`.`id` < ?
            SELECT `t`.`id`, `t`.`name`, `t`.`n`, `t`.`s`, `t`.`done` FROM `t` WHERE `t`.`id` > 2
            redacted: SELECT `t`.`id`, `t`.`name`, `t`.`n`, `t`.`s`, `t`.`done` FROM `t` WHERE `t`.`id` > ?
            SELECT `t`.`id`, `t`.`name`, `t`.`n`, `t`.`s`, `t`.`done` FROM `t` WHERE `t`.`id` <= 3
            redacted: SELECT `t`.`id`, `t`.`name`, `t`.`n`, `t`.`s`, `t`.`done` FROM `t` WHERE `t`.`id` <= ?
            SELECT `t`.`id`, `t`.`name`, `t`.`n`, `t`.`s`, `t`.`done` FROM `t` WHERE `t`.`id` >= 4
            redacted: SELECT `t`.`id`, `t`.`name`, `t`.`n`, `t`.`s`, `t`.`done` FROM `t` WHERE `t`.`id` >= ?
            |} )
    ; test "the rows of 3.2 on an option field" (fun () ->
          expect
            (String.concat "\n"
               [ where (n ==. Some 1L)
               ; where (n ==. None)
               ; where (is_null n)
               ; where (n <>. Some 1L)
               ; where (n <>. None)
               ; where (n <. Some 1L)
               ; where (n >=. Some (-1L))
               ; where (in_ n [Some 1L; Some 2L])
               ; where (in_ n [None; Some 1L])
               ; where (in_ n [None])
               ; where (in_ n [])
               ; where (starts_with_opt s "a")
               ; where (contains_opt s "a") ] )
          @@ __POS_OF__
               {|
            SELECT `t`.`id`, `t`.`name`, `t`.`n`, `t`.`s`, `t`.`done` FROM `t` WHERE `t`.`n` <=> 1
            redacted: SELECT `t`.`id`, `t`.`name`, `t`.`n`, `t`.`s`, `t`.`done` FROM `t` WHERE `t`.`n` <=> ?
            SELECT `t`.`id`, `t`.`name`, `t`.`n`, `t`.`s`, `t`.`done` FROM `t` WHERE `t`.`n` IS NULL
            redacted: SELECT `t`.`id`, `t`.`name`, `t`.`n`, `t`.`s`, `t`.`done` FROM `t` WHERE `t`.`n` IS NULL
            SELECT `t`.`id`, `t`.`name`, `t`.`n`, `t`.`s`, `t`.`done` FROM `t` WHERE `t`.`n` IS NULL
            redacted: SELECT `t`.`id`, `t`.`name`, `t`.`n`, `t`.`s`, `t`.`done` FROM `t` WHERE `t`.`n` IS NULL
            SELECT `t`.`id`, `t`.`name`, `t`.`n`, `t`.`s`, `t`.`done` FROM `t` WHERE NOT (`t`.`n` <=> 1)
            redacted: SELECT `t`.`id`, `t`.`name`, `t`.`n`, `t`.`s`, `t`.`done` FROM `t` WHERE NOT (`t`.`n` <=> ?)
            SELECT `t`.`id`, `t`.`name`, `t`.`n`, `t`.`s`, `t`.`done` FROM `t` WHERE `t`.`n` IS NOT NULL
            redacted: SELECT `t`.`id`, `t`.`name`, `t`.`n`, `t`.`s`, `t`.`done` FROM `t` WHERE `t`.`n` IS NOT NULL
            SELECT `t`.`id`, `t`.`name`, `t`.`n`, `t`.`s`, `t`.`done` FROM `t` WHERE (`t`.`n` IS NOT NULL AND `t`.`n` < 1)
            redacted: SELECT `t`.`id`, `t`.`name`, `t`.`n`, `t`.`s`, `t`.`done` FROM `t` WHERE (`t`.`n` IS NOT NULL AND `t`.`n` < ?)
            SELECT `t`.`id`, `t`.`name`, `t`.`n`, `t`.`s`, `t`.`done` FROM `t` WHERE (`t`.`n` IS NOT NULL AND `t`.`n` >= -1)
            redacted: SELECT `t`.`id`, `t`.`name`, `t`.`n`, `t`.`s`, `t`.`done` FROM `t` WHERE (`t`.`n` IS NOT NULL AND `t`.`n` >= ?)
            SELECT `t`.`id`, `t`.`name`, `t`.`n`, `t`.`s`, `t`.`done` FROM `t` WHERE (`t`.`n` IS NOT NULL AND `t`.`n` IN (1, 2))
            redacted: SELECT `t`.`id`, `t`.`name`, `t`.`n`, `t`.`s`, `t`.`done` FROM `t` WHERE (`t`.`n` IS NOT NULL AND `t`.`n` IN (?, ?))
            SELECT `t`.`id`, `t`.`name`, `t`.`n`, `t`.`s`, `t`.`done` FROM `t` WHERE (`t`.`n` IN (1) OR `t`.`n` IS NULL)
            redacted: SELECT `t`.`id`, `t`.`name`, `t`.`n`, `t`.`s`, `t`.`done` FROM `t` WHERE (`t`.`n` IN (?) OR `t`.`n` IS NULL)
            SELECT `t`.`id`, `t`.`name`, `t`.`n`, `t`.`s`, `t`.`done` FROM `t` WHERE `t`.`n` IS NULL
            redacted: SELECT `t`.`id`, `t`.`name`, `t`.`n`, `t`.`s`, `t`.`done` FROM `t` WHERE `t`.`n` IS NULL
            SELECT `t`.`id`, `t`.`name`, `t`.`n`, `t`.`s`, `t`.`done` FROM `t` WHERE FALSE
            redacted: SELECT `t`.`id`, `t`.`name`, `t`.`n`, `t`.`s`, `t`.`done` FROM `t` WHERE FALSE
            SELECT `t`.`id`, `t`.`name`, `t`.`n`, `t`.`s`, `t`.`done` FROM `t` WHERE (`t`.`s` IS NOT NULL AND `t`.`s` LIKE _utf8mb4 X'6125' ESCAPE '!')
            redacted: SELECT `t`.`id`, `t`.`name`, `t`.`n`, `t`.`s`, `t`.`done` FROM `t` WHERE (`t`.`s` IS NOT NULL AND `t`.`s` LIKE ? ESCAPE '!')
            SELECT `t`.`id`, `t`.`name`, `t`.`n`, `t`.`s`, `t`.`done` FROM `t` WHERE (`t`.`s` IS NOT NULL AND `t`.`s` LIKE _utf8mb4 X'256125' ESCAPE '!')
            redacted: SELECT `t`.`id`, `t`.`name`, `t`.`n`, `t`.`s`, `t`.`done` FROM `t` WHERE (`t`.`s` IS NOT NULL AND `t`.`s` LIKE ? ESCAPE '!')
            |} )
    ; test "in_, starts_with and contains on a field that is not an option"
        (fun () ->
          expect
            (String.concat "\n"
               [ where (in_ id [1L; -2L])
               ; where (in_ id [])
               ; where (starts_with name "a%_!")
               ; where (contains name "") ] )
          @@ __POS_OF__
               {|
            SELECT `t`.`id`, `t`.`name`, `t`.`n`, `t`.`s`, `t`.`done` FROM `t` WHERE `t`.`id` IN (1, -2)
            redacted: SELECT `t`.`id`, `t`.`name`, `t`.`n`, `t`.`s`, `t`.`done` FROM `t` WHERE `t`.`id` IN (?, ?)
            SELECT `t`.`id`, `t`.`name`, `t`.`n`, `t`.`s`, `t`.`done` FROM `t` WHERE FALSE
            redacted: SELECT `t`.`id`, `t`.`name`, `t`.`n`, `t`.`s`, `t`.`done` FROM `t` WHERE FALSE
            SELECT `t`.`id`, `t`.`name`, `t`.`n`, `t`.`s`, `t`.`done` FROM `t` WHERE `t`.`name` LIKE _utf8mb4 X'612125215F212125' ESCAPE '!'
            redacted: SELECT `t`.`id`, `t`.`name`, `t`.`n`, `t`.`s`, `t`.`done` FROM `t` WHERE `t`.`name` LIKE ? ESCAPE '!'
            SELECT `t`.`id`, `t`.`name`, `t`.`n`, `t`.`s`, `t`.`done` FROM `t` WHERE `t`.`name` LIKE _utf8mb4 X'2525' ESCAPE '!'
            redacted: SELECT `t`.`id`, `t`.`name`, `t`.`n`, `t`.`s`, `t`.`done` FROM `t` WHERE `t`.`name` LIKE ? ESCAPE '!'
            |} )
    ; test "the nesting of &&, || and not" (fun () ->
          expect
            (String.concat "\n"
               [ where (id ==. 1L && (name ==. "a" || not (n ==. None)))
               ; where (not (id ==. 1L || id ==. 2L))
               ; where (not (not (id ==. 1L)))
               ; where (Expr.and_ [])
               ; where (Expr.or_ []) ] )
          @@ __POS_OF__
               {|
            SELECT `t`.`id`, `t`.`name`, `t`.`n`, `t`.`s`, `t`.`done` FROM `t` WHERE (`t`.`id` = 1 AND (`t`.`name` = _utf8mb4 X'61' OR NOT (`t`.`n` IS NULL)))
            redacted: SELECT `t`.`id`, `t`.`name`, `t`.`n`, `t`.`s`, `t`.`done` FROM `t` WHERE (`t`.`id` = ? AND (`t`.`name` = ? OR NOT (`t`.`n` IS NULL)))
            SELECT `t`.`id`, `t`.`name`, `t`.`n`, `t`.`s`, `t`.`done` FROM `t` WHERE NOT (`t`.`id` = 1 OR `t`.`id` = 2)
            redacted: SELECT `t`.`id`, `t`.`name`, `t`.`n`, `t`.`s`, `t`.`done` FROM `t` WHERE NOT (`t`.`id` = ? OR `t`.`id` = ?)
            SELECT `t`.`id`, `t`.`name`, `t`.`n`, `t`.`s`, `t`.`done` FROM `t` WHERE NOT (NOT (`t`.`id` = 1))
            redacted: SELECT `t`.`id`, `t`.`name`, `t`.`n`, `t`.`s`, `t`.`done` FROM `t` WHERE NOT (NOT (`t`.`id` = ?))
            SELECT `t`.`id`, `t`.`name`, `t`.`n`, `t`.`s`, `t`.`done` FROM `t` WHERE TRUE
            redacted: SELECT `t`.`id`, `t`.`name`, `t`.`n`, `t`.`s`, `t`.`done` FROM `t` WHERE TRUE
            SELECT `t`.`id`, `t`.`name`, `t`.`n`, `t`.`s`, `t`.`done` FROM `t` WHERE FALSE
            redacted: SELECT `t`.`id`, `t`.`name`, `t`.`n`, `t`.`s`, `t`.`done` FROM `t` WHERE FALSE
            |} )
    ; test "two filters" (fun () ->
          expect (show (Compile.select (q ~where:[id ==. 1L; name ==. "a"] ())))
          @@ __POS_OF__
               {|
            SELECT `t`.`id`, `t`.`name`, `t`.`n`, `t`.`s`, `t`.`done` FROM `t` WHERE (`t`.`id` = 1 AND `t`.`name` = _utf8mb4 X'61')
            redacted: SELECT `t`.`id`, `t`.`name`, `t`.`n`, `t`.`s`, `t`.`done` FROM `t` WHERE (`t`.`id` = ? AND `t`.`name` = ?)
            |} )
    ; test "order_by, limit and offset" (fun () ->
          expect
            (String.concat "\n"
               [ show
                   (Compile.select
                      (q ~order:[(col name, Compile.Asc); (col n, Desc)] ()) )
               ; show (Compile.select (q ~limit:10 ()))
               ; show (Compile.select (q ~offset:5 ()))
               ; show (Compile.select (q ~limit:0 ~offset:0 ()))
               ; show
                   (Compile.select
                      (q ~order:[(col name, Desc)] ~limit:10 ~offset:20 ()) )
               ; show
                   (Compile.select
                      (q ~order:[(col id, Desc); (col name, Asc)] ~limit:3 ()) )
               ] )
          @@ __POS_OF__
               {|
            SELECT `t`.`id`, `t`.`name`, `t`.`n`, `t`.`s`, `t`.`done` FROM `t` ORDER BY `t`.`name` ASC, `t`.`n` DESC
            redacted: SELECT `t`.`id`, `t`.`name`, `t`.`n`, `t`.`s`, `t`.`done` FROM `t` ORDER BY `t`.`name` ASC, `t`.`n` DESC
            SELECT `t`.`id`, `t`.`name`, `t`.`n`, `t`.`s`, `t`.`done` FROM `t` ORDER BY `t`.`id` ASC LIMIT 10
            redacted: SELECT `t`.`id`, `t`.`name`, `t`.`n`, `t`.`s`, `t`.`done` FROM `t` ORDER BY `t`.`id` ASC LIMIT 10
            SELECT `t`.`id`, `t`.`name`, `t`.`n`, `t`.`s`, `t`.`done` FROM `t` ORDER BY `t`.`id` ASC LIMIT 18446744073709551615 OFFSET 5
            redacted: SELECT `t`.`id`, `t`.`name`, `t`.`n`, `t`.`s`, `t`.`done` FROM `t` ORDER BY `t`.`id` ASC LIMIT 18446744073709551615 OFFSET 5
            SELECT `t`.`id`, `t`.`name`, `t`.`n`, `t`.`s`, `t`.`done` FROM `t` ORDER BY `t`.`id` ASC LIMIT 0 OFFSET 0
            redacted: SELECT `t`.`id`, `t`.`name`, `t`.`n`, `t`.`s`, `t`.`done` FROM `t` ORDER BY `t`.`id` ASC LIMIT 0 OFFSET 0
            SELECT `t`.`id`, `t`.`name`, `t`.`n`, `t`.`s`, `t`.`done` FROM `t` ORDER BY `t`.`name` DESC, `t`.`id` ASC LIMIT 10 OFFSET 20
            redacted: SELECT `t`.`id`, `t`.`name`, `t`.`n`, `t`.`s`, `t`.`done` FROM `t` ORDER BY `t`.`name` DESC, `t`.`id` ASC LIMIT 10 OFFSET 20
            SELECT `t`.`id`, `t`.`name`, `t`.`n`, `t`.`s`, `t`.`done` FROM `t` ORDER BY `t`.`id` DESC, `t`.`name` ASC LIMIT 3
            redacted: SELECT `t`.`id`, `t`.`name`, `t`.`n`, `t`.`s`, `t`.`done` FROM `t` ORDER BY `t`.`id` DESC, `t`.`name` ASC LIMIT 3
            |} )
    ; test "the locks" (fun () ->
          expect
            (String.concat "\n"
               [ show (Compile.select (q ~lock:For_update ()))
               ; show (Compile.select (q ~lock:For_share ~limit:1 ())) ] )
          @@ __POS_OF__
               {|
            SELECT `t`.`id`, `t`.`name`, `t`.`n`, `t`.`s`, `t`.`done` FROM `t` FOR UPDATE
            redacted: SELECT `t`.`id`, `t`.`name`, `t`.`n`, `t`.`s`, `t`.`done` FROM `t` FOR UPDATE
            SELECT `t`.`id`, `t`.`name`, `t`.`n`, `t`.`s`, `t`.`done` FROM `t` ORDER BY `t`.`id` ASC LIMIT 1 FOR SHARE
            MariaDB: SELECT `t`.`id`, `t`.`name`, `t`.`n`, `t`.`s`, `t`.`done` FROM `t` ORDER BY `t`.`id` ASC LIMIT 1 LOCK IN SHARE MODE
            redacted: SELECT `t`.`id`, `t`.`name`, `t`.`n`, `t`.`s`, `t`.`done` FROM `t` ORDER BY `t`.`id` ASC LIMIT 1 FOR SHARE
            redacted MariaDB: SELECT `t`.`id`, `t`.`name`, `t`.`n`, `t`.`s`, `t`.`done` FROM `t` ORDER BY `t`.`id` ASC LIMIT 1 LOCK IN SHARE MODE
            |} )
    ; test "first" (fun () ->
          expect
            (String.concat "\n"
               [ show (Compile.first (q ()))
               ; show (Compile.first (q ~limit:5 ~offset:2 ()))
               ; show (Compile.first (q ~limit:0 ()))
               ; show (Compile.first (q ~lock:For_update ())) ] )
          @@ __POS_OF__
               {|
            SELECT `t`.`id`, `t`.`name`, `t`.`n`, `t`.`s`, `t`.`done` FROM `t` ORDER BY `t`.`id` ASC LIMIT 1
            redacted: SELECT `t`.`id`, `t`.`name`, `t`.`n`, `t`.`s`, `t`.`done` FROM `t` ORDER BY `t`.`id` ASC LIMIT 1
            SELECT `t`.`id`, `t`.`name`, `t`.`n`, `t`.`s`, `t`.`done` FROM `t` ORDER BY `t`.`id` ASC LIMIT 1 OFFSET 2
            redacted: SELECT `t`.`id`, `t`.`name`, `t`.`n`, `t`.`s`, `t`.`done` FROM `t` ORDER BY `t`.`id` ASC LIMIT 1 OFFSET 2
            SELECT `t`.`id`, `t`.`name`, `t`.`n`, `t`.`s`, `t`.`done` FROM `t` ORDER BY `t`.`id` ASC LIMIT 0
            redacted: SELECT `t`.`id`, `t`.`name`, `t`.`n`, `t`.`s`, `t`.`done` FROM `t` ORDER BY `t`.`id` ASC LIMIT 0
            SELECT `t`.`id`, `t`.`name`, `t`.`n`, `t`.`s`, `t`.`done` FROM `t` ORDER BY `t`.`id` ASC LIMIT 1 FOR UPDATE
            redacted: SELECT `t`.`id`, `t`.`name`, `t`.`n`, `t`.`s`, `t`.`done` FROM `t` ORDER BY `t`.`id` ASC LIMIT 1 FOR UPDATE
            |} )
    ; test "count" (fun () ->
          expect
            (String.concat "\n"
               [ show (Compile.count (q ()))
               ; show
                   (Compile.count
                      (q
                         ~where:[n ==. Some 1L]
                         ~order:[(col name, Asc)]
                         ~lock:For_update () ) )
               ; show (Compile.count (q ~where:[n ==. Some 1L] ~limit:10 ()))
               ; show (Compile.count (q ~offset:3 ~lock:For_share ())) ] )
          @@ __POS_OF__
               {|
            SELECT COUNT(*) FROM `t`
            redacted: SELECT COUNT(*) FROM `t`
            SELECT COUNT(*) FROM `t` WHERE `t`.`n` <=> 1
            redacted: SELECT COUNT(*) FROM `t` WHERE `t`.`n` <=> ?
            SELECT COUNT(*) FROM (SELECT 1 FROM `t` WHERE `t`.`n` <=> 1 ORDER BY `t`.`id` ASC LIMIT 10) AS `t`
            redacted: SELECT COUNT(*) FROM (SELECT 1 FROM `t` WHERE `t`.`n` <=> ? ORDER BY `t`.`id` ASC LIMIT 10) AS `t`
            SELECT COUNT(*) FROM (SELECT 1 FROM `t` ORDER BY `t`.`id` ASC LIMIT 18446744073709551615 OFFSET 3) AS `t`
            redacted: SELECT COUNT(*) FROM (SELECT 1 FROM `t` ORDER BY `t`.`id` ASC LIMIT 18446744073709551615 OFFSET 3) AS `t`
            |} )
    ; test "exists" (fun () ->
          expect
            (String.concat "\n"
               [ show (Compile.exists (q ()))
               ; show
                   (Compile.exists
                      (q
                         ~where:[id >. 1L]
                         ~order:[(col name, Asc)]
                         ~lock:For_update () ) )
               ; show
                   (Compile.exists
                      (q ~order:[(col name, Asc)] ~limit:2 ~offset:1 ()) ) ] )
          @@ __POS_OF__
               {|
            SELECT EXISTS (SELECT 1 FROM `t`)
            redacted: SELECT EXISTS (SELECT 1 FROM `t`)
            SELECT EXISTS (SELECT 1 FROM `t` WHERE `t`.`id` > 1)
            redacted: SELECT EXISTS (SELECT 1 FROM `t` WHERE `t`.`id` > ?)
            SELECT EXISTS (SELECT 1 FROM `t` ORDER BY `t`.`name` ASC, `t`.`id` ASC LIMIT 2 OFFSET 1)
            redacted: SELECT EXISTS (SELECT 1 FROM `t` ORDER BY `t`.`name` ASC, `t`.`id` ASC LIMIT 2 OFFSET 1)
            |} )
    ; test "get" (fun () ->
          expect
            (String.concat "\n"
               [ show (Compile.get t (Int 7L))
               ; show (Compile.get coded (Text "a")) ] )
          @@ __POS_OF__
               {|
            SELECT `t`.`id`, `t`.`name`, `t`.`n`, `t`.`s`, `t`.`done` FROM `t` WHERE `t`.`id` = 7 LIMIT 1
            redacted: SELECT `t`.`id`, `t`.`name`, `t`.`n`, `t`.`s`, `t`.`done` FROM `t` WHERE `t`.`id` = ? LIMIT 1
            SELECT `co``ded`.`code`, `co``ded`.`v` FROM `co``ded` WHERE `co``ded`.`code` = _utf8mb4 X'61' LIMIT 1
            redacted: SELECT `co``ded`.`code`, `co``ded`.`v` FROM `co``ded` WHERE `co``ded`.`code` = ? LIMIT 1
            |} )
    ; test "insert" (fun () ->
          expect
            (String.concat "\n"
               [ show
                   (Compile.insert t
                      [ ("name", Text "a")
                      ; ("n", Null)
                      ; ("s", Text "b")
                      ; ("done", Float 1.5) ] )
               ; show (Compile.insert auto_only [])
               ; show (Compile.insert coded [("code", Text "c"); ("v", Int 1L)])
               ] )
          @@ __POS_OF__
               {|
            INSERT INTO `t` (`name`, `n`, `s`, `done`) VALUES (_utf8mb4 X'61', NULL, _utf8mb4 X'62', 1.5e0)
            redacted: INSERT INTO `t` (`name`, `n`, `s`, `done`) VALUES (?, ?, ?, ?)
            INSERT INTO `auto_only` () VALUES ()
            redacted: INSERT INTO `auto_only` () VALUES ()
            INSERT INTO `co``ded` (`code`, `v`) VALUES (_utf8mb4 X'63', 1)
            redacted: INSERT INTO `co``ded` (`code`, `v`) VALUES (?, ?)
            |} )
    ; test "the literals of each scalar" (fun () ->
          expect
            (show
               (Compile.insert t
                  [ ("i", Int (-9223372036854775808L))
                  ; ("f", Float (-0.0))
                  ; ("b", Int 1L)
                  ; ("t", Text "\xc3\xa9'")
                  ; ("y", Bytes "\x00'")
                  ; ( "d"
                    , Datetime
                        (Option.get (Ptime.of_float_s 1700000000.1234567)) )
                  ; ("n", Null) ] ) )
          @@ __POS_OF__
               {|
            INSERT INTO `t` (`i`, `f`, `b`, `t`, `y`, `d`, `n`) VALUES (-9223372036854775808, -0e0, 1, _utf8mb4 X'C3A927', X'0027', '2023-11-14 22:13:20.123456', NULL)
            redacted: INSERT INTO `t` (`i`, `f`, `b`, `t`, `y`, `d`, `n`) VALUES (?, ?, ?, ?, ?, ?, ?)
            |} )
    ; test "update and delete of one row" (fun () ->
          expect
            (String.concat "\n"
               [ show
                   (Compile.update t ~key:(Int 3L)
                      [("name", Text "a"); ("n", Null)] )
               ; show (Compile.delete t ~key:(Int 3L))
               ; show (Compile.delete coded ~key:(Text "k")) ] )
          @@ __POS_OF__
               {|
            UPDATE `t` SET `name` = _utf8mb4 X'61', `n` = NULL WHERE `t`.`id` = 3
            redacted: UPDATE `t` SET `name` = ?, `n` = ? WHERE `t`.`id` = ?
            DELETE FROM `t` WHERE `t`.`id` = 3
            redacted: DELETE FROM `t` WHERE `t`.`id` = ?
            DELETE FROM `co``ded` WHERE `co``ded`.`code` = _utf8mb4 X'6B'
            redacted: DELETE FROM `co``ded` WHERE `co``ded`.`code` = ?
            |} )
    ; test "the bulk forms" (fun () ->
          expect
            (String.concat "\n"
               [ show (Compile.update_all (q ()) [("name", Text "z")])
               ; show
                   (Compile.update_all
                      (q
                         ~where:[n ==. None]
                         ~order:[(col name, Desc)]
                         ~limit:2 () )
                      [("n", Int 1L); ("s", Null)] )
               ; show (Compile.delete_all (q ()))
               ; show
                   (Compile.delete_all
                      (q ~where:[name ==. "a"] ~order:[(col name, Asc)] ()) )
               ; show (Compile.delete_all (q ~limit:1 ())) ] )
          @@ __POS_OF__
               {|
            UPDATE `t` SET `name` = _utf8mb4 X'7A'
            redacted: UPDATE `t` SET `name` = ?
            UPDATE `t` SET `n` = 1, `s` = NULL WHERE `t`.`n` IS NULL ORDER BY `t`.`name` DESC, `t`.`id` ASC LIMIT 2
            redacted: UPDATE `t` SET `n` = ?, `s` = ? WHERE `t`.`n` IS NULL ORDER BY `t`.`name` DESC, `t`.`id` ASC LIMIT 2
            DELETE FROM `t`
            redacted: DELETE FROM `t`
            DELETE FROM `t` WHERE `t`.`name` = _utf8mb4 X'61' ORDER BY `t`.`name` ASC
            redacted: DELETE FROM `t` WHERE `t`.`name` = ? ORDER BY `t`.`name` ASC
            DELETE FROM `t` ORDER BY `t`.`id` ASC LIMIT 1
            redacted: DELETE FROM `t` ORDER BY `t`.`id` ASC LIMIT 1
            |} )
    ; test "the refusals of the bulk forms" (fun () ->
          let raised f =
            match f () with
            | _ ->
                "no exception"
            | exception Invalid_argument m ->
                m
          in
          expect
            (String.concat "\n"
               [ raised (fun () ->
                     Compile.update_all (q ~offset:1 ()) [("n", Null)] )
               ; raised (fun () ->
                     Compile.update_all (q ~lock:For_update ()) [("n", Null)] )
               ; raised (fun () ->
                     Compile.update_all (q ~lock:For_share ()) [("n", Null)] )
               ; raised (fun () -> Compile.update_all (q ()) [])
               ; raised (fun () -> Compile.delete_all (q ~offset:0 ()))
               ; raised (fun () -> Compile.delete_all (q ~lock:For_share ()))
               ; raised (fun () -> Compile.update t ~key:(Int 1L) [])
               ; raised (fun () ->
                     Compile.get {t with fields= [f_name]} (Int 1L) ) ] )
          @@ __POS_OF__
               {|
            Query.update: OFFSET is not permitted (MySQL has no OFFSET here)
            Query.update: FOR UPDATE and FOR SHARE are not permitted
            Query.update: FOR UPDATE and FOR SHARE are not permitted
            Query.update: no column to set
            Query.delete: OFFSET is not permitted (MySQL has no OFFSET here)
            Query.delete: FOR UPDATE and FOR SHARE are not permitted
            Compile.update: no column to set
            Compile: the model "t" has no key
            |} )
    ; test "the empty forms of a model AST from other code" (fun () ->
          let c = col id in
          let raw where = show (Compile.select {(q ()) with where}) in
          expect
            (String.concat "\n"
               [ raw [Model_ast.In (c, [])]
               ; raw [Model_ast.And []]
               ; raw [Model_ast.Or []]
               ; raw [Model_ast.Not (Model_ast.Or [Model_ast.In (c, [])])] ] )
          @@ __POS_OF__
               {|
            SELECT `t`.`id`, `t`.`name`, `t`.`n`, `t`.`s`, `t`.`done` FROM `t` WHERE FALSE
            redacted: SELECT `t`.`id`, `t`.`name`, `t`.`n`, `t`.`s`, `t`.`done` FROM `t` WHERE FALSE
            SELECT `t`.`id`, `t`.`name`, `t`.`n`, `t`.`s`, `t`.`done` FROM `t` WHERE TRUE
            redacted: SELECT `t`.`id`, `t`.`name`, `t`.`n`, `t`.`s`, `t`.`done` FROM `t` WHERE TRUE
            SELECT `t`.`id`, `t`.`name`, `t`.`n`, `t`.`s`, `t`.`done` FROM `t` WHERE FALSE
            redacted: SELECT `t`.`id`, `t`.`name`, `t`.`n`, `t`.`s`, `t`.`done` FROM `t` WHERE FALSE
            SELECT `t`.`id`, `t`.`name`, `t`.`n`, `t`.`s`, `t`.`done` FROM `t` WHERE NOT (FALSE)
            redacted: SELECT `t`.`id`, `t`.`name`, `t`.`n`, `t`.`s`, `t`.`done` FROM `t` WHERE NOT (FALSE)
            |} )
    ; test "a negative limit or offset" (fun () ->
          let raised f =
            match f () with
            | _ ->
                "no exception"
            | exception Invalid_argument m ->
                m
          in
          let lim = q ~limit:(-1) () and off = q ~offset:(-2) () in
          expect
            (String.concat "\n"
               [ raised (fun () -> Compile.select lim)
               ; raised (fun () -> Compile.select off)
               ; raised (fun () -> Compile.first lim)
               ; raised (fun () -> Compile.first off)
               ; raised (fun () -> Compile.count lim)
               ; raised (fun () -> Compile.exists off)
               ; raised (fun () -> Compile.update_all lim [("n", Null)])
               ; raised (fun () -> Compile.delete_all lim) ] )
          @@ __POS_OF__
               {|
            Query: the limit -1 is negative
            Query: the offset -2 is negative
            Query: the limit -1 is negative
            Query: the offset -2 is negative
            Query: the limit -1 is negative
            Query: the offset -2 is negative
            Query: the limit -1 is negative
            Query: the limit -1 is negative
            |} )
    ; test "a value with no literal" (fun () ->
          expect
            (String.concat "\n"
               [ where (x ==. Float.nan)
               ; where (in_ x [1.0; Float.infinity])
               ; show (Compile.insert t [("done", Float Float.nan)])
               ; show (Compile.update t ~key:(Int 1L) [("s", Text "\xff")])
               ; show (Compile.update_all (q ()) [("done", Float Float.nan)])
               ; show (Compile.get t (Float Float.nan))
               ; show (Compile.delete t ~key:(Float Float.nan))
               ; show (Compile.count (q ~where:[x <. Float.nan] ()))
               ; show (Compile.exists (q ~where:[x <. Float.nan] ()))
               ; show (Compile.delete_all (q ~where:[x <. Float.nan] ())) ] )
          @@ __POS_OF__
               {|
            Error: a NaN is not a SQL value
            Error: an infinity is not a SQL value
            Error: a NaN is not a SQL value
            Error: the text is not valid UTF-8
            Error: a NaN is not a SQL value
            Error: a NaN is not a SQL value
            Error: a NaN is not a SQL value
            Error: a NaN is not a SQL value
            Error: a NaN is not a SQL value
            Error: a NaN is not a SQL value
            |} )
    ]
