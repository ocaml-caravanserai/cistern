(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* The queries of the typed API (phase 2d2) on Fake_db, with the hand-written
   models: each finisher, the bulk forms and their refusals, for_update and
   for_share, and the same in a Tx. *)

open Windtrap
open Cistern_test_models
open Cistern_test_models_ppx
module D = Fake_db
module Db = Cistern.Db
module Tx = Cistern.Tx
module Query = Cistern.Query
module E = Cistern_core.Error
module User = User_todo.User
module Todo = User_todo.Todo

let with_pool = Test_pool.with_pool

let script pairs db = db.D.faults.script <- (fun sql -> List.assoc_opt sql pairs)

let text = function Ok _ -> "ok" | Error e -> E.to_string e

let cols =
  "`user`.`id`, `user`.`name`, `user`.`email`, `user`.`nick`, `user`.`age`, \
   `user`.`active`"

let row id name = [Some id; Some name; Some (name ^ "@x"); None; None; Some "1"]

let user id name =
  { User_todo.id
  ; name
  ; email= name ^ "@x"
  ; nick= None
  ; age= None
  ; active= true
  ; todos= Cistern.Deferred.Unloaded }

let older = User.where User.Q.(age >. Some 30)

let older_sql = "WHERE (`user`.`age` IS NOT NULL AND `user`.`age` > 30)"

let raised f =
  match f () with _ -> "no exception" | exception Invalid_argument m -> m

let tests =
  group "the queries"
    [ test "to_list with a filter, two orders, a limit and an offset" (fun () ->
          let sql =
            "SELECT " ^ cols ^ " FROM `user` " ^ older_sql
            ^ " ORDER BY `user`.`name` DESC, `user`.`email` ASC, `user`.`id` \
               ASC LIMIT 2 OFFSET 4"
          in
          let r, events =
            with_pool
              ~setup:(script [(sql, D.Rows [row "1" "b"; row "2" "a"])])
              (fun _ _ pool ->
                Db.exec pool
                  ( older
                  |> Query.order_by User.Q.name `Desc
                  |> Query.order_by User.Q.email `Asc
                  |> Query.limit 2 |> Query.offset 4 |> Query.to_list ) )
          in
          equal bool true (r = Ok [user 1L "b"; user 2L "a"]) ;
          equal (list string)
            [ "connect 10.0.0.9:3306 ok"
            ; Printf.sprintf "query %S done 2"
                ( "SELECT " ^ cols
                ^ " FROM `user` WHERE (`user`.`age` IS NOT NULL AND \
                   `user`.`age` > ?) ORDER BY `user`.`name` DESC, \
                   `user`.`email` ASC, `user`.`id` ASC LIMIT 2 OFFSET 4" ) ]
            events )
    ; test "to_list of all, and two filters" (fun () ->
          let all = "SELECT " ^ cols ^ " FROM `user`" in
          let two =
            all ^ " WHERE ("
            ^ "(`user`.`age` IS NOT NULL AND `user`.`age` > 30)"
            ^ " AND `user`.`active` = 1)"
          in
          let r, _ =
            with_pool
              ~setup:(script [(all, D.Rows []); (two, D.Rows [row "3" "c"])])
              (fun _ _ pool ->
                let a = Db.exec pool (Query.to_list User.all) in
                let b =
                  Db.exec pool
                    ( older
                    |> Query.where User.Q.(active ==. true)
                    |> Query.to_list )
                in
                (a, b) )
          in
          equal bool true (r = (Ok [], Ok [user 3L "c"])) )
    ; test "first: LIMIT 1 and the key in the order" (fun () ->
          let sql =
            "SELECT " ^ cols ^ " FROM `user` " ^ older_sql
            ^ " ORDER BY `user`.`id` ASC LIMIT 1"
          in
          let none =
            "SELECT " ^ cols ^ " FROM `user` ORDER BY `user`.`id` ASC LIMIT 1"
          in
          let r, _ =
            with_pool
              ~setup:(script [(sql, D.Rows [row "1" "b"]); (none, D.Rows [])])
              (fun _ _ pool ->
                let a = Db.exec pool (Query.first older) in
                let b = Db.exec pool (Query.first User.all) in
                (a, b) )
          in
          equal bool true (r = (Ok (Some (user 1L "b")), Ok None)) )
    ; test "count, with and with no limit, and a reply with no row" (fun () ->
          let plain = "SELECT COUNT(*) FROM `user` " ^ older_sql in
          let paged =
            "SELECT COUNT(*) FROM (SELECT 1 FROM `user` " ^ older_sql
            ^ " ORDER BY `user`.`id` ASC LIMIT 5) AS `user`"
          in
          let r, events =
            with_pool
              ~setup:
                (script
                   [ (plain, D.Rows [[Some "3"]])
                   ; (paged, D.Rows [[Some "2"]])
                   ; ("SELECT COUNT(*) FROM `user`", D.Rows []) ] )
              (fun _ _ pool ->
                let a = Db.exec pool (Query.count older) in
                let b = Db.exec pool (Query.count (Query.limit 5 older)) in
                let c = text (Db.exec pool (Query.count User.all)) in
                (a, b, c) )
          in
          let a, b, c = r in
          equal bool true (a = Ok 3) ;
          equal bool true (b = Ok 2) ;
          equal string "Decode: the reply has 0 rows, not 1" c ;
          equal (list string)
            [ "connect 10.0.0.9:3306 ok"
            ; "query \"SELECT COUNT(*) FROM `user` WHERE (`user`.`age` IS NOT \
               NULL AND `user`.`age` > ?)\" done 1"
            ; "query \"SELECT COUNT(*) FROM (SELECT 1 FROM `user` WHERE \
               (`user`.`age` IS NOT NULL AND `user`.`age` > ?) ORDER BY \
               `user`.`id` ASC LIMIT 5) AS `user`\" done 1"
            ; "query \"SELECT COUNT(*) FROM `user`\" failed Decode" ]
            events )
    ; test "exists: 1 and 0, and two rows" (fun () ->
          let sql = "SELECT EXISTS (SELECT 1 FROM `user` " ^ older_sql ^ ")" in
          let all = "SELECT EXISTS (SELECT 1 FROM `user`)" in
          let r, _ =
            with_pool
              ~setup:(fun db ->
                let n = ref 0 in
                db.D.faults.script <-
                  (fun s ->
                    if s = sql then (
                      incr n ;
                      Some (D.Rows [[Some (if !n = 1 then "1" else "0")]]) )
                    else if s = all then Some (D.Rows [[Some "1"]; [Some "1"]])
                    else None ) )
              (fun _ _ pool ->
                let a = Db.exec pool (Query.exists older) in
                let b = Db.exec pool (Query.exists older) in
                let c = text (Db.exec pool (Query.exists User.all)) in
                (a, b, c) )
          in
          equal bool true
            (r = (Ok true, Ok false, "Decode: the reply has 2 rows, not 1")) )
    ; test "filter_by of an index and of the key of a Belongs_to" (fun () ->
          let tcols =
            "SELECT `todo`.`id`, `todo`.`user_id`, `todo`.`title`, \
             `todo`.`done`, `todo`.`note` FROM `todo`"
          in
          let r, _ =
            with_pool
              ~setup:
                (script
                   [ ( tcols ^ " WHERE `todo`.`title` = _utf8mb4 X'4D696C6B'"
                     , D.Rows [[Some "1"; Some "7"; Some "Milk"; Some "0"; None]]
                     )
                   ; ( tcols ^ " WHERE `todo`.`user_id` = 7"
                     , D.Rows
                         [[Some "2"; Some "7"; Some "Eggs"; Some "1"; Some "n"]]
                     ) ] )
              (fun _ _ pool ->
                let a =
                  Db.exec pool (Query.to_list (Todo.filter_by_title "Milk"))
                in
                let b =
                  Db.exec pool (Query.to_list (Todo.filter_by_user_id 7L))
                in
                (a, b) )
          in
          equal bool true
            ( r
            = ( Ok
                  [ { User_todo.id= 1L
                    ; user_id= 7L
                    ; user= Cistern.Deferred.Unloaded
                    ; title= "Milk"
                    ; done_= false
                    ; note= None } ]
              , Ok
                  [ { User_todo.id= 2L
                    ; user_id= 7L
                    ; user= Cistern.Deferred.Unloaded
                    ; title= "Eggs"
                    ; done_= true
                    ; note= Some "n" } ] ) ) )
    ; test "the bulk forms: the matched rows" (fun () ->
          let up =
            "UPDATE `user` SET `active` = 0 " ^ older_sql
            ^ " ORDER BY `user`.`name` ASC, `user`.`id` ASC LIMIT 10"
          in
          let del = "DELETE FROM `user` " ^ older_sql in
          let r, events =
            with_pool
              ~setup:
                (script
                   [ (up, D.Counts {affected= 3L; insert= 0L})
                   ; (del, D.Counts {affected= 2L; insert= 0L}) ] )
              (fun _ _ pool ->
                let a =
                  Db.exec pool
                    (Query.update
                       (Cistern.Update.set User.Q.active false)
                       ( older
                       |> Query.order_by User.Q.name `Asc
                       |> Query.limit 10 ) )
                in
                let b = Db.exec pool (Query.delete older) in
                (a, b) )
          in
          equal bool true (r = (Ok 3, Ok 2)) ;
          equal (list string)
            [ "connect 10.0.0.9:3306 ok"
            ; "query \"UPDATE `user` SET `active` = ? WHERE (`user`.`age` IS \
               NOT NULL AND `user`.`age` > ?) ORDER BY `user`.`name` ASC, \
               `user`.`id` ASC LIMIT 10\" done 3"
            ; "query \"DELETE FROM `user` WHERE (`user`.`age` IS NOT NULL AND \
               `user`.`age` > ?)\" done 2" ]
            events )
    ; test "the refusals when the statement is made" (fun () ->
          let set = Cistern.Update.set User.Q.active false in
          equal (list string)
            [ "Query.update: OFFSET is not permitted (MySQL has no OFFSET here)"
            ; "Query.update: FOR UPDATE and FOR SHARE are not permitted"
            ; "Query.delete: FOR UPDATE and FOR SHARE are not permitted"
            ; "Query.delete: OFFSET is not permitted (MySQL has no OFFSET here)"
            ; "Update: the key \"user\".\"id\" cannot change"
            ; "Query.limit: -1 is negative"
            ; "Query.offset: -2 is negative" ]
            [ raised (fun () -> Query.update set (Query.offset 1 User.all))
            ; raised (fun () -> Query.update set (Query.for_update User.all))
            ; raised (fun () -> Query.delete (Query.for_share User.all))
            ; raised (fun () -> Query.delete (Query.offset 0 User.all))
            ; raised (fun () ->
                  Query.update Cistern.Update.(set User.Q.id 1L) User.all )
            ; raised (fun () -> Query.limit (-1) User.all)
            ; raised (fun () -> Query.offset (-2) User.all) ] )
    ; test "for_update by Db.exec in a Tx fiber: the message of the Tx"
        (fun () ->
          let r, events =
            with_pool (fun _ _ pool ->
                let inner = ref "" in
                let _ =
                  Db.transaction pool (fun _ ->
                      inner :=
                        raised (fun () ->
                            Db.exec pool
                              (Query.to_list (Query.for_update User.all)) ) )
                in
                !inner )
          in
          equal string "Db.exec in the fiber of a Tx" r ;
          (* The events of the Tx only: no event of the refused statement. *)
          equal (list string) []
            (List.filter
               (fun e -> String.starts_with ~prefix:"query \"SELECT" e)
               events ) )
    ; test "for_update by Db.exec after with_db: the message of the use after"
        (fun () ->
          let pool, events = with_pool (fun _ _ pool -> pool) in
          equal string "Db.t used after with_db"
            (raised (fun () ->
                 Db.exec pool (Query.to_list (Query.for_update User.all)) ) ) ;
          equal (list string) [] events )
    ; test "for_update in Db.exec: Invalid_argument, no event, also after map"
        (fun () ->
          let r, events =
            with_pool (fun _ db pool ->
                let q = Query.for_update User.all in
                let a = raised (fun () -> Db.exec pool (Query.to_list q)) in
                let b =
                  raised (fun () ->
                      Db.exec pool
                        (Cistern.Stmt.map List.length (Query.to_list q)) )
                in
                let c = raised (fun () -> Db.exec pool (Query.first q)) in
                (a, b, c, db.D.connects) )
          in
          let msg = "Db.exec: a query with for_update runs only in a Tx" in
          equal bool true (r = (msg, msg, msg, 0)) ;
          equal (list string) [] events )
    ; test "count and exists ignore for_update in Db.exec" (fun () ->
          let r, _ =
            with_pool
              ~setup:
                (script
                   [ ("SELECT COUNT(*) FROM `user`", D.Rows [[Some "4"]])
                   ; ( "SELECT EXISTS (SELECT 1 FROM `user`)"
                     , D.Rows [[Some "1"]] ) ] )
              (fun _ _ pool ->
                let q = Query.for_update User.all in
                let a = Db.exec pool (Query.count q) in
                let b = Db.exec pool (Query.exists q) in
                (a, b) )
          in
          equal bool true (r = (Ok 4, Ok true)) )
    ; test "for_share in Db.exec" (fun () ->
          let sql = "SELECT " ^ cols ^ " FROM `user` FOR SHARE" in
          let r, _ =
            with_pool
              ~setup:(script [(sql, D.Rows [row "1" "a"])])
              (fun _ _ pool ->
                Db.exec pool (Query.to_list (Query.for_share User.all)) )
          in
          equal bool true (r = Ok [user 1L "a"]) )
    ; test
        "a lost reply: count gives Connection_lost, a bulk form Outcome_unknown"
        (fun () ->
          let r, _ =
            with_pool
              ~setup:
                (script
                   [ ("SELECT COUNT(*) FROM `user`", D.No_reply)
                   ; ("DELETE FROM `user`", D.No_reply) ] )
              (fun _ _ pool ->
                let a = text (Db.exec pool (Query.count User.all)) in
                let b = text (Db.exec pool (Query.delete User.all)) in
                (a, b) )
          in
          equal bool true (r = ("Connection_lost", "Outcome_unknown statement")) )
    ; test "in a Tx: for_update, first, count and the bulk forms" (fun () ->
          let lock =
            "SELECT " ^ cols ^ " FROM `user` " ^ older_sql ^ " FOR UPDATE"
          in
          let first_lock =
            "SELECT " ^ cols ^ " FROM `user` " ^ older_sql
            ^ " ORDER BY `user`.`id` ASC LIMIT 1 FOR UPDATE"
          in
          let r, _ =
            with_pool
              ~setup:
                (script
                   [ (lock, D.Rows [row "1" "a"])
                   ; (first_lock, D.Rows [row "1" "a"])
                   ; ( "SELECT COUNT(*) FROM `user` " ^ older_sql
                     , D.Rows [[Some "1"]] )
                   ; ( "UPDATE `user` SET `active` = 0 " ^ older_sql
                     , D.Counts {affected= 1L; insert= 0L} )
                   ; ( "DELETE FROM `user` " ^ older_sql
                     , D.Counts {affected= 1L; insert= 0L} ) ] )
              (fun _ db pool ->
                let r =
                  Db.transaction pool (fun tx ->
                      let q = Query.for_update older in
                      let a = Tx.exec tx (Query.to_list q) in
                      let b =
                        Tx.exec tx
                          (Cistern.Stmt.map Option.is_some (Query.first q))
                      in
                      let c = Tx.exec tx (Query.count q) in
                      let d =
                        Tx.exec tx
                          (Query.update
                             (Cistern.Update.set User.Q.active false)
                             older )
                      in
                      let e = Tx.exec tx (Query.delete older) in
                      (a, b, c, d, e) )
                in
                (r, List.mem "COMMIT" db.D.log) )
          in
          equal bool true
            (r = (Ok (Ok [user 1L "a"], Ok true, Ok 1, Ok 1, Ok 1), true)) ) ]

(* The nested create of the hand-written user and todo (phase 4c1, the plan of
   phase 4, E10) on Fake_db: the parent gets the id 41, each todo the next id
   from 100. [fail] gives an ERR 1062 for the todo with this title. *)
let nested_tests =
  let setup ?fail db =
    let next = ref 99 in
    db.D.faults.script <-
      (fun sql ->
        if String.starts_with ~prefix:"INSERT INTO `user`" sql then
          Some (D.Counts {affected= 1L; insert= 41L})
        else if String.starts_with ~prefix:"INSERT INTO `todo`" sql then (
          match fail with
          | Some title when Test_atomic.contains sql title ->
              Some (D.Err 1062)
          | _ ->
              incr next ;
              Some (D.Counts {affected= 1L; insert= Int64.of_int !next}) )
        else None )
  in
  let milk = Todo.draft ~title:"Milk" ()
  and eggs = Todo.draft ~title:"Eggs" () in
  let ann = User.create ~name:"Ann" ~email:"a@x" ~todos:[milk; eggs] () in
  (* The commands of the fake server after the connect, and the query events. *)
  let commands (db : D.t) =
    List.rev db.log
    |> List.filter (fun s -> s <> "" && s <> "CLEAN")
    |> List.map (fun s ->
        if String.starts_with ~prefix:"INSERT INTO `todo`" s then
          (* The key of the parent, and the title. *)
          if Test_atomic.contains s "(41," then
            if Test_atomic.contains s "X'4D696C6B'" then "todo 41 Milk"
            else "todo 41 Eggs"
          else "todo ?"
        else if String.starts_with ~prefix:"INSERT INTO `user`" s then "user"
        else s )
  in
  let child_ids (u : User_todo.user) =
    match u.todos with
    | Cistern.Deferred.Loaded l ->
        List.map
          (fun (t : User_todo.todo) ->
            Printf.sprintf "%Ld %Ld %s %s" t.id t.user_id t.title
              ( match t.user with
              | Cistern.Deferred.Unloaded ->
                  "Unloaded"
              | Loaded _ ->
                  "Loaded" ) )
          l
    | Unloaded ->
        ["Unloaded"]
  in
  group "the nested create"
    [ test "in Db.exec: the transaction, the ids, the events" (fun () ->
          let (r, cmds), events =
            with_pool ~size:1
              ~setup:(fun db -> setup db)
              (fun _ db pool ->
                let r = Db.exec pool ann in
                (r, commands db) )
          in
          ( match r with
          | Ok u ->
              equal int64 41L u.id ;
              equal (list string)
                ["100 41 Milk Unloaded"; "101 41 Eggs Unloaded"]
                (child_ids u)
          | Error e ->
              fail (E.to_string e) ) ;
          equal (list string)
            [ "START TRANSACTION"
            ; "user"
            ; "todo 41 Milk"
            ; "todo 41 Eggs"
            ; "COMMIT" ]
            cmds ;
          let child =
            "query \"INSERT INTO `todo` (`user_id`, `title`, `done`, `note`) \
             VALUES (?, ?, ?, ?)\" done 1"
          in
          equal (list string)
            [ "query \"START TRANSACTION\" done 0"
            ; "query \"INSERT INTO `user` (`name`, `email`, `nick`, `age`, \
               `active`) VALUES (?, ?, ?, ?, ?)\" done 1"
            ; child
            ; child
            ; "query \"COMMIT\" done 0" ]
            (Test_pool.queries events) )
    ; test "in a Tx: a savepoint" (fun () ->
          let (r, cmds), _ =
            with_pool ~size:1
              ~setup:(fun db -> setup db)
              (fun _ db pool ->
                let r = Db.transaction pool (fun tx -> Tx.exec_exn tx ann) in
                (r, commands db) )
          in
          ( match r with
          | Ok u ->
              equal (list string)
                ["100 41 Milk Unloaded"; "101 41 Eggs Unloaded"]
                (child_ids u)
          | Error e ->
              fail (E.to_string e) ) ;
          equal (list string)
            [ "START TRANSACTION"
            ; "SAVEPOINT cistern_sp_1"
            ; "user"
            ; "todo 41 Milk"
            ; "todo 41 Eggs"
            ; "RELEASE SAVEPOINT cistern_sp_1"
            ; "COMMIT" ]
            cmds )
    ; test "an empty list of todos gives Loaded [], no list gives Unloaded"
        (fun () ->
          let (a, b), _ =
            with_pool ~size:1
              ~setup:(fun db -> setup db)
              (fun _ _ pool ->
                let a =
                  Db.exec pool (User.create ~name:"A" ~email:"a@x" ~todos:[] ())
                in
                let b = Db.exec pool (User.create ~name:"B" ~email:"b@x" ()) in
                (a, b) )
          in
          match (a, b) with
          | Ok a, Ok b ->
              is_true (a.todos = Cistern.Deferred.Loaded []) ;
              is_true (b.todos = Cistern.Deferred.Unloaded)
          | _ ->
              fail "the creates" )
    ; test "two has_many: the commands and the records of each group" (fun () ->
          (* The ids: 11 for a kid, 21 for rex, 22 for tom. *)
          let coded db =
            let id n = Some (D.Counts {affected= 1L; insert= n}) in
            db.D.faults.script <-
              (fun sql ->
                if String.starts_with ~prefix:"INSERT INTO `c`" sql then id 11L
                else if String.starts_with ~prefix:"INSERT INTO `d`" sql then
                  if Test_atomic.contains sql "X'726578'" then id 21L
                  else id 22L
                else None )
          in
          let shape (p : Coded.p) =
            let ids = function
              | Cistern.Deferred.Loaded l ->
                  String.concat "," l
              | Unloaded ->
                  "Unloaded"
            in
            Printf.sprintf "kids %s pets %s"
              (ids
                 Cistern.Deferred.(
                   match p.kids with
                   | Loaded l ->
                       Loaded
                         (List.map
                            (fun (c : Coded.c) -> Printf.sprintf "%Ld" c.id)
                            l )
                   | Unloaded ->
                       Unloaded ) )
              (ids
                 Cistern.Deferred.(
                   match p.pets with
                   | Loaded l ->
                       Loaded
                         (List.map
                            (fun (d : Coded.d) ->
                              Printf.sprintf "%Ld %s %s" d.did d.owner d.name )
                            l )
                   | Unloaded ->
                       Unloaded ) )
          in
          let r, _ =
            with_pool ~size:1 ~setup:coded (fun _ db pool ->
                let run s =
                  let from = List.length db.D.log in
                  let r =
                    match Db.exec pool s with
                    | Ok p ->
                        shape p
                    | Error e ->
                        E.to_string e
                  in
                  let cmds =
                    List.rev db.D.log
                    |> List.filteri (fun i _ -> i >= from)
                    |> List.map (fun s ->
                        match String.index_opt s '(' with
                        | Some i ->
                            String.sub s 0 (i - 1)
                        | None ->
                            s )
                  in
                  r :: cmds
                in
                [ run (Coded.create ~pets:["rex"; "tom"] [2.])
                ; run (Coded.create ~pets:["rex"] [])
                ; run (Coded.create ~pets:[] []) ] )
          in
          equal
            (list (list string))
            [ [ "kids 11 pets 21 A rex,22 A tom"
              ; "START TRANSACTION"
              ; "INSERT INTO `p`"
              ; "INSERT INTO `c`"
              ; "INSERT INTO `d`"
              ; "INSERT INTO `d`"
              ; "COMMIT" ]
            ; [ "kids  pets 21 A rex"
              ; "START TRANSACTION"
              ; "INSERT INTO `p`"
              ; "INSERT INTO `d`"
              ; "COMMIT" ]
            ; ["kids  pets "; "INSERT INTO `p`"] ]
            r )
    ; test "a lost reply of a child in a Tx: Outcome_unknown" (fun () ->
          let inner = ref "" in
          let r, _ =
            with_pool ~size:1
              ~setup:(fun db ->
                setup db ;
                let s = db.D.faults.script in
                db.D.faults.script <-
                  (fun sql ->
                    if Test_atomic.contains sql "X'45676773'" then
                      Some D.No_reply
                    else s sql ) )
              (fun _ _ pool ->
                Db.transaction pool (fun tx -> inner := text (Tx.exec tx ann)) )
          in
          (* A child INSERT is a write: its lost reply is not known. The Tx ends
             with the final error of a lost reply of DML. *)
          equal string "Outcome_unknown statement" !inner ;
          equal bool true (r = Error E.Connection_lost) )
    ; test "an error of the second child: ROLLBACK, the error" (fun () ->
          let (r, cmds), _ =
            with_pool ~size:1
              ~setup:(fun db -> setup ~fail:"X'45676773'" db)
              (fun _ db pool ->
                let r = text (Db.exec pool ann) in
                (r, commands db) )
          in
          equal string "Constraint_violation Unique: scripted" r ;
          equal (list string)
            [ "START TRANSACTION"
            ; "user"
            ; "todo 41 Milk"
            ; "todo 41 Eggs"
            ; "ROLLBACK" ]
            cmds )
    ; test "an error of the second child in a Tx: ROLLBACK TO, the Tx goes on"
        (fun () ->
          let (r, cmds), _ =
            with_pool ~size:1
              ~setup:(fun db -> setup ~fail:"X'45676773'" db)
              (fun _ db pool ->
                let r =
                  Db.transaction pool (fun tx ->
                      let a = text (Tx.exec tx ann) in
                      let b =
                        text
                          (Tx.exec tx (User.create ~name:"Bo" ~email:"b@x" ()))
                      in
                      a ^ " / " ^ b )
                in
                (r, commands db) )
          in
          equal bool true (r = Ok "Constraint_violation Unique: scripted / ok") ;
          equal (list string)
            [ "START TRANSACTION"
            ; "SAVEPOINT cistern_sp_1"
            ; "user"
            ; "todo 41 Milk"
            ; "todo 41 Eggs"
            ; "ROLLBACK TO SAVEPOINT cistern_sp_1"
            ; "user"
            ; "COMMIT" ]
            cmds ) ]

(* The relation scopes on Fake_db (phase 4c2, E11): the scope of a nullable
   belongs_to with no key runs nothing, and a row that is gone gives
   Not_found. *)
let scope_tests =
  let module Node = Tree.Node in
  let ncols =
    "SELECT `node`.`id`, `node`.`name`, `node`.`parent_id` FROM `node`"
  in
  let node ?parent_id id : Tree.node =
    { id
    ; name= "n"
    ; parent_id
    ; parent= Cistern.Deferred.Unloaded
    ; children= Cistern.Deferred.Unloaded }
  in
  let get3 = ncols ^ " WHERE `node`.`id` = 3 LIMIT 1" in
  group "the relation scopes"
    [ test "a nullable key with None: None, no checkout, no event" (fun () ->
          let (r, connects), events =
            with_pool (fun _ db pool ->
                let r = Db.exec pool (Node.parent (node 4L)) in
                (r, db.D.connects) )
          in
          is_true (r = Ok None) ;
          equal int 0 connects ;
          equal (list string) [] events )
    ; test "a nullable key with Some k: Some of the row" (fun () ->
          let r, _ =
            with_pool
              ~setup:(script [(get3, D.Rows [[Some "3"; Some "n"; None]])])
              (fun _ _ pool ->
                Db.exec pool (Node.parent (node ~parent_id:3L 4L)) )
          in
          is_true (r = Ok (Some (node 3L))) )
    ; test "Node.create ?children: the children of a self relation" (fun () ->
          (* The ids: 40 for the root r, 41 for a, 42 for b. *)
          let setup db =
            let id n = Some (D.Counts {affected= 1L; insert= n}) in
            db.D.faults.script <-
              (fun sql ->
                if not (String.starts_with ~prefix:"INSERT INTO `node`" sql)
                then None
                else if Test_atomic.contains sql "X'72'" then id 40L
                else if Test_atomic.contains sql "X'61'" then id 41L
                else id 42L )
          in
          let r, _ =
            with_pool ~size:1 ~setup (fun _ _ pool ->
                Db.exec pool
                  (Node.create ~name:"r"
                     ~children:[Node.draft ~name:"a" (); Node.draft ~name:"b" ()]
                     () ) )
          in
          let child id name : Tree.node =
            { id
            ; name
            ; parent_id= Some 40L
            ; parent= Cistern.Deferred.Unloaded
            ; children= Cistern.Deferred.Unloaded }
          in
          is_true
            ( r
            = Ok
                { Tree.id= 40L
                ; name= "r"
                ; parent_id= None
                ; parent= Cistern.Deferred.Unloaded
                ; children=
                    Cistern.Deferred.Loaded [child 41L "a"; child 42L "b"] } ) )
    ; test "a row that is gone: Not_found" (fun () ->
          let ucols =
            "SELECT `user`.`id`, `user`.`name`, `user`.`email`, `user`.`nick`, \
             `user`.`age`, `user`.`active` FROM `user` WHERE `user`.`id` = 7 \
             LIMIT 1"
          in
          let r, _ =
            with_pool
              ~setup:(script [(get3, D.Rows []); (ucols, D.Rows [])])
              (fun _ _ pool ->
                let a =
                  text (Db.exec pool (Node.parent (node ~parent_id:3L 4L)))
                in
                let b =
                  text
                    (Db.exec pool
                       (Todo.user
                          { User_todo.id= 1L
                          ; user_id= 7L
                          ; user= Cistern.Deferred.Unloaded
                          ; title= "t"
                          ; done_= false
                          ; note= None } ) )
                in
                (a, b) )
          in
          equal (pair string string) ("Not_found", "Not_found") r ) ]

(* include_ on Fake_db (phase 4d, E8): the query of each relation after the main
   query, the grouping, the chunks and the events. *)
let include_tests =
  let module Node = Tree.Node in
  let ucols = "SELECT " ^ cols ^ " FROM `user`" in
  let tcols =
    "SELECT `todo`.`id`, `todo`.`user_id`, `todo`.`title`, `todo`.`done`, \
     `todo`.`note` FROM `todo`"
  in
  let ncols =
    "SELECT `node`.`id`, `node`.`name`, `node`.`parent_id` FROM `node`"
  in
  let todo_row id user =
    [Some id; Some user; Some ("t" ^ id); Some "0"; None]
  in
  (* [answer l]: the script of the SQL texts that start with a prefix of [l]. *)
  let answer l db =
    db.D.faults.script <-
      (fun sql ->
        List.find_map
          (fun (p, r) ->
            if String.starts_with ~prefix:p sql then Some r else None )
          l )
  in
  let child_sql = tcols ^ " WHERE `todo`.`user_id` IN (" in
  (* The SQL commands of the fake server with a prefix. *)
  let sent (db : D.t) p =
    List.filter (String.starts_with ~prefix:p) (List.rev db.log)
  in
  let ids = function
    | Cistern.Deferred.Loaded l ->
        String.concat ","
          (List.map (fun (t : User_todo.todo) -> Int64.to_string t.id) l)
    | Unloaded ->
        "Unloaded"
  in
  group "include_"
    [ test "a has_many: the children of each key, a parent with no child"
        (fun () ->
          let (r, child), events =
            with_pool
              ~setup:
                (answer
                   [ ( child_sql
                     , D.Rows
                         [ todo_row "10" "2"
                         ; todo_row "11" "1"
                         ; todo_row "12" "2" ] )
                   ; (ucols, D.Rows [row "1" "a"; row "2" "b"; row "3" "c"]) ] )
              (fun _ db pool ->
                let r =
                  Db.exec pool
                    (User.all |> Query.include_ User.Rel.todos |> Query.to_list)
                in
                (r, sent db child_sql) )
          in
          ( match r with
          | Ok us ->
              equal (list string) ["1 11"; "2 10,12"; "3 "]
                (List.map
                   (fun (u : User_todo.user) ->
                     Printf.sprintf "%Ld %s" u.id (ids u.todos) )
                   us ) ;
              (* No inverse relation: each child stays Unloaded. *)
              List.iter
                (fun (u : User_todo.user) ->
                  match u.todos with
                  | Loaded l ->
                      List.iter
                        (fun (t : User_todo.todo) ->
                          is_true (t.user = Cistern.Deferred.Unloaded) )
                        l
                  | Unloaded ->
                      fail "Unloaded" )
                us
          | Error e ->
              fail (E.to_string e) ) ;
          equal (list string)
            [child_sql ^ "1, 2, 3) ORDER BY `todo`.`id` ASC"]
            child ;
          equal (list string)
            [ "connect 10.0.0.9:3306 ok"
            ; Printf.sprintf "query %S done 3" ucols
            ; Printf.sprintf "query %S done 3"
                (child_sql ^ "?, ?, ?) ORDER BY `todo`.`id` ASC") ]
            events )
    ; test "a belongs_to: unique keys, a parent that is gone stays Unloaded"
        (fun () ->
          let usql = ucols ^ " WHERE `user`.`id` IN (" in
          let (r, parent), _ =
            with_pool
              ~setup:
                (answer
                   [ (usql, D.Rows [row "1" "a"; row "2" "b"])
                   ; ( tcols
                     , D.Rows
                         [ todo_row "10" "2"
                         ; todo_row "11" "1"
                         ; todo_row "12" "2"
                         ; todo_row "13" "9" ] ) ] )
              (fun _ db pool ->
                let r =
                  Db.exec pool
                    (Todo.all |> Query.include_ Todo.Rel.user |> Query.to_list)
                in
                (r, sent db usql) )
          in
          equal (list string) [usql ^ "2, 1, 9)"] parent ;
          match r with
          | Ok ts ->
              equal (list string)
                ["10 b"; "11 a"; "12 b"; "13 Unloaded"]
                (List.map
                   (fun (t : User_todo.todo) ->
                     Printf.sprintf "%Ld %s" t.id
                       ( match t.user with
                       | Loaded u ->
                           u.name
                       | Unloaded ->
                           "Unloaded" ) )
                   ts )
          | Error e ->
              fail (E.to_string e) )
    ; test "first with include_" (fun () ->
          let r, _ =
            with_pool
              ~setup:
                (answer
                   [ (child_sql, D.Rows [todo_row "11" "1"])
                   ; (ucols, D.Rows [row "1" "a"]) ] )
              (fun _ _ pool ->
                Db.exec pool
                  (User.all |> Query.include_ User.Rel.todos |> Query.first) )
          in
          match r with
          | Ok (Some u) ->
              equal string "11" (ids u.todos)
          | _ ->
              fail "first" )
    ; test "a nullable belongs_to: None gives Loaded None; two include_"
        (fun () ->
          let nrow id parent = [Some id; Some "n"; parent] in
          let node id parent_id : Tree.node =
            { id
            ; name= "n"
            ; parent_id
            ; parent= Cistern.Deferred.Unloaded
            ; children= Cistern.Deferred.Unloaded }
          in
          let (r, cmds), events =
            with_pool
              ~setup:
                (answer
                   [ (ncols ^ " WHERE `node`.`id` IN (", D.Rows [nrow "1" None])
                   ; ( ncols ^ " WHERE `node`.`parent_id` IN ("
                     , D.Rows [nrow "2" (Some "1"); nrow "3" (Some "1")] )
                   ; ( ncols
                     , D.Rows
                         [ nrow "1" None
                         ; nrow "2" (Some "1")
                         ; nrow "3" (Some "1")
                         ; nrow "4" (Some "9") ] ) ] )
              (fun _ db pool ->
                let r =
                  Db.exec pool
                    ( Node.all
                    |> Query.include_ Node.Rel.parent
                    |> Query.include_ Node.Rel.children
                    |> Query.to_list )
                in
                (r, sent db ncols) )
          in
          equal (list string)
            [ ncols
            ; ncols ^ " WHERE `node`.`id` IN (1, 9)"
            ; ncols
              ^ " WHERE `node`.`parent_id` IN (1, 2, 3, 4) ORDER BY \
                 `node`.`id` ASC" ]
            cmds ;
          equal int 4 (List.length events) ;
          let n1 = node 1L None in
          let child id = node id (Some 1L) in
          is_true
            ( r
            = Ok
                [ { n1 with
                    parent= Loaded None
                  ; children= Loaded [child 2L; child 3L] }
                ; {(child 2L) with parent= Loaded (Some n1); children= Loaded []}
                ; {(child 3L) with parent= Loaded (Some n1); children= Loaded []}
                  (* Its parent 9 is gone: the relation stays Unloaded (the
                     notes of the review of 4a). *)
                ; {(node 4L (Some 9L)) with children= Loaded []} ] ) )
    ; cases "the chunks of 1000 keys"
        ~id:(fun (n, _) -> string_of_int n)
        [(1000, [1000]); (1001, [1000; 1]); (2500, [1000; 1000; 500])]
        (fun (n, sizes) ->
          let rows = List.init n (fun i -> row (string_of_int (i + 1)) "u") in
          let (r, child), events =
            with_pool
              ~setup:(answer [(child_sql, D.Rows []); (ucols, D.Rows rows)])
              (fun _ db pool ->
                let r =
                  Db.exec pool
                    (User.all |> Query.include_ User.Rel.todos |> Query.to_list)
                in
                (r, sent db child_sql) )
          in
          ( match r with
          | Ok us ->
              equal int n (List.length us) ;
              is_true
                (List.for_all
                   (fun (u : User_todo.user) ->
                     u.todos = Cistern.Deferred.Loaded [] )
                   us )
          | Error e ->
              fail (E.to_string e) ) ;
          equal int (List.length sizes) (List.length child) ;
          (* Each chunk event: one ? for each key. *)
          let marks e = List.length (String.split_on_char '?' e) - 1 in
          equal (list int) sizes
            (List.filter_map
               (fun e ->
                 if Test_atomic.contains e "IN (" then Some (marks e) else None )
               events ) )
    ; test "no key: no query of include_" (fun () ->
          let (r, child), events =
            with_pool
              ~setup:(answer [(ucols, D.Rows [])])
              (fun _ db pool ->
                let r =
                  Db.exec pool
                    (User.all |> Query.include_ User.Rel.todos |> Query.to_list)
                in
                (r, sent db child_sql) )
          in
          is_true (r = Ok []) ;
          equal (list string) [] child ;
          equal int 2 (List.length events) )
    ; test
        "a lost reply of a chunk: Connection_lost, the events of each command"
        (fun () ->
          let r, events =
            with_pool
              ~setup:
                (answer
                   [(child_sql, D.No_reply); (ucols, D.Rows [row "1" "a"])] )
              (fun _ _ pool ->
                text
                  (Db.exec pool
                     (User.all |> Query.include_ User.Rel.todos |> Query.to_list) ) )
          in
          equal string "Connection_lost" r ;
          (* No joined event of the statement. *)
          equal (list string)
            [ Printf.sprintf "query %S done 1" ucols
            ; Printf.sprintf "query %S failed Connection_lost"
                (child_sql ^ "?) ORDER BY `todo`.`id` ASC") ]
            (Test_pool.queries events) )
    ; test "a lost reply of the main query: Connection_lost (a read)" (fun () ->
          let r, events =
            with_pool
              ~setup:(answer [(ucols, D.No_reply)])
              (fun _ _ pool ->
                text
                  (Db.exec pool
                     (User.all |> Query.include_ User.Rel.todos |> Query.to_list) ) )
          in
          equal string "Connection_lost" r ;
          equal (list string)
            [Printf.sprintf "query %S failed Connection_lost" ucols]
            (Test_pool.queries events) )
    ; test "a cancel in a chunk: its Cancelled event, the connection goes"
        (fun () ->
          let (r, connects), events =
            with_pool
              ~setup:
                (answer
                   [ (child_sql, D.Delay (200, D.Rows []))
                   ; ("SELECT COUNT(*) FROM `user`", D.Rows [[Some "1"]])
                   ; (ucols, D.Rows [row "1" "a"]) ] )
              (fun _ db pool ->
                let r =
                  match
                    Weft.Fiber.with_timeout Weft.Clock.Mono (Weft.Span.ms 50)
                      (fun () ->
                        Db.exec pool
                          ( User.all
                          |> Query.include_ User.Rel.todos
                          |> Query.to_list ) )
                  with
                  | r ->
                      text r
                  | exception Weft.Fiber.Timeout ->
                      "cancelled"
                in
                ignore (Db.exec pool (Query.count User.all)) ;
                (r, db.D.connects) )
          in
          equal string "cancelled" r ;
          equal int 2 connects ;
          equal (list string)
            [ Printf.sprintf "query %S done 1" ucols
            ; Printf.sprintf "query %S cancelled sent=true"
                (child_sql ^ "?) ORDER BY `todo`.`id` ASC")
              (* No joined event: the next event is the count. *)
            ; "query \"SELECT COUNT(*) FROM `user`\" done 1" ]
            (Test_pool.queries events) )
    ; test "the margin before a chunk in Db.exec: Timeout, no chunk" (fun () ->
          let (r, child), events =
            with_pool
              ~setup:
                (answer
                   [ (child_sql, D.Rows [])
                   ; (ucols, D.Delay (80, D.Rows [row "1" "a"])) ] )
              (fun _ db pool ->
                let r =
                  text
                    (Db.exec ~timeout:(Weft.Span.ms 120) pool
                       ( User.all
                       |> Query.include_ User.Rel.todos
                       |> Query.to_list ) )
                in
                (r, sent db child_sql) )
          in
          equal string "Timeout" r ;
          equal (list string) [] child ;
          equal (list string)
            [ Printf.sprintf "query %S done 1" ucols
            ; Printf.sprintf "query %S failed Timeout"
                (child_sql ^ "?) ORDER BY `todo`.`id` ASC") ]
            (Test_pool.queries events) )
    ; cases "an error of a chunk in a Tx"
        ~id:(fun (c, _, _) -> string_of_int c)
        [ (1213, "Retryable deadlock", "Retryable deadlock")
        ; (1406, "Invalid_value 1406: scripted", "ok") ]
        (fun (code, inner, result) ->
          let steps = ref [] in
          let r, _ =
            with_pool
              ~setup:
                (answer
                   [ (child_sql, D.Err code)
                   ; ("SELECT COUNT(*) FROM `user`", D.Rows [[Some "1"]])
                   ; (ucols, D.Rows [row "1" "a"]) ] )
              (fun _ _ pool ->
                Db.transaction pool (fun tx ->
                    let a =
                      text
                        (Tx.exec tx
                           ( User.all |> Query.for_update
                           |> Query.include_ User.Rel.todos
                           |> Query.to_list ) )
                    in
                    let b = text (Tx.exec tx (Query.count User.all)) in
                    steps := [a; b] ) )
          in
          (* 1213 fails the Tx (the next step gives the first error); 1406 is an
             error of the statement only. *)
          equal (list string)
            [inner; (if code = 1213 then inner else "ok")]
            (List.map (fun s -> s) !steps) ;
          equal string result (text r) )
    ; test "the lock of the belongs_to chunks in a Tx" (fun () ->
          let usql = ucols ^ " WHERE `user`.`id` IN (" in
          let parent, _ =
            with_pool
              ~setup:
                (answer
                   [ (usql, D.Rows [row "1" "a"])
                   ; (tcols, D.Rows [todo_row "10" "1"]) ] )
              (fun _ db pool ->
                ignore
                  (Db.transaction pool (fun tx ->
                       Tx.exec_exn tx
                         ( Todo.all |> Query.for_update
                         |> Query.include_ Todo.Rel.user
                         |> Query.to_list ) ) ) ;
                sent db usql )
          in
          equal (list string) [usql ^ "1) FOR UPDATE"] parent )
    ; test "the statement deadline in a Tx: a chunk after its margin" (fun () ->
          let steps = ref [] in
          let r, _ =
            with_pool
              ~setup:
                (answer
                   [ (child_sql, D.Rows [])
                   ; (ucols, D.Delay (80, D.Rows [row "1" "a"])) ] )
              (fun _ _ pool ->
                Db.transaction pool (fun tx ->
                    steps :=
                      [ text
                          (Tx.exec ~timeout:(Weft.Span.ms 120) tx
                             ( User.all
                             |> Query.include_ User.Rel.todos
                             |> Query.to_list ) ) ] ) )
          in
          (* The margin of an earlier statement deadline fails only the
             statement (C-M4): the Tx commits. *)
          equal (list string) ["Timeout"] !steps ;
          equal string "ok" (text r) )
    ; test "count and exists with include_: no extra query" (fun () ->
          let r, events =
            with_pool
              ~setup:
                (answer
                   [ ("SELECT COUNT(*) FROM `user`", D.Rows [[Some "3"]])
                   ; ("SELECT EXISTS", D.Rows [[Some "1"]]) ] )
              (fun _ _ pool ->
                let q = User.all |> Query.include_ User.Rel.todos in
                (Db.exec pool (Query.count q), Db.exec pool (Query.exists q)) )
          in
          is_true (r = (Ok 3, Ok true)) ;
          equal int 3 (List.length events) )
    ; test "in a Tx: the events of each command" (fun () ->
          let r, events =
            with_pool
              ~setup:
                (answer
                   [ (child_sql, D.Rows [todo_row "11" "1"])
                   ; (ucols, D.Rows [row "1" "a"]) ] )
              (fun _ _ pool ->
                Db.transaction pool (fun tx ->
                    Tx.exec_exn tx
                      ( User.all
                      |> Query.include_ User.Rel.todos
                      |> Query.for_update |> Query.to_list ) ) )
          in
          ( match r with
          | Ok [u] ->
              equal string "11" (ids u.todos)
          | _ ->
              fail "the Tx" ) ;
          equal (list string)
            [ "query \"START TRANSACTION\" done 0"
            ; Printf.sprintf "query %S done 1" (ucols ^ " FOR UPDATE")
            ; Printf.sprintf "query %S done 1"
                (child_sql ^ "?) ORDER BY `todo`.`id` ASC FOR UPDATE")
            ; "query \"COMMIT\" done 0" ]
            (Test_pool.queries events) ) ]
