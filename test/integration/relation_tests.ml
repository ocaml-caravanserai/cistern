(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* The integration rows of relations (phase 4e, the plan of phase 4, section
   4.7) on the three servers, with the hand-written user and todo: the nested
   create in Db.exec and in a Tx, a child that fails, include_ with 2500 keys
   and with a locking main query, the reset of a key, the scopes, and the delete
   of a parent with children. *)

open Windtrap
open Support
open Cistern_test_models
module Db = Cistern.Db
module Tx = Cistern.Tx
module Raw = Cistern.Raw
module Query = Cistern.Query
module Ev = Cistern_core.Event
module Message = Cistern_mysql.Message
module User = User_todo.User
module Todo = User_todo.Todo

let tables = "DROP TABLE IF EXISTS `todo`, `user`"

(* [with_schema f]: a pool of one connection (so that each statement runs on the
   same connection) with an event recorder, the tables of user and todo, then [f
   db events]. *)
let with_schema f =
  in_weft (fun () ->
      warm () ;
      let on_query, events = Db_tests.recorder () in
      Db.with_db ~pool_size:1 ~on_query (Db_tests.config ()) (fun db ->
          let raw s = ignore (Db.exec_exn db (Raw.exec (Raw.sql s))) in
          raw tables ;
          Fun.protect
            ~finally:(fun () -> raw tables)
            (fun () ->
              Db.push_schema db [User.model; Todo.model] ;
              f db events ) ) )

let get = function Ok v -> v | Error e -> fail (error_text e)

let text = function Ok _ -> "ok" | Error e -> error_text e

(* The SQL of the Query events from the event [from] on. *)
let sqls events ~from =
  List.filteri (fun i _ -> i >= from) (events ())
  |> List.filter_map (function Ev.Query {sql; _} -> Some sql | _ -> None)

let starts p s = String.starts_with ~prefix:p s

(* The SQL texts, cut before the first parenthesis (the columns). *)
let heads l =
  List.map
    (fun s ->
      match String.index_opt s '(' with
      | Some i ->
          String.trim (String.sub s 0 i)
      | None ->
          s )
    l

(* A statement of another session, on its own connection. *)
let other sql = with_conn (on_socket ~clear:true ()) (fun c -> exec_ok c sql)

(* [n] users u<first>..., in one raw INSERT. *)
let insert_users db ~first n =
  let values =
    List.init n (fun i ->
        Printf.sprintf "('u%d', 'u%d@x', 1)" (first + i) (first + i) )
  in
  ignore
    (Db.exec_exn db
       (Raw.exec
          (Raw.sql
             ( "INSERT INTO `user` (`name`, `email`, `active`) VALUES "
             ^ String.concat ", " values ) ) ) )

(* The todos of the pairs (user id, title), in one raw INSERT. *)
let insert_todos db l =
  let values = List.map (fun (u, t) -> Printf.sprintf "(%Ld, '%s', 0)" u t) l in
  ignore
    (Db.exec_exn db
       (Raw.exec
          (Raw.sql
             ( "INSERT INTO `todo` (`user_id`, `title`, `done`) VALUES "
             ^ String.concat ", " values ) ) ) )

let loaded = function
  | Cistern.Deferred.Loaded l ->
      l
  | Unloaded ->
      fail "the relation is not loaded"

(* The IN_TRANS flag of the next checkout, after a ping (the seam on_hold). *)
let in_trans_at_next_checkout db =
  let flag = ref None in
  (Cistern.Private.Pool.Private.on_hold :=
     fun c ->
       ignore (Conn.ping c) ;
       flag := Some (Conn.status c land Message.status_bit In_trans <> 0) ) ;
  Fun.protect
    ~finally:(fun () -> Cistern.Private.Pool.Private.on_hold := ignore)
    (fun () -> ignore (Db.exec db (Query.count User.all))) ;
  !flag

let deterministic =
  group ("relations on " ^ server)
    [ test "the nested create in Db.exec: the transaction and the parent key"
        (fun () ->
          with_schema (fun db events ->
              (* A first user, so that the parent does not have the id 1. *)
              ignore (get (Db.exec db (User.create ~name:"z" ~email:"z@x" ()))) ;
              let from = List.length (events ()) in
              let u =
                get
                  (Db.exec db
                     (User.create ~name:"a" ~email:"a@x"
                        ~todos:
                          [Todo.draft ~title:"a1" (); Todo.draft ~title:"a2" ()]
                        () ) )
              in
              equal (list string)
                [ "START TRANSACTION"
                ; "INSERT INTO `user`"
                ; "INSERT INTO `todo`"
                ; "INSERT INTO `todo`"
                ; "COMMIT" ]
                (heads (sqls events ~from)) ;
              let server_rows =
                get
                  (Db.exec db
                     (Query.to_list
                        (Query.order_by Todo.Q.id `Asc
                           (Todo.filter_by_user_id u.id) ) ) )
              in
              equal (list string) ["a1"; "a2"]
                (List.map (fun (t : User_todo.todo) -> t.title) server_rows) ;
              equal (list int64)
                (List.map (fun (t : User_todo.todo) -> t.id) server_rows)
                (List.map (fun (t : User_todo.todo) -> t.id) (loaded u.todos)) ) )
    ; test "a child that fails: no parent, the error of the child, no open Tx"
        (fun () ->
          with_schema (fun db events ->
              let from = List.length (events ()) in
              equal string "Invalid_value 1406"
                (text
                   (Db.exec db
                      (User.create ~name:"b" ~email:"b@x"
                         ~todos:
                           [ Todo.draft ~title:"b1" ()
                           ; Todo.draft ~title:(String.make 300 't') () ]
                         () ) ) ) ;
              equal (list string)
                [ "START TRANSACTION"
                ; "INSERT INTO `user`"
                ; "INSERT INTO `todo`"
                ; "INSERT INTO `todo`"
                ; "ROLLBACK" ]
                (heads (sqls events ~from)) ;
              (* The next statement runs on the same connection, with IN_TRANS
                 clear. *)
              equal (option bool) (Some false) (in_trans_at_next_checkout db) ;
              is_true (get (Db.exec db (User.find_by_email "b@x")) = None) ;
              equal int 0 (get (Db.exec db (Query.count Todo.all))) ) )
    ; test "the nested create in a Tx: a savepoint, and the Tx goes on"
        (fun () ->
          with_schema (fun db events ->
              let from = List.length (events ()) in
              let r =
                Db.transaction db (fun tx ->
                    ignore
                      (Tx.exec_exn tx (User.create ~name:"c0" ~email:"c0@x" ())) ;
                    let failed =
                      text
                        (Tx.exec tx
                           (User.create ~name:"c1" ~email:"c1@x"
                              ~todos:
                                [ Todo.draft ~title:"c1a" ()
                                ; Todo.draft ~title:(String.make 300 't') () ]
                              () ) )
                    in
                    ignore
                      (Tx.exec_exn tx
                         (User.create ~name:"c2" ~email:"c2@x"
                            ~todos:[Todo.draft ~title:"c2a" ()]
                            () ) ) ;
                    failed )
              in
              equal string "Invalid_value 1406" (get r) ;
              equal (list string)
                [ "START TRANSACTION"
                ; "INSERT INTO `user`"
                ; "SAVEPOINT cistern_sp_1"
                ; "INSERT INTO `user`"
                ; "INSERT INTO `todo`"
                ; "INSERT INTO `todo`"
                ; "ROLLBACK TO SAVEPOINT cistern_sp_1"
                ; "SAVEPOINT cistern_sp_2"
                ; "INSERT INTO `user`"
                ; "INSERT INTO `todo`"
                ; "RELEASE SAVEPOINT cistern_sp_2"
                ; "COMMIT" ]
                (heads (sqls events ~from)) ;
              equal (list string) ["c0"; "c2"]
                (List.map
                   (fun (u : User_todo.user) -> u.name)
                   (get
                      (Db.exec db
                         (Query.to_list
                            (Query.order_by User.Q.id `Asc User.all) ) ) ) ) ;
              equal (list string) ["c2a"]
                (List.map
                   (fun (t : User_todo.todo) -> t.title)
                   (get (Db.exec db (Query.to_list Todo.all))) ) ) )
    ; test "a child INSERT after a plain read and a concurrent update"
        (fun () ->
          (* On MariaDB 11.8 (innodb_snapshot_isolation), the foreign key check
             of the child reads the parent that another session changed after
             the snapshot: 1020, and the Tx fails (E0, E6). *)
          with_schema (fun db _ ->
              let u =
                get (Db.exec db (User.create ~name:"d" ~email:"d@x" ()))
              in
              let inner = ref "" in
              let r =
                Db.transaction db (fun tx ->
                    ignore (Tx.exec_exn tx (User.get_by_id u.id)) ;
                    other
                      (Printf.sprintf
                         "UPDATE `user` SET `name` = 'd2' WHERE `id` = %Ld" u.id ) ;
                    inner :=
                      text
                        (Tx.exec tx (Todo.create ~user_id:u.id ~title:"d1" ())) )
              in
              let expected =
                if server = "mariadb118" then "Retryable rolled back" else "ok"
              in
              equal string expected !inner ;
              equal string expected (text r) ;
              equal int
                (if server = "mariadb118" then 0 else 1)
                (get (Db.exec db (Query.count Todo.all))) ) )
    ; test "include_ with 2500 parents: 3 chunks, the grouping" (fun () ->
          with_schema (fun db events ->
              insert_users db ~first:1 2500 ;
              let users =
                get
                  (Db.exec db
                     (Query.to_list (Query.order_by User.Q.id `Asc User.all)) )
              in
              (* One todo for each 10th user, two for each 100th. *)
              insert_todos db
                (List.concat_map
                   (fun (u : User_todo.user) ->
                     if Int64.rem u.id 100L = 0L then [(u.id, "x"); (u.id, "y")]
                     else if Int64.rem u.id 10L = 0L then [(u.id, "x")]
                     else [] )
                   users ) ;
              let from = List.length (events ()) in
              let r =
                get
                  (Db.exec db
                     (User.all |> Query.include_ User.Rel.todos |> Query.to_list) )
              in
              let s = sqls events ~from in
              equal int 4 (List.length s) ;
              equal (list int) [1000; 1000; 500]
                (List.map
                   (fun e -> List.length (String.split_on_char '?' e) - 1)
                   (List.tl s) ) ;
              equal int 2500 (List.length r) ;
              equal int (225 + 50)
                (List.fold_left
                   (fun n (u : User_todo.user) ->
                     n + List.length (loaded u.todos) )
                   0 r ) ;
              List.iter
                (fun (u : User_todo.user) ->
                  let ts = loaded u.todos in
                  is_true
                    (List.for_all
                       (fun (t : User_todo.todo) -> t.user_id = u.id)
                       ts ) ;
                  equal int
                    ( if Int64.rem u.id 100L = 0L then 2
                      else if Int64.rem u.id 10L = 0L then 1
                      else 0 )
                    (List.length ts) )
                r ;
              (* The belongs_to direction over 2500 parents (the review of 4e,
                 L1): a todo for each user with none, then each todo gets its
                 own parent, in 3 chunks of parent keys. *)
              insert_todos db
                (List.filter_map
                   (fun (u : User_todo.user) ->
                     if Int64.rem u.id 10L = 0L then None else Some (u.id, "w") )
                   users ) ;
              let from = List.length (events ()) in
              let ts =
                get
                  (Db.exec db
                     (Todo.all |> Query.include_ Todo.Rel.user |> Query.to_list) )
              in
              equal int (2250 + 275) (List.length ts) ;
              equal int 4 (List.length (sqls events ~from)) ;
              is_true
                (List.for_all
                   (fun (t : User_todo.todo) ->
                     match t.user with
                     | Loaded p ->
                         p.id = t.user_id
                         && p.name = Printf.sprintf "u%Ld" t.user_id
                     | Unloaded ->
                         false )
                   ts ) ) )
    ; test "include_ with 2500 children of one parent, in each direction"
        (fun () ->
          with_schema (fun db events ->
              let u =
                get (Db.exec db (User.create ~name:"p" ~email:"p@x" ()))
              in
              insert_todos db
                (List.init 2500 (fun i -> (u.id, Printf.sprintf "t%d" i))) ;
              let from = List.length (events ()) in
              let p =
                get
                  (Db.exec db
                     ( User.where User.Q.(id ==. u.id)
                     |> Query.include_ User.Rel.todos
                     |> Query.first ) )
              in
              let children = loaded (Option.get p).todos in
              equal int 2500 (List.length children) ;
              (* In the order of the key of the child. *)
              let ids = List.map (fun (t : User_todo.todo) -> t.id) children in
              is_true (ids = List.sort compare ids) ;
              let ts =
                get
                  (Db.exec db
                     ( Todo.filter_by_user_id u.id
                     |> Query.include_ Todo.Rel.user
                     |> Query.to_list ) )
              in
              equal int 2500 (List.length ts) ;
              is_true
                (List.for_all
                   (fun (t : User_todo.todo) ->
                     match t.user with
                     | Loaded p ->
                         p.id = u.id
                     | Unloaded ->
                         false )
                   ts ) ;
              (* 2 + 2 queries: the one key of the belongs_to is one chunk. *)
              equal int 4 (List.length (sqls events ~from)) ) )
    ; cases "the lock of include_ after a locking main query"
        ~id:(fun (n, _) -> n)
        [("for_update", Query.for_update); ("for_share", Query.for_share)]
        (fun (_, lock) ->
          (* A plain read makes the snapshot, then another session adds a child
             of the locked parent: the locking query of include_ reads the
             newest committed rows, or gives 1020 on MariaDB 11.8 (A-M1). *)
          with_schema (fun db _ ->
              let u =
                get (Db.exec db (User.create ~name:"l" ~email:"l@x" ()))
              in
              let w =
                get (Db.exec db (User.create ~name:"w" ~email:"w@x" ()))
              in
              insert_todos db [(u.id, "old")] ;
              let r =
                Db.transaction db (fun tx ->
                    ignore (Tx.exec_exn tx (User.get_by_id w.id)) ;
                    other
                      (Printf.sprintf
                         "INSERT INTO `todo` (`user_id`, `title`, `done`) \
                          VALUES (%Ld, 'new', 0)"
                         u.id ) ;
                    match
                      Tx.exec tx
                        ( User.where User.Q.(id ==. u.id)
                        |> lock
                        |> Query.include_ User.Rel.todos
                        |> Query.to_list )
                    with
                    | Ok [p] ->
                        String.concat ","
                          (List.map
                             (fun (t : User_todo.todo) -> t.title)
                             (loaded p.todos) )
                    | Ok _ ->
                        "rows"
                    | Error e ->
                        error_text e )
              in
              if server = "mariadb118" then
                equal string "Retryable rolled back" (text r)
              else equal string "old,new" (get r) ) )
    ; cases "the lock of the belongs_to queries of include_"
        ~id:(fun (n, _) -> n)
        [("for_update", Query.for_update); ("for_share", Query.for_share)]
        (fun (_, lock) ->
          (* A plain read makes the snapshot, then another session changes the
             parent of a todo: the locking query of the parent reads the new
             value, or gives 1020 on MariaDB 11.8 (the review of 4e, M1). *)
          with_schema (fun db _ ->
              let u =
                get (Db.exec db (User.create ~name:"l" ~email:"l@x" ()))
              in
              let w =
                get (Db.exec db (User.create ~name:"w" ~email:"w@x" ()))
              in
              let t =
                get (Db.exec db (Todo.create ~user_id:u.id ~title:"t" ()))
              in
              let r =
                Db.transaction db (fun tx ->
                    ignore (Tx.exec_exn tx (User.get_by_id w.id)) ;
                    other
                      (Printf.sprintf
                         "UPDATE `user` SET `name` = 'l2' WHERE `id` = %Ld" u.id ) ;
                    match
                      Tx.exec tx
                        ( Todo.where Todo.Q.(id ==. t.id)
                        |> lock
                        |> Query.include_ Todo.Rel.user
                        |> Query.to_list )
                    with
                    | Ok [{user= Loaded p; _}] ->
                        p.name
                    | Ok _ ->
                        "rows"
                    | Error e ->
                        error_text e )
              in
              if server = "mariadb118" then
                equal string "Retryable rolled back" (text r)
              else equal string "l2" (get r) ) )
    ; test "update of the key: Unloaded for a new key, kept for the same key"
        (fun () ->
          with_schema (fun db _ ->
              let u1 =
                get (Db.exec db (User.create ~name:"r1" ~email:"r1@x" ()))
              in
              let u2 =
                get (Db.exec db (User.create ~name:"r2" ~email:"r2@x" ()))
              in
              let t =
                get (Db.exec db (Todo.create ~user_id:u1.id ~title:"r" ()))
              in
              let with_user () =
                Option.get
                  (get
                     (Db.exec db
                        ( Todo.where Todo.Q.(id ==. t.id)
                        |> Query.include_ Todo.Rel.user
                        |> Query.first ) ) )
              in
              let t1 = with_user () in
              is_true (t1.user <> Cistern.Deferred.Unloaded) ;
              let moved = get (Db.exec db (Todo.update ~user_id:u2.id t1)) in
              is_true (moved.user = Cistern.Deferred.Unloaded) ;
              equal int64 u2.id (get (Db.exec db (Todo.get_by_id t.id))).user_id ;
              let t2 = with_user () in
              let same = get (Db.exec db (Todo.update ~user_id:u2.id t2)) in
              is_true (same.user = t2.user) ;
              is_true (same.user <> Cistern.Deferred.Unloaded) ) )
    ; test "User.todos with a filter and an order, against the OCaml rule"
        (fun () ->
          with_schema (fun db _ ->
              let u =
                get (Db.exec db (User.create ~name:"s" ~email:"s@x" ()))
              in
              let o =
                get (Db.exec db (User.create ~name:"o" ~email:"o@x" ()))
              in
              let rows =
                [ ("b", true)
                ; ("d", false)
                ; ("a", false)
                ; ("c", false)
                ; ("e", true) ]
              in
              List.iter
                (fun (title, done_) ->
                  ignore
                    (get
                       (Db.exec db (Todo.create ~user_id:u.id ~title ~done_ ())) ) ;
                  ignore
                    (get
                       (Db.exec db (Todo.create ~user_id:o.id ~title ~done_ ())) ) )
                rows ;
              let rule =
                List.filter (fun (_, d) -> not d) rows
                |> List.map fst
                |> List.sort (fun a b -> compare b a)
              in
              let got =
                get
                  (Db.exec db
                     ( User.todos u
                     |> Query.where Todo.Q.(done_ ==. false)
                     |> Query.order_by Todo.Q.title `Desc
                     |> Query.to_list ) )
              in
              equal (list string) rule
                (List.map (fun (t : User_todo.todo) -> t.title) got) ;
              is_true
                (List.for_all
                   (fun (t : User_todo.todo) -> t.user_id = u.id)
                   got ) ) )
    ; test "the delete of a parent with children: Foreign_key" (fun () ->
          with_schema (fun db _ ->
              let u =
                get
                  (Db.exec db
                     (User.create ~name:"f" ~email:"f@x"
                        ~todos:[Todo.draft ~title:"f1" ()]
                        () ) )
              in
              equal string "Constraint_violation Foreign_key"
                (text (Db.exec db (User.delete u))) ) ) ]
