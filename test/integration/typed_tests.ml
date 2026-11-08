(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* The integration tests of the typed API (phase 2f) on the three servers, with
   the hand-written models. One suite runs through the two runners of DESIGN.md,
   "OCaml 5.5 features": (fun s -> Db.exec db s) and (fun s -> Tx.exec tx s).
   The OCaml rule of each filter is the reference, as in the property test of
   Expr (test/prop/prop_expr.ml): the server must select the rows of the rule,
   and not p the other rows. *)

open Windtrap
open Support
open Cistern_test_models
module Db = Cistern.Db
module Tx = Cistern.Tx
module Raw = Cistern.Raw
module Stmt = Cistern.Stmt
module Query = Cistern.Query
module Expr = Cistern.Expr
module U = Cistern.Update
module Ev = Cistern_core.Event
module User = User_todo.User
module Todo = User_todo.Todo
module Kinds = Kinds.Kinds
module Auto_only = Auto_only.Auto_only
module Span = Weft.Span
module Time = Weft.Time
module Fiber = Weft.Fiber

let models = [User.model; Todo.model; Kinds.model; Auto_only.model]

let tables = "DROP TABLE IF EXISTS `todo`, `user`, `kinds`, `auto_only`"

(* [with_schema f]: a pool with an event recorder, the tables of the models
   (push_schema), then [f db events]. The tables are dropped at the end. *)
let with_schema ?(pool_size = 2) f =
  in_weft (fun () ->
      warm () ;
      let on_query, events = Db_tests.recorder () in
      Db.with_db ~pool_size ~on_query (Db_tests.config ()) (fun db ->
          let raw s = ignore (Db.exec_exn db (Raw.exec (Raw.sql s))) in
          raw tables ;
          Fun.protect
            ~finally:(fun () -> raw tables)
            (fun () -> Db.push_schema db models ; f db events) ) )

let get = function Ok v -> v | Error e -> fail (error_text e)

let text = function Ok _ -> "ok" | Error e -> error_text e

let ids (us : User_todo.user list) =
  List.sort compare (List.map (fun (u : User_todo.user) -> u.id) us)

(* The filters of the suite, each with its OCaml rule (DESIGN.md, "Filters"):
   ==. and <>. are the equality of OCaml, also for None; an order and a pattern
   never select None. *)
let filters (u2 : User_todo.user) =
  let module Q = User.Q in
  let opt_lt v = function Some x -> x < v | None -> false in
  let opt_ge v = function Some x -> x >= v | None -> false in
  let has p s =
    let n = String.length s and k = String.length p in
    let rec at i = i + k <= n && (String.sub s i k = p || at (i + 1)) in
    at 0
  in
  let nick_has f (r : User_todo.user) =
    Option.fold ~none:false ~some:f r.nick
  in
  [ ( "age ==. Some 30"
    , Q.(age ==. Some 30)
    , fun (r : User_todo.user) -> r.age = Some 30 )
  ; ("age ==. None", Q.(age ==. None), fun r -> r.age = None)
  ; ("is_null age", Q.(is_null age), fun r -> r.age = None)
  ; ("age <>. Some 30", Q.(age <>. Some 30), fun r -> r.age <> Some 30)
  ; ("age <>. None", Q.(age <>. None), fun r -> r.age <> None)
  ; ("age <. Some 30", Q.(age <. Some 30), fun r -> opt_lt 30 r.age)
  ; ("age >=. Some 30", Q.(age >=. Some 30), fun r -> opt_ge 30 r.age)
  ; ( "age >. Some 20"
    , Q.(age >. Some 20)
    , fun r -> match r.age with Some a -> a > 20 | None -> false )
  ; ( "age <=. Some 20"
    , Q.(age <=. Some 20)
    , fun r -> match r.age with Some a -> a <= 20 | None -> false )
  ; ( "in_ age [Some 20; Some 30]"
    , Q.(in_ age [Some 20; Some 30])
    , fun r -> List.mem r.age [Some 20; Some 30] )
  ; ( "in_ age [None; Some 20]"
    , Q.(in_ age [None; Some 20])
    , fun r -> List.mem r.age [None; Some 20] )
  ; ("in_ age [None]", Q.(in_ age [None]), fun r -> r.age = None)
  ; ("in_ age []", Q.(in_ age []), fun _ -> false)
  ; ("name ==. ann", Q.(name ==. "ann"), fun r -> r.name = "ann")
  ; ("name <>. ann", Q.(name <>. "ann"), fun r -> r.name <> "ann")
  ; ("name <. b", Q.(name <. "b"), fun r -> String.compare r.name "b" < 0)
  ; ("active ==. true", Q.(active ==. true), fun r -> r.active)
  ; ("id >. u2", Q.(id >. u2.id), fun r -> r.id > u2.id)
  ; ( "starts_with name c"
    , Q.(starts_with name "c")
    , fun r -> String.starts_with ~prefix:"c" r.name )
  ; ( "starts_with name cy%"
    , Q.(starts_with name "cy%")
    , fun r -> String.starts_with ~prefix:"cy%" r.name )
  ; ("contains name _", Q.(contains name "_"), fun r -> has "_" r.name)
  ; ("contains name !", Q.(contains name "!"), fun r -> has "!" r.name)
  ; ("contains name %", Q.(contains name "%"), fun r -> has "%" r.name)
  ; ("contains name empty", Q.(contains name ""), fun _ -> true)
  ; ( "starts_with_opt nick c_"
    , Q.(starts_with_opt nick "c_")
    , nick_has (String.starts_with ~prefix:"c_") )
  ; ("contains_opt nick !", Q.(contains_opt nick "!"), nick_has (has "!"))
  ; ( "starts_with_opt nick d (case)"
    , Q.(starts_with_opt nick "d")
    , nick_has (String.starts_with ~prefix:"d") )
  ; ("nick ==. Some an", Q.(nick ==. Some "an"), fun r -> r.nick = Some "an")
  ; ("nick <>. Some an", Q.(nick <>. Some "an"), fun r -> r.nick <> Some "an")
  ; ( "age <. Some 30 || is_null age"
    , Q.(age <. Some 30 || is_null age)
    , fun r -> opt_lt 30 r.age || r.age = None )
  ; ( "not (age ==. Some 30) && active ==. true"
    , Q.((not (age ==. Some 30)) && active ==. true)
    , fun r -> r.age <> Some 30 && r.active )
  ; ("and_ []", Expr.and_ [], fun _ -> true)
  ; ("or_ []", Expr.or_ [], fun _ -> false)
  ; ( "or_ [name ==. bob; nick ==. None]"
    , Expr.or_ Q.[name ==. "bob"; nick ==. None]
    , fun r -> r.name = "bob" || r.nick = None ) ]

let user_rows =
  [ ("ann", "a@x", Some "an", Some 30, true)
  ; ("bob", "b@x", None, Some 20, false)
  ; ("cy%", "c@x", Some "c_y", None, true)
  ; ("d_!", "d@x", Some "D!x", Some 30, true)
  ; ("Ann", "e@x", None, None, false) ]

let t_ns = Option.get (Ptime.of_float_s 1700000000.1234567)

let t_1000 = Option.get (Ptime.of_date_time ((1000, 1, 1), ((0, 0, 0), 0)))

(* The suite, on one runner. *)
let suite ~events (run : 'a. 'a Stmt.t -> ('a, Cistern.Error.t) result) =
  let ok s = get (run s) in
  (* A clean start: the bulk delete with no filter. *)
  ignore (ok (Query.delete Todo.all)) ;
  ignore (ok (Query.delete User.all)) ;
  ignore (ok (Query.delete Kinds.all)) ;
  ignore (ok (Raw.exec (Raw.sql "DELETE FROM `auto_only`"))) ;
  (* create, get, find, get_by and find_by. *)
  let users =
    List.map
      (fun (name, email, nick, age, active) ->
        ok (User.create ~name ~email ?nick ?age ~active ()) )
      user_rows
  in
  let u1 = List.nth users 0 and u2 = List.nth users 1 in
  List.iter
    (fun (u : User_todo.user) -> equal bool true (ok (User.get_by_id u.id) = u))
    users ;
  equal string "Not_found" (text (run (User.get_by_id 999_999L))) ;
  equal bool true (ok (User.find_by_id 999_999L) = None) ;
  equal bool true (ok (User.get_by_email "a@x") = u1) ;
  equal bool true (ok (User.get_by_nick "an") = u1) ;
  equal bool true (ok (User.find_by_nick "zz") = None) ;
  (* The filters, and the complement of each one. *)
  let all = ok (Query.to_list User.all) in
  equal (list int64) (ids users) (ids all) ;
  List.iter
    (fun (name, e, rule) ->
      let got = ok (Query.to_list (User.where e)) in
      let neg = ok (Query.to_list (User.where (User.Q.not e))) in
      equal ~msg:name (list int64) (ids (List.filter rule all)) (ids got) ;
      equal ~msg:("not " ^ name) (list int64)
        (ids (List.filter (fun r -> not (rule r)) all))
        (ids neg) ;
      equal ~msg:("count " ^ name) int (List.length got)
        (ok (Query.count (User.where e))) ;
      equal ~msg:("exists " ^ name) bool (got <> [])
        (ok (Query.exists (User.where e))) )
    (filters u2) ;
  (* The order with two keys: NULL first in ASC, last in DESC, as None in
     OCaml. *)
  let sorted =
    ok
      ( User.all
      |> Query.order_by User.Q.age `Desc
      |> Query.order_by User.Q.name `Asc
      |> Query.to_list )
  in
  let expected =
    List.sort
      (fun (a : User_todo.user) (b : User_todo.user) ->
        match compare b.age a.age with
        | 0 ->
            String.compare a.name b.name
        | c ->
            c )
      all
  in
  equal (list int64)
    (List.map (fun (u : User_todo.user) -> u.id) expected)
    (List.map (fun (u : User_todo.user) -> u.id) sorted) ;
  (* Stable pages over equal values: the key goes after active. *)
  let page n =
    ok
      ( User.all
      |> Query.order_by User.Q.active `Asc
      |> Query.limit 2 |> Query.offset n |> Query.to_list )
  in
  equal (list int64) (ids users) (ids (page 0 @ page 2 @ page 4)) ;
  equal int 2 (ok (Query.count (Query.limit 2 User.all))) ;
  equal int 1 (ok (Query.count (Query.offset 4 User.all))) ;
  equal bool false (ok (Query.exists (Query.offset 5 User.all))) ;
  equal bool true
    ( ok (Query.first (Query.order_by User.Q.name `Asc User.all))
    = Some (List.nth users 4) ) ;
  (* update, and the bulk forms with the complement. *)
  let u1' = ok (User.update ~age:(Some 31) ~nick:None u1) in
  equal bool true (u1' = {u1 with age= Some 31; nick= None}) ;
  equal bool true (ok (User.get_by_id u1.id) = u1') ;
  equal bool true (ok (User.update u1') = u1') ;
  let p = User.Q.(age <. Some 30 || is_null age) in
  let in_p =
    List.filter
      (fun (r : User_todo.user) ->
        match r.age with Some a -> a < 30 | None -> true )
      (ok (Query.to_list User.all))
  in
  equal int (List.length in_p)
    (ok (Query.update (U.set User.Q.active true) (User.where p))) ;
  equal bool true
    (List.for_all
       (fun (r : User_todo.user) -> r.active)
       (ok (Query.to_list (User.where p))) ) ;
  (* Todo: create, filter_by_*, the bulk delete with an order and a limit. *)
  let todo title done_ = ok (Todo.create ~user_id:u2.id ~title ~done_ ()) in
  (* "milk" true comes first: a delete with no order would take it, the delete
     of the order by title takes "eggs". *)
  let _ = todo "milk" true in
  let _ = todo "eggs" true in
  let t1 = todo "milk" false in
  equal int 3 (List.length (ok (Query.to_list (Todo.filter_by_user_id u2.id)))) ;
  equal int 2 (List.length (ok (Query.to_list (Todo.filter_by_title "milk")))) ;
  equal int 1
    (ok
       (Query.delete
          ( Todo.where Todo.Q.(done_ ==. true)
          |> Query.order_by Todo.Q.title `Asc
          |> Query.limit 1 ) ) ) ;
  equal int 2 (ok (Query.count Todo.all)) ;
  equal (list string) ["milk"]
    (List.map
       (fun (x : User_todo.todo) -> x.title)
       (ok (Query.to_list (Todo.where Todo.Q.(done_ ==. true)))) ) ;
  (* Stable pages over 24 rows with one value of the order: each id comes one
     time, because the key goes after the keys of the order. With no key, MySQL
     8.4 gives a row on two pages; MariaDB keeps the order of the scan for equal
     values, thus the expect tests of the SQL find a missing key there. *)
  let paged = List.init 24 (fun _ -> (todo "page" false).id) in
  let page n =
    ok
      ( Todo.where Todo.Q.(title ==. "page")
      |> Query.order_by Todo.Q.done_ `Asc
      |> Query.limit 4 |> Query.offset n |> Query.to_list )
  in
  equal (list int64) (List.sort compare paged)
    (List.sort compare
       (List.concat_map
          (fun n -> List.map (fun (x : User_todo.todo) -> x.id) (page n))
          [0; 4; 8; 12; 16; 20] ) ) ;
  (* The SQL of each page has the key after the keys of the order, on each
     server. *)
  List.iter
    (fun n ->
      let tail =
        Printf.sprintf
          "ORDER BY `todo`.`done` ASC, `todo`.`id` ASC LIMIT 4 OFFSET %d" n
      in
      equal ~msg:tail bool true
        (List.exists
           (function
             | Ev.Query {sql; _} ->
                 String.ends_with ~suffix:tail sql
             | _ ->
                 false )
           (events ()) ) )
    [0; 4; 8; 12; 16; 20] ;
  ignore (ok (Query.delete (Todo.where Todo.Q.(title ==. "page")))) ;
  (* The errors: each fails only its statement. *)
  equal string "Constraint_violation Unique"
    (text (run (User.create ~name:"x" ~email:"a@x" ()))) ;
  equal string "Constraint_violation Foreign_key"
    (text (run (Todo.create ~user_id:999_999L ~title:"x" ()))) ;
  equal string "Constraint_violation Not_null"
    (text (run (Todo.create ~title:"x" ()))) ;
  equal string "Invalid_value 1406"
    (text (run (User.create ~name:"x" ~email:(String.make 300 'e') ()))) ;
  equal string "Constraint_violation Foreign_key" (text (run (User.delete u2))) ;
  (* delete. *)
  equal bool true (ok (Todo.delete t1) = ()) ;
  equal string "Not_found" (text (run (Todo.delete t1))) ;
  equal string "Not_found"
    (text (run (User.update ~name:"z" {u1' with id= 999_999L}))) ;
  (* The round trip of each scalar, and the cut of a Ptime.t. *)
  let k =
    ok
      (Kinds.create
         (Kinds.draft ~id:1L ~i:(-5) ~s:"\xc3\xa9\x00?" ~s_opt:"x" ~b:true
            ~b_opt:false ~f:(-0.0) ~f_opt:1e300
            ~y:(Bytes.of_string "\x00\xff")
            ~t:t_ns ~t_opt:t_1000 () ) )
  in
  let back = ok (Kinds.get_by_id 1L) in
  equal bool true (back = k) ;
  equal bool true (Ptime.equal back.t (Ptime.truncate ~frac_s:6 t_ns)) ;
  (* MySQL keeps -0.0, MariaDB gives 0.0 (DESIGN.md, "Literals"). *)
  equal bool mysql (Float.sign_bit back.f) ;
  let k' =
    ok (Kinds.update [U.set Kinds.Q.t t_ns; U.set Kinds.Q.i_opt (Some 7)] k)
  in
  equal bool true (ok (Kinds.get_by_id 1L) = k') ;
  (* Auto_only: INSERT INTO auto_only () VALUES (). *)
  let a = ok (Auto_only.create ()) and b = ok (Auto_only.create ()) in
  equal bool true (b.id > a.id) ;
  equal bool true (ok (Auto_only.get_by_id b.id) = b)

let deterministic =
  group
    ("the typed API on " ^ server)
    [ test "through Db.exec" (fun () ->
          with_schema (fun db events -> suite ~events (fun s -> Db.exec db s)) )
    ; test "through Tx.exec, then COMMIT" (fun () ->
          with_schema (fun db events ->
              match
                Db.transaction db (fun tx ->
                    suite ~events (fun s -> Tx.exec tx s) )
              with
              | Ok () ->
                  equal int 5
                    (List.length (get (Db.exec db (Query.to_list User.all))))
              | Error e ->
                  fail (error_text e) ) )
    ; test "the event of a 1062 holds no value" (fun () ->
          with_schema (fun db events ->
              ignore
                (get (Db.exec db (User.create ~name:"a" ~email:"secret@x" ()))) ;
              equal string "Constraint_violation Unique"
                (text (Db.exec db (User.create ~name:"b" ~email:"secret@x" ()))) ;
              let inserts =
                List.filter_map
                  (function
                    | Ev.Query {sql; outcome; _}
                      when String.starts_with ~prefix:"INSERT INTO `user`" sql
                      ->
                        Some (sql, outcome)
                    | _ ->
                        None )
                  (events ())
              in
              (* The successful INSERT and the failed one, each redacted. *)
              equal int 2 (List.length inserts) ;
              let failed =
                List.filter_map
                  (function _, Ev.Failed e -> Some e | _ -> None)
                  inserts
              in
              equal int 1 (List.length failed) ;
              List.iter
                (fun e ->
                  equal bool false
                    (Db_tests.contains (Cistern.Error.to_string e) "secret") )
                failed ;
              List.iter
                (fun (sql, _) ->
                  equal string
                    "INSERT INTO `user` (`name`, `email`, `nick`, `age`, \
                     `active`) VALUES (?, ?, ?, ?, ?)"
                    sql )
                inserts ) )
    ; test "for_update: a second Tx waits for the first" (fun () ->
          with_schema ~pool_size:3 (fun db _ ->
              let u =
                get (Db.exec db (User.create ~name:"a" ~email:"a@x" ()))
              in
              let q = User.where User.Q.(id ==. u.id) |> Query.for_update in
              equal string "Db.exec: a query with for_update runs only in a Tx"
                ( match Db.exec db (Query.first q) with
                | _ ->
                    "no error"
                | exception Invalid_argument m ->
                    m ) ;
              let locked = Fiber.Semaphore.create ~max:1 0 in
              let waited = ref 0. in
              Fiber.scope (fun s ->
                  Fiber.fork s (fun () ->
                      ignore
                        (Db.transaction db (fun tx ->
                             ignore (get (Tx.exec tx (Query.first q))) ;
                             Fiber.Semaphore.release locked ;
                             Time.sleep Weft.Clock.Mono (Span.ms 500) ;
                             ignore
                               (get (Tx.exec tx (User.update ~name:"first" u))) )
                        ) ) ;
                  Fiber.fork s (fun () ->
                      Weft.sync (Fiber.Semaphore.acquire locked) ;
                      let t0 = Time.now Weft.Clock.Mono in
                      let r =
                        Db.transaction db (fun tx ->
                            Option.map
                              (fun (x : User_todo.user) -> x.name)
                              (get (Tx.exec tx (Query.first q))) )
                      in
                      waited := Db_tests.seconds t0 ;
                      (* The second Tx reads the row of the first COMMIT. *)
                      equal bool true (r = Ok (Some "first")) ) ) ;
              equal bool true (!waited >= 0.3) ) )
    ; test "for_share in Db.exec and in a Tx" (fun () ->
          with_schema (fun db _ ->
              let u =
                get (Db.exec db (User.create ~name:"a" ~email:"a@x" ()))
              in
              let q = Query.for_share User.all in
              equal bool true (get (Db.exec db (Query.to_list q)) = [u]) ;
              equal bool true
                ( Db.transaction db (fun tx ->
                      get (Tx.exec tx (Query.to_list q)) )
                = Ok [u] ) ) ) ]
