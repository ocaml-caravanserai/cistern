(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* Db.push_schema and Db.check_schema on the real servers (phase 2e), with the
   hand-written models. Phase 2f adds the integration tests of the typed API.
   Phase 3d runs them on the derived models of test/model_ppx, and adds the
   hostile group of D17 with its SQL names. *)

open Windtrap
open Support
open Cistern_test_models_ppx
module Db = Cistern.Db
module Raw = Cistern.Raw
module User = User_todo.User

let models = [User.model; User_todo.Todo.model]

let deterministic =
  group
    ("push_schema and check_schema on " ^ server)
    [ test "push_schema two times, a row, check_schema, two changes" (fun () ->
          in_weft (fun () ->
              warm () ;
              Db.with_db (Db_tests.config ()) (fun db ->
                  let run s = ignore (Db.exec_exn db (Raw.exec (Raw.sql s))) in
                  run "DROP TABLE IF EXISTS `todo`, `user`" ;
                  Fun.protect
                    ~finally:(fun () ->
                      run "DROP TABLE IF EXISTS `todo`, `user`" )
                    (fun () ->
                      Db.push_schema db models ;
                      Db.push_schema db models ;
                      let u =
                        Db.exec_exn db (User.create ~name:"Ann" ~email:"a@x" ())
                      in
                      equal bool true (Db.exec db (User.get_by_id u.id) = Ok u) ;
                      equal
                        (result unit (list string))
                        (Ok ())
                        (Db.check_schema db models) ;
                      run
                        "ALTER TABLE `user` MODIFY `email` VARCHAR(50) NOT NULL" ;
                      run "ALTER TABLE `todo` MODIFY `id` BIGINT NOT NULL" ;
                      equal
                        (result unit (list string))
                        (Error
                           [ "\"user\".\"email\": the length is 50, the model \
                              needs 255"
                           ; "\"todo\".\"id\": the column has no \
                              AUTO_INCREMENT, the model needs it ([@auto])" ] )
                        (Db.check_schema db models) ) ) ) )
    ; test
        "check_schema finds DATETIME with no fraction, a case-insensitive \
         collation and an unsigned column" (fun () ->
          in_weft (fun () ->
              warm () ;
              Db.with_db (Db_tests.config ()) (fun db ->
                  let run s = ignore (Db.exec_exn db (Raw.exec (Raw.sql s))) in
                  let kinds = [Kinds.Kinds.model] in
                  run "DROP TABLE IF EXISTS `kinds`" ;
                  Fun.protect
                    ~finally:(fun () -> run "DROP TABLE IF EXISTS `kinds`")
                    (fun () ->
                      Db.push_schema db kinds ;
                      equal
                        (result unit (list string))
                        (Ok ()) (Db.check_schema db kinds) ;
                      let ci =
                        if mysql then "utf8mb4_0900_ai_ci"
                        else "utf8mb4_general_ci"
                      in
                      let bin =
                        if mysql then "utf8mb4_0900_bin"
                        else "utf8mb4_nopad_bin"
                      in
                      run "ALTER TABLE `kinds` MODIFY `t` DATETIME NOT NULL" ;
                      run
                        ( "ALTER TABLE `kinds` MODIFY `s` MEDIUMTEXT COLLATE "
                        ^ ci ^ " NOT NULL" ) ;
                      run
                        "ALTER TABLE `kinds` MODIFY `i` BIGINT UNSIGNED NOT \
                         NULL" ;
                      equal
                        (result unit (list string))
                        (Error
                           [ "\"kinds\".\"i\": the column is unsigned"
                           ; Printf.sprintf
                               "\"kinds\".\"s\": the collation is \"%s\", the \
                                model needs %s"
                               ci bin
                           ; "\"kinds\".\"t\": the precision is 0, the model \
                              needs 6" ] )
                        (Db.check_schema db kinds) ) ) ) )
    ; test "push_schema and check_schema on the derived models" (fun () ->
          in_weft (fun () ->
              warm () ;
              Db.with_db (Db_tests.config ()) (fun db ->
                  let run s = ignore (Db.exec_exn db (Raw.exec (Raw.sql s))) in
                  let drop () =
                    run "DROP TABLE IF EXISTS `people`, `kinds`, `auto_only`"
                  in
                  let models =
                    [ Person.Person.model
                    ; Kinds.Kinds.model
                    ; Auto_only.Auto_only.model ]
                  in
                  drop () ;
                  Fun.protect ~finally:drop (fun () ->
                      Db.push_schema db models ;
                      Db.push_schema db models ;
                      equal
                        (result unit (list string))
                        (Ok ())
                        (Db.check_schema db models) ) ) ) )
    ; test "the hostile group: its SQL names, the values and check_schema"
        (fun () ->
          in_weft (fun () ->
              warm () ;
              Db.with_db (Db_tests.config ()) (fun db ->
                  (* Hostile has its own modules List, Option and Fun. *)
                  let open Hostile in
                  let run s = ignore (Db.exec_exn db (Raw.exec (Raw.sql s))) in
                  let ex s = Db.exec_exn db s in
                  let drop () =
                    run "DROP TABLE IF EXISTS `order`, `s`, `f`, `d`, `m`, `u`"
                  in
                  let models =
                    [T.model; S.model; F.model; D.model; M.model; U.model]
                  in
                  drop () ;
                  Stdlib.Fun.protect ~finally:drop (fun () ->
                      Db.push_schema db models ;
                      equal
                        (result unit (list string))
                        (Ok ())
                        (Db.check_schema db models) ;
                      (* The defaults name the user module Q. *)
                      let a = ex (T.create ~r:"a" ~e:true ()) in
                      equal int 7 a.v ;
                      equal string "q" a.d ;
                      let b =
                        ex (T.create ~r:"b" ~v:9 ~k:"x" ~d:"y z" ~e:false ())
                      in
                      equal bool true (ex (T.get_by_c b.c) = b) ;
                      equal bool true (ex (T.find_by_r "a") = Some a) ;
                      equal bool true
                        ( ex (Cistern.Query.to_list (T.filter_by_k (Some "x")))
                        = [b] ) ;
                      equal bool true
                        ( ex
                            (Cistern.Query.to_list
                               ( T.where T.Q.(v >. 7)
                               |> Cistern.Query.order_by T.Q.v `Desc ) )
                        = [b] ) ;
                      equal bool true
                        ( ex
                            (Cistern.Query.to_list
                               (Cistern.Query.order_by T.Q.v `Desc T.all) )
                        = [b; a] ) ;
                      let b' = ex (T.update ~r:"c" ~k:None b) in
                      equal bool true (ex (T.get_by_c b.c) = b') ;
                      (* The last field of a derived type, read back. *)
                      let b'' = ex (T.update ~e:true b') in
                      equal bool true b''.e ;
                      equal bool true (ex (T.get_by_c b.c) = b'') ;
                      ex (T.delete a) ;
                      equal bool true (ex (T.find_by_c a.c) = None) ;
                      let s =
                        ex
                          (S.create ~draft:5L ~schema:"q\"\\" ~s_name:"n"
                             ~name:"m" ~model:3 () )
                      in
                      equal bool true (ex (S.get_by_schema "q\"\\") = s) ;
                      equal bool true (ex (S.get_by_name "m") = s) ;
                      equal bool true
                        (ex (Cistern.Query.to_list (S.filter_by_model 3)) = [s]) ;
                      let f = ex (F.create ~update:"u" ~key:1 ()) in
                      equal bool true (ex (F.get_by_key 1) = f) ;
                      equal (option int) None f.all ;
                      let d = ex (D.create ~delete:2 ()) in
                      equal bool true (ex (D.get_by_where d.where) = d) ;
                      let m = ex (M.create ~decode:"x" ()) in
                      equal bool true (ex (M.get_by_fields m.fields) = m) ;
                      let u = ex (U.create ()) in
                      equal bool true (ex (U.get_by_cistern u.cistern) = u) ) ) ) )
    ]
