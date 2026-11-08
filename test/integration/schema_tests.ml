(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* Db.push_schema and Db.check_schema on the real servers (phase 2e), with the
   hand-written models. Phase 2f adds the integration tests of the typed API. *)

open Windtrap
open Support
open Cistern_test_models
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
                        (Db.check_schema db models) ) ) ) ) ]
