(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* The one-row operations of the typed API (phase 2d1) on Fake_db, with the
   hand-written models: the exact SQL, the results, the events, and the same in
   a Tx. The script of Fake_db answers each exact SQL text. *)

open Windtrap
open Cistern_test_models
module D = Fake_db
module Db = Cistern.Db
module Tx = Cistern.Tx
module E = Cistern_core.Error
module User = User_todo.User
module Todo = User_todo.Todo

let with_pool = Test_pool.with_pool

let user_cols =
  "SELECT `user`.`id`, `user`.`name`, `user`.`email`, `user`.`nick`, \
   `user`.`age`, `user`.`active` FROM `user`"

let get7 = user_cols ^ " WHERE `user`.`id` = 7 LIMIT 1"

let row7 = [Some "7"; Some "Ann"; Some "a@x"; None; Some "33"; Some "1"]

let ann =
  { User_todo.id= 7L
  ; name= "Ann"
  ; email= "a@x"
  ; nick= None
  ; age= Some 33
  ; active= true }

(* The script: each pair is an exact SQL text and its reply. *)
let script pairs db = db.D.faults.script <- (fun sql -> List.assoc_opt sql pairs)

let text = function Ok _ -> "ok" | Error e -> E.to_string e

let insert_user =
  "INSERT INTO `user` (`name`, `email`, `nick`, `age`, `active`) VALUES \
   (_utf8mb4 X'426F62', _utf8mb4 X'6240', NULL, NULL, 1)"

let update_user =
  "UPDATE `user` SET `name` = _utf8mb4 X'5A', `age` = NULL WHERE `user`.`id` = \
   7"

let delete_user = "DELETE FROM `user` WHERE `user`.`id` = 7"

let counts affected insert = D.Counts {affected; insert}

let tests =
  group "the one-row operations"
    [ test "get: a row, and no row" (fun () ->
          let r, events =
            with_pool
              ~setup:
                (script
                   [ (get7, D.Rows [row7])
                   ; (user_cols ^ " WHERE `user`.`id` = 8 LIMIT 1", D.Rows [])
                   ] )
              (fun _ _ pool ->
                let a = Db.exec pool (User.get_by_id 7L) in
                let b = Db.exec pool (User.get_by_id 8L) in
                (a, text b) )
          in
          equal bool true (fst r = Ok ann) ;
          equal string "Not_found" (snd r) ;
          equal (list string)
            [ "connect 10.0.0.9:3306 ok"
            ; Printf.sprintf "query %S done 1"
                (user_cols ^ " WHERE `user`.`id` = ? LIMIT 1")
            ; Printf.sprintf "query %S failed Not_found"
                (user_cols ^ " WHERE `user`.`id` = ? LIMIT 1") ]
            events )
    ; test "find: Some and None" (fun () ->
          let r, _ =
            with_pool
              ~setup:
                (script
                   [ (get7, D.Rows [row7])
                   ; (user_cols ^ " WHERE `user`.`id` = 8 LIMIT 1", D.Rows [])
                   ] )
              (fun _ _ pool ->
                ( Db.exec pool (User.find_by_id 7L)
                , Db.exec pool (User.find_by_id 8L) ) )
          in
          equal bool true (r = (Ok (Some ann), Ok None)) )
    ; test "get_by and find_by of a unique field" (fun () ->
          let by_email =
            user_cols ^ " WHERE `user`.`email` = _utf8mb4 X'6140' LIMIT 1"
          in
          let by_nick =
            user_cols ^ " WHERE `user`.`nick` = _utf8mb4 X'616E' LIMIT 1"
          in
          let r, _ =
            with_pool
              ~setup:(script [(by_email, D.Rows [row7]); (by_nick, D.Rows [])])
              (fun _ _ pool ->
                ( Db.exec pool (User.get_by_email "a@")
                , Db.exec pool (User.find_by_email "a@")
                , text (Db.exec pool (User.get_by_nick "an"))
                , Db.exec pool (User.find_by_nick "an") ) )
          in
          let a, b, c, d = r in
          equal bool true (a = Ok ann) ;
          equal bool true (b = Ok (Some ann)) ;
          equal string "Not_found" c ;
          equal bool true (d = Ok None) )
    ; test "get_by with None raises Invalid_argument" (fun () ->
          raises
            (Invalid_argument
               "Model.get_by: None for the field \"user\".\"nick\"" ) (fun () ->
              Cistern.Private.Model.get_by
                (module User_todo.Cistern_model__user)
                User.Q.nick None ) )
    ; test "create: the last insert id, 0, a key in the draft, Auto_only"
        (fun () ->
          let insert_kinds k =
            Kinds.Kinds.draft ~id:k ~i:1 ~s:"s" ~b:true ~f:1.5
              ~y:(Bytes.of_string "y")
              ~t:(Option.get (Ptime.of_float_s 0.))
              ()
          in
          let kinds_sql =
            "INSERT INTO `kinds` (`id`, `i`, `i_opt`, `s`, `s_opt`, `b`, \
             `b_opt`, `f`, `f_opt`, `y`, `y_opt`, `t`, `t_opt`) VALUES (4, 1, \
             NULL, _utf8mb4 X'73', NULL, 1, NULL, 1.5e0, NULL, X'79', NULL, \
             '1970-01-01 00:00:00.000000', NULL)"
          in
          let r, events =
            with_pool
              ~setup:(fun db ->
                let n = ref 0 in
                db.D.faults.script <-
                  (fun sql ->
                    if sql = insert_user then (
                      incr n ;
                      Some (counts 1L (if !n = 1 then 12L else 0L)) )
                    else if sql = "INSERT INTO `auto_only` () VALUES ()" then
                      Some (counts 1L 5L)
                    else if sql = kinds_sql then Some (counts 1L 0L)
                    else None ) )
              (fun _ _ pool ->
                let c () =
                  Db.exec pool (User.create ~name:"Bob" ~email:"b@" ())
                in
                let a = c () in
                let b = c () in
                let o = Db.exec pool (Auto_only.Auto_only.create ()) in
                let k =
                  Result.map
                    (fun (k : Kinds.kinds) -> k.id)
                    (Db.exec pool (Kinds.Kinds.create (insert_kinds 4L)))
                in
                (a, text b, o, k) )
          in
          let a, b, c, d = r in
          equal bool true
            ( a
            = Ok
                { User_todo.id= 12L
                ; name= "Bob"
                ; email= "b@"
                ; nick= None
                ; age= None
                ; active= true } ) ;
          equal string "Decode: the INSERT of an [@auto] key gave no insert id"
            b ;
          equal bool true (c = Ok {Auto_only.id= 5L}) ;
          equal bool true (d = Ok 4L) ;
          equal (list string)
            [ "connect 10.0.0.9:3306 ok"
            ; "query \"INSERT INTO `user` (`name`, `email`, `nick`, `age`, \
               `active`) VALUES (?, ?, ?, ?, ?)\" done 1"
            ; "query \"INSERT INTO `user` (`name`, `email`, `nick`, `age`, \
               `active`) VALUES (?, ?, ?, ?, ?)\" failed Decode"
            ; "query \"INSERT INTO `auto_only` () VALUES ()\" done 1"
            ; "query \"INSERT INTO `kinds` (`id`, `i`, `i_opt`, `s`, `s_opt`, \
               `b`, `b_opt`, `f`, `f_opt`, `y`, `y_opt`, `t`, `t_opt`) VALUES \
               (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)\" done 1" ]
            events )
    ; test "update: 1 matched row, 0, and no field" (fun () ->
          let r, events =
            with_pool
              ~setup:(fun db ->
                let n = ref 0 in
                db.D.faults.script <-
                  (fun sql ->
                    if sql = update_user then (
                      incr n ;
                      Some (counts (if !n = 1 then 1L else 0L) 0L) )
                    else None ) )
              (fun _ db pool ->
                let u () = Db.exec pool (User.update ~name:"Z" ~age:None ann) in
                let a = u () in
                let b = u () in
                let before = db.D.commands in
                let c = Db.exec pool (User.update ann) in
                (a, text b, c, db.D.commands - before) )
          in
          let a, b, c, commands = r in
          equal bool true (a = Ok {ann with name= "Z"; age= None}) ;
          equal string "Not_found" b ;
          equal bool true (c = Ok ann) ;
          equal int 0 commands ;
          equal (list string)
            [ "connect 10.0.0.9:3306 ok"
            ; "query \"UPDATE `user` SET `name` = ?, `age` = ? WHERE \
               `user`.`id` = ?\" done 1"
            ; "query \"UPDATE `user` SET `name` = ?, `age` = ? WHERE \
               `user`.`id` = ?\" failed Not_found" ]
            events )
    ; test "update with no field after with_db raises Invalid_argument"
        (fun () ->
          let pool, _ = with_pool (fun _ _ pool -> pool) in
          raises (Invalid_argument "Db.t used after with_db") (fun () ->
              Db.exec pool (User.update ann) ) )
    ; test "delete: 1 row, and 0" (fun () ->
          let r, _ =
            with_pool
              ~setup:(fun db ->
                let n = ref 0 in
                db.D.faults.script <-
                  (fun sql ->
                    if sql = delete_user then (
                      incr n ;
                      Some (counts (if !n = 1 then 1L else 0L) 0L) )
                    else None ) )
              (fun _ _ pool ->
                let a = Db.exec pool (User.delete ann) in
                (a, text (Db.exec pool (User.delete ann))) )
          in
          equal bool true (fst r = Ok ()) ;
          equal string "Not_found" (snd r) )
    ; test "a row that does not decode, then a statement on the connection"
        (fun () ->
          let r, _ =
            with_pool ~size:1
              ~setup:
                (script
                   [ ( get7
                     , D.Rows
                         [ [ Some "x"
                           ; Some "Ann"
                           ; Some "a@x"
                           ; None
                           ; None
                           ; Some "1" ] ] )
                   ; ( user_cols ^ " WHERE `user`.`id` = 8 LIMIT 1"
                     , D.Rows [row7] ) ] )
              (fun _ db pool ->
                let a = text (Db.exec pool (User.get_by_id 7L)) in
                let b = Db.exec pool (User.get_by_id 8L) in
                (a, b, db.D.connects) )
          in
          let a, b, connects = r in
          equal string "Decode: column 0: expected an integer" a ;
          equal bool true (b = Ok ann) ;
          equal int 1 connects )
    ; test "a value with no literal: no byte sent" (fun () ->
          let r, events =
            with_pool (fun _ db pool ->
                let k =
                  Kinds.Cistern_model__kinds.of_draft
                    (Kinds.Kinds.draft ~id:1L ~i:1 ~s:"" ~b:true ~f:0.
                       ~y:Bytes.empty
                       ~t:(Option.get (Ptime.of_float_s 0.))
                       () )
                    None
                in
                let a =
                  Db.exec pool
                    (Kinds.Kinds.update
                       [Cistern.Update.set Kinds.Kinds.Q.f Float.nan]
                       k )
                in
                (text a, db.D.commands, db.D.connects) )
          in
          let a, commands, connects = r in
          equal string "Invalid_value: a NaN is not a SQL value" a ;
          equal int 0 commands ;
          equal int 0 connects ;
          equal (list string)
            [ "query \"<Invalid_value: a NaN is not a SQL value>\" failed \
               Invalid_value" ]
            events )
    ; test "a lost reply: a read gives Connection_lost, a write Outcome_unknown"
        (fun () ->
          let r, _ =
            with_pool
              ~setup:(script [(get7, D.No_reply); (insert_user, D.No_reply)])
              (fun _ _ pool ->
                ( text (Db.exec pool (User.get_by_id 7L))
                , text (Db.exec pool (User.create ~name:"Bob" ~email:"b@" ()))
                ) )
          in
          equal string "Connection_lost" (fst r) ;
          equal string "Outcome_unknown statement" (snd r) )
    ; test "no session reset after a typed statement" (fun () ->
          let r, _ =
            with_pool ~size:1
              ~setup:
                (script [(get7, D.Rows [row7]); (delete_user, counts 1L 0L)])
              (fun _ db pool ->
                ignore (Db.exec pool (User.get_by_id 7L)) ;
                ignore (Db.exec pool (User.delete ann)) ;
                ignore (Db.exec pool (User.get_by_id 7L)) ;
                (db.D.resets, db.D.connects) )
          in
          equal bool true (r = (0, 1)) )
    ; test "in a Tx: each operation, and Not_found keeps the Tx" (fun () ->
          let r, events =
            with_pool
              ~setup:(fun db ->
                let n = ref 0 in
                db.D.faults.script <-
                  (fun sql ->
                    if sql = get7 then Some (D.Rows [row7])
                    else if sql = user_cols ^ " WHERE `user`.`id` = 8 LIMIT 1"
                    then Some (D.Rows [])
                    else if sql = insert_user then Some (counts 1L 12L)
                    else if sql = update_user then Some (counts 1L 0L)
                    else if sql = delete_user then (
                      incr n ;
                      Some (counts (if !n = 1 then 1L else 0L) 0L) )
                    else None ) )
              (fun _ db pool ->
                let r =
                  Db.transaction pool (fun tx ->
                      let a = Tx.exec tx (User.get_by_id 7L) in
                      let b = text (Tx.exec tx (User.get_by_id 8L)) in
                      let c =
                        Tx.exec tx (User.create ~name:"Bob" ~email:"b@" ())
                      in
                      let d =
                        Tx.exec tx (User.update ~name:"Z" ~age:None ann)
                      in
                      let e = Tx.exec tx (User.update ann) in
                      let f = Tx.exec tx (User.delete ann) in
                      let g = text (Tx.exec tx (User.delete ann)) in
                      ( a = Ok ann
                      , b
                      , Result.map (fun (u : User_todo.user) -> u.id) c
                      , d = Ok {ann with name= "Z"; age= None}
                      , e = Ok ann
                      , f = Ok ()
                      , g ) )
                in
                (r, List.rev db.D.log) )
          in
          let r, log = r in
          ( match r with
          | Ok (a, b, c, d, e, f, g) ->
              equal bool true a ;
              equal string "Not_found" b ;
              equal bool true (c = Ok 12L) ;
              equal bool true d ;
              equal bool true e ;
              equal bool true f ;
              equal string "Not_found" g
          | Error e ->
              fail (E.to_string e) ) ;
          equal bool true (List.mem "COMMIT" log) ;
          (* The update with no field has no event. *)
          equal (list string) []
            (List.filter (String.starts_with ~prefix:"query \"\"") events) )
    ; test "in a failed Tx, update with no field gives the first error"
        (fun () ->
          let inner = ref "" in
          let r, _ =
            with_pool (fun _ _ pool ->
                Db.transaction pool (fun tx ->
                    ignore (Tx.exec tx (Test_pool.q "DEADLOCK")) ;
                    inner := text (Tx.exec tx (User.update ann)) ) )
          in
          equal string "Retryable deadlock" !inner ;
          equal bool true (r = Error (E.Retryable `Deadlock)) )
    ; test "map of an update with no field runs at the exec" (fun () ->
          let calls = ref 0 in
          let r, _ =
            with_pool (fun _ _ pool ->
                let s =
                  Cistern.Stmt.map
                    (fun (u : User_todo.user) -> incr calls ; u.name)
                    (User.update ann)
                in
                let before = !calls in
                let v = Db.exec pool s in
                (before, v, !calls) )
          in
          equal bool true (r = (0, Ok "Ann", 1)) )
    ; test
        "an update with no field: Db.exec in a Tx, Tx.exec from another fiber \
         and after the end" (fun () ->
          let raised f =
            match f () with
            | _ ->
                "no error"
            | exception Invalid_argument m ->
                m
          in
          let r, _ =
            with_pool (fun _ _ pool ->
                let saved = ref None and child = ref "" and in_tx = ref "" in
                let _ =
                  Db.transaction pool (fun tx ->
                      saved := Some tx ;
                      in_tx := raised (fun () -> Db.exec pool (User.update ann)) ;
                      Weft.Fiber.scope (fun s ->
                          Weft.Fiber.fork s (fun () ->
                              child :=
                                raised (fun () -> Tx.exec tx (User.update ann)) ) ) )
                in
                let after =
                  raised (fun () ->
                      Tx.exec (Option.get !saved) (User.update ann) )
                in
                [!in_tx; !child; after] )
          in
          equal (list string)
            [ "Db.exec in the fiber of a Tx"
            ; "Tx.exec from another fiber than the fiber of the Tx"
            ; "Tx.exec after the end of the transaction" ]
            r )
    ; test "an ERR after some rows: the error, no decode, the connection stays"
        (fun () ->
          let decoded = ref 0 in
          let stmt =
            Cistern.Stmt.Private.make
              { redacted= (fun _ -> ["ROWS"])
              ; kind= Cistern.Stmt.Private.Read
              ; raw= None
              ; sql= (fun _ -> ["ROWS"])
              ; text= None }
              ~run:(fun ctx ->
                Cistern.Private.Exec.rows ctx (Cistern.Raw.sql "ROWS") (fun _ ->
                    incr decoded ; Ok () ) )
              ~rows:List.length ~finish:List.length
          in
          let r, _ =
            with_pool ~size:1
              ~setup:
                (script
                   [ ("ROWS", D.Rows_err ([[Some "1"]; [Some "2"]], 1317))
                   ; (get7, D.Rows [row7]) ] )
              (fun _ db pool ->
                let a = text (Db.exec pool stmt) in
                let b = Db.exec pool (User.get_by_id 7L) in
                (a, b, db.D.connects) )
          in
          let a, b, connects = r in
          equal string "Driver 1317 28000: scripted" a ;
          equal int 0 !decoded ;
          equal bool true (b = Ok ann) ;
          equal int 1 connects ) ]
