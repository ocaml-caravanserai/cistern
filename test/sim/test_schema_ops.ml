(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* Db.push_schema and Db.check_schema (phase 2e) on Fake_db, with the
   hand-written models. *)

open Windtrap
open Weft
open Cistern_test_models_ppx
module D = Fake_db
module Db = Cistern.Db
module Tx = Cistern.Tx
module E = Cistern_core.Error
module Ddl = Cistern_sql.Ddl

let with_pool = Test_pool.with_pool

let models = [User_todo.User.model; User_todo.Todo.model]

let schemas =
  [ User_todo.Cistern_fields__user.User.Schema.cistern__model
  ; User_todo.Cistern_fields__user.Todo.Schema.cistern__model ]

let ddl = List.map (Ddl.create_table Mysql schemas) schemas

let set n =
  Printf.sprintf "SET foreign_key_checks = 0, lock_wait_timeout = %d" n

(* The outcome of a call, as text. *)
let outcome f =
  match f () with
  | () ->
      "ok"
  | exception E.Cistern_error e ->
      "Cistern_error " ^ E.to_string e
  | exception Invalid_argument m ->
      "Invalid_argument " ^ m

(* A user model with an uppercase table name, which Schema.check refuses. *)
module Upper = struct
  include User_todo.Cistern_model__user

  let schema = {schema with table= "User"}
end

let script pairs db = db.D.faults.script <- (fun sql -> List.assoc_opt sql pairs)

let query sql outcome = Printf.sprintf "query %S %s" sql outcome

let tests =
  group "push_schema and check_schema"
    [ test "push_schema: the statements in order, on its own connection"
        (fun () ->
          let r, events =
            with_pool (fun _ db pool ->
                Db.push_schema pool models ;
                (* The server reads COM_QUIT in its own fiber. *)
                Time.sleep Mono (Span.ms 10) ;
                (List.rev db.D.log, db.D.connects, db.D.quits, db.D.identities) )
          in
          let log, connects, quits, identities = r in
          (* The log has "" for COM_QUIT. *)
          equal (list string) ((set 29 :: ddl) @ [""]) log ;
          equal int 1 connects ;
          equal int 1 quits ;
          equal int 0 identities ;
          equal (list string)
            ( "connect 10.0.0.9:3306 ok"
            :: query (set 29) "done 1"
            :: List.map (fun s -> query s "done 1") ddl )
            events )
    ; test "push_schema of derived models: the DDL of the ppx" (fun () ->
          let derived =
            Cistern_test_models_ppx.
              [ Person.Person.model
              ; Hostile.T.model
              ; Hostile.S.model
              ; Hostile.F.model ]
          in
          let r, _ =
            with_pool (fun _ db pool ->
                Db.push_schema pool derived ;
                List.tl (List.rev db.D.log) )
          in
          equal (list string)
            [ "CREATE TABLE IF NOT EXISTS `people` (`id` BIGINT NOT NULL \
               AUTO_INCREMENT, `email` VARCHAR(255) NOT NULL, `nick_name` \
               VARCHAR(255) NULL, `city` VARCHAR(255) NOT NULL, `age` BIGINT \
               NULL, `active` TINYINT(1) NOT NULL, PRIMARY KEY (`id`), UNIQUE \
               KEY `people_email_key` (`email`), UNIQUE KEY \
               `people_nick_name_key` (`nick_name`), KEY `people_city_idx` \
               (`city`)) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 \
               COLLATE=utf8mb4_0900_bin ROW_FORMAT=DYNAMIC"
            ; "CREATE TABLE IF NOT EXISTS `order` (`c` BIGINT NOT NULL \
               AUTO_INCREMENT, `select` VARCHAR(255) NOT NULL, `key` BIGINT \
               NOT NULL, `desc` VARCHAR(255) NULL, `a b` MEDIUMTEXT NOT NULL, \
               `it's` TINYINT(1) NOT NULL, PRIMARY KEY (`c`), UNIQUE KEY \
               `order_select_key` (`select`), KEY `order_desc_idx` (`desc`)) \
               ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_bin \
               ROW_FORMAT=DYNAMIC"
            ; "CREATE TABLE IF NOT EXISTS `s` (`draft` BIGINT NOT NULL, \
               `q\"uote\\back` VARCHAR(255) NOT NULL, `name` MEDIUMTEXT NOT \
               NULL, `s_name` VARCHAR(255) NULL, `caf\xc3\xa9` BIGINT NOT \
               NULL, PRIMARY KEY (`draft`), UNIQUE KEY `s_q\"uote\\back_key` \
               (`q\"uote\\back`), UNIQUE KEY `s_s_name_key` (`s_name`), KEY \
               `s_caf\xc3\xa9_idx_7f4276cb` (`caf\xc3\xa9`)) ENGINE=InnoDB \
               DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_bin \
               ROW_FORMAT=DYNAMIC"
            ; "CREATE TABLE IF NOT EXISTS `f` (`update` VARCHAR(255) NOT NULL, \
               `cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc` \
               BIGINT NOT NULL, `all` BIGINT NULL, PRIMARY KEY (`update`), \
               UNIQUE KEY \
               `f_ccccccccccccccccccccccccccccccccccccccccccccccccc_key_2cefbc4e` \
               (`cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc`)) \
               ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_bin \
               ROW_FORMAT=DYNAMIC" ]
            r )
    ; test "push_schema with kill=true: no round trip 3 on its connection"
        (fun () ->
          let r, _ =
            with_pool ~kill:true (fun _ db pool ->
                (* The killer opens its control connection first. *)
                Time.sleep Mono (Span.ms 50) ;
                let before = (db.D.connects, db.D.identities) in
                Db.push_schema pool models ;
                (before, (db.D.connects, db.D.identities)) )
          in
          let (c0, i0), (c1, i1) = r in
          equal int 1 (c1 - c0) ;
          equal int 0 (i1 - i0) )
    ; cases "the value of lock_wait_timeout"
        ~id:(fun (ms, _) -> Printf.sprintf "timeout %d ms" ms)
        [(30000, 29); (2500, 2); (2000, 1); (1200, 1); (800, 1)]
        (fun (ms, n) ->
          let r, _ =
            with_pool (fun _ db pool ->
                Db.push_schema ~timeout:(Span.ms ms) pool models ;
                List.hd (List.rev db.D.log) )
          in
          equal string (set n) r )
    ; test "a refused schema: Invalid_argument, no connect" (fun () ->
          let r, events =
            with_pool (fun _ db pool ->
                let o =
                  outcome (fun () ->
                      Db.push_schema pool [(module Upper); User_todo.Todo.model] )
                in
                (o, db.D.connects) )
          in
          equal string
            "Invalid_argument Db.push_schema: model \"user\": the table \
             \"User\" has an uppercase ASCII letter (version 1 permits only \
             lowercase table names)"
            (fst r) ;
          equal int 0 (snd r) ;
          equal (list string) [] events )
    ; test "an empty list: no connect" (fun () ->
          let r, _ =
            with_pool (fun _ db pool ->
                Db.push_schema pool [] ;
                (db.D.connects, Db.check_schema pool []) )
          in
          equal bool true (r = (0, Ok ())) )
    ; test "a failed connect: Cistern_error, and the Connect event" (fun () ->
          let r, events =
            with_pool
              ~setup:(fun db -> db.D.faults.refuse <- true)
              (fun _ _ pool -> outcome (fun () -> Db.push_schema pool models))
          in
          equal bool true (String.starts_with ~prefix:"Cistern_error Connect" r) ;
          equal int 1 (List.length events) ;
          equal bool true
            (String.starts_with ~prefix:"connect 10.0.0.9:3306 Connect"
               (List.hd events) ) )
    ; test "a failed statement: Cistern_error, the abort, no more statement"
        (fun () ->
          let r, events =
            with_pool
              ~setup:(script [(List.hd ddl, D.Err 1064)])
              (fun _ db pool ->
                let o = outcome (fun () -> Db.push_schema pool models) in
                (o, List.length db.D.log, db.D.quits) )
          in
          equal bool true
            (r = ("Cistern_error Driver 1064 28000: scripted", 2, 0)) ;
          equal (list string)
            [ "connect 10.0.0.9:3306 ok"
            ; query (set 29) "done 1"
            ; query (List.hd ddl) "failed Driver 1064 28000" ]
            events )
    ; test "1205: Cistern_error Retryable lock_timeout" (fun () ->
          let r, _ =
            with_pool
              ~setup:(script [(List.nth ddl 1, D.Err 1205)])
              (fun _ _ pool -> outcome (fun () -> Db.push_schema pool models))
          in
          equal string "Cistern_error Retryable lock timeout" r )
    ; test "the deadline in a statement: Cistern_error Timeout" (fun () ->
          let r, events =
            with_pool
              ~setup:
                (script
                   [ ( List.hd ddl
                     , D.Delay (5000, D.Counts {affected= 0L; insert= 0L}) ) ] )
              (fun _ _ pool ->
                let t0 = Time.now Mono in
                let o =
                  outcome (fun () ->
                      Db.push_schema ~timeout:(Span.s 2) pool models )
                in
                (o, Test_pool.seconds t0) )
          in
          let o, s = r in
          equal string "Cistern_error Timeout" o ;
          equal bool true (s >= 2.0 && s < 2.1) ;
          equal string
            (query (List.hd ddl) "failed Timeout")
            (List.nth events 2) )
    ; test "the margin: no statement after the deadline" (fun () ->
          let r, events =
            with_pool
              ~setup:
                (script
                   [ ( List.hd ddl
                     , D.Delay (1999, D.Counts {affected= 0L; insert= 0L}) ) ] )
              (fun _ db pool ->
                let o =
                  outcome (fun () ->
                      Db.push_schema ~timeout:(Span.s 2) pool models )
                in
                (o, List.length db.D.log) )
          in
          equal bool true (r = ("Cistern_error Timeout", 2)) ;
          equal string
            (query (List.nth ddl 1) "failed Timeout")
            (List.nth events 3) )
    ; test "a cancel in a statement: the event, the abort, the cancel goes up"
        (fun () ->
          let r, events =
            with_pool
              ~setup:
                (script
                   [ ( List.hd ddl
                     , D.Delay (5000, D.Counts {affected= 0L; insert= 0L}) ) ] )
              (fun _ db pool ->
                let o =
                  match
                    Fiber.with_timeout Mono (Span.ms 300) (fun () ->
                        Db.push_schema pool models )
                  with
                  | () ->
                      "no cancel"
                  | exception Fiber.Timeout ->
                      "Timeout"
                in
                (o, db.D.quits) )
          in
          equal bool true (r = ("Timeout", 0)) ;
          equal string
            (query (List.hd ddl) "cancelled sent=true")
            (List.nth events 2) )
    ; test "the deadline in the connect: Cistern_error Timeout" (fun () ->
          let r, events =
            with_pool
              ~setup:(fun db -> db.D.faults.hang_greeting <- true)
              (fun _ _ pool ->
                outcome (fun () ->
                    Db.push_schema ~timeout:(Span.ms 500) pool models ) )
          in
          equal string "Cistern_error Timeout" r ;
          equal (list string) ["connect 10.0.0.9:3306 Timeout"] events )
    ; test "a failed SET: the abort, no CREATE" (fun () ->
          let r, _ =
            with_pool
              ~setup:(script [(set 29, D.Err 1064)])
              (fun _ db pool ->
                let o = outcome (fun () -> Db.push_schema pool models) in
                Time.sleep Mono (Span.ms 10) ;
                (o, List.rev db.D.log, db.D.quits) )
          in
          equal bool true
            (r = ("Cistern_error Driver 1064 28000: scripted", [set 29], 0)) )
    ; test "a refused schema after with_db: the message of the use after"
        (fun () ->
          let pool, _ = with_pool (fun _ _ pool -> pool) in
          equal string "Invalid_argument Db.t used after with_db"
            (outcome (fun () -> Db.push_schema pool [(module Upper)])) )
    ; test "lock_wait_timeout from the time left after a slow connect"
        (fun () ->
          let r, _ =
            with_pool
              ~setup:(fun db -> db.D.faults.greeting_ms <- 3000)
              (fun _ db pool ->
                Db.push_schema ~timeout:(Span.s 10) pool models ;
                List.hd (List.rev db.D.log) )
          in
          equal string (set 6) r )
    ; test "in the fiber of a Tx, and after with_db: Invalid_argument"
        (fun () ->
          let r, _ =
            with_pool (fun _ _ pool ->
                let inner = ref [] in
                let _ =
                  Db.transaction pool (fun _ ->
                      inner :=
                        [ outcome (fun () -> Db.push_schema pool models)
                        ; outcome (fun () ->
                              ignore (Db.check_schema pool models) ) ] )
                in
                (!inner, pool) )
          in
          let inner, pool = r in
          equal (list string)
            [ "Invalid_argument Db.push_schema in the fiber of a Tx"
            ; "Invalid_argument Db.check_schema in the fiber of a Tx" ]
            inner ;
          equal (list string)
            [ "Invalid_argument Db.t used after with_db"
            ; "Invalid_argument Db.t used after with_db" ]
            [ outcome (fun () -> Db.push_schema pool models)
            ; outcome (fun () -> ignore (Db.check_schema pool models)) ] ) ]

(* check_schema *)

let check_sql =
  "SELECT TABLE_NAME, COLUMN_NAME, DATA_TYPE, IS_NULLABLE, \
   CHARACTER_MAXIMUM_LENGTH, DATETIME_PRECISION, COLLATION_NAME, COLUMN_TYPE \
   LIKE '%unsigned%', EXTRA LIKE '%auto_increment%' FROM \
   information_schema.COLUMNS WHERE TABLE_SCHEMA = DATABASE() AND TABLE_NAME \
   IN ("

let check_full =
  check_sql
  ^ " _utf8mb4 X'75736572' ,  _utf8mb4 X'746F646F' ) ORDER BY TABLE_NAME, \
     ORDINAL_POSITION"

let coll = Some "utf8mb4_0900_bin"

let nopad = Some "utf8mb4_nopad_bin"

let col ?(null = "NO") ?len ?(c = None) ?(auto = "0") ?(unsigned = "0") table
    name typ =
  [ Some table
  ; Some name
  ; Some typ
  ; Some null
  ; len
  ; None
  ; c
  ; Some unsigned
  ; Some auto ]

let rows_with coll =
  [ col "todo" "id" "bigint" ~auto:"1"
  ; col "todo" "user_id" "bigint"
  ; col "todo" "title" "varchar" ~len:"255" ~c:coll
  ; col "todo" "done" "tinyint"
  ; col "todo" "note" "mediumtext" ~null:"YES" ~len:"16777215" ~c:coll
  ; col "user" "id" "bigint" ~auto:"1"
  ; col "user" "name" "mediumtext" ~len:"16777215" ~c:coll
  ; col "user" "email" "varchar" ~len:"255" ~c:coll
  ; col "user" "nick" "varchar" ~null:"YES" ~len:"255" ~c:coll
  ; col "user" "age" "bigint" ~null:"YES"
  ; col "user" "active" "tinyint" ]

let rows = rows_with coll

let check_tests =
  group "check_schema"
    [ test "equal: the exact SQL, and the redacted event" (fun () ->
          let r, events =
            with_pool
              ~setup:(script [(check_full, D.Rows rows)])
              (fun _ db pool -> (Db.check_schema pool models, db.D.resets))
          in
          ( match r with
          | Ok (), n ->
              equal int 0 n
          | Error ms, _ ->
              fail (String.concat " | " ms) ) ;
          equal (list string)
            [ "connect 10.0.0.9:3306 ok"
            ; query
                (check_sql ^ " ? ,  ? ) ORDER BY TABLE_NAME, ORDINAL_POSITION")
                "done 11" ]
            events )
    ; test "the differences" (fun () ->
          let changed =
            List.filter_map
              (fun r ->
                match r with
                | [_; Some "note"; _; _; _; _; _; _; _] ->
                    None
                | [t; Some "name"; _; n; _; p; _; u; a] ->
                    Some
                      [ t
                      ; Some "name"
                      ; Some "varchar"
                      ; n
                      ; Some "50"
                      ; p
                      ; coll
                      ; u
                      ; a ]
                | [t; Some "id"; d; n; l; p; c; u; _] when t = Some "user" ->
                    Some [t; Some "id"; d; n; l; p; c; u; Some "0"]
                | r ->
                    Some r )
              rows
          in
          let r, _ =
            with_pool
              ~setup:(script [(check_full, D.Rows changed)])
              (fun _ _ pool -> Db.check_schema pool models)
          in
          equal
            (result unit (list string))
            (Error
               [ "\"user\".\"id\": the column has no AUTO_INCREMENT, the model \
                  needs it ([@auto])"
               ; "\"user\".\"name\": the type is \"varchar\", the model needs \
                  mediumtext"
               ; "the table \"todo\" has no column \"note\"" ] )
            r )
    ; test "a refused schema: the messages, no statement" (fun () ->
          let r, _ =
            with_pool (fun _ db pool ->
                ( Db.check_schema pool [(module Upper); User_todo.Todo.model]
                , db.D.connects ) )
          in
          equal bool true
            ( r
            = ( Error
                  [ "model \"user\": the table \"User\" has an uppercase ASCII \
                     letter (version 1 permits only lowercase table names)" ]
              , 0 ) ) )
    ; test "MariaDB: its collation, and the collation of MySQL as differences"
        (fun () ->
          let r, _ =
            with_pool
              ~setup:(fun db ->
                db.D.faults.version <- "11.8.8-MariaDB" ;
                let n = ref 0 in
                db.D.faults.script <-
                  (fun sql ->
                    if sql = check_full then (
                      incr n ;
                      Some (D.Rows (rows_with (if !n = 1 then nopad else coll)))
                      )
                    else None ) )
              (fun _ _ pool ->
                let a = Db.check_schema pool models in
                (a, Db.check_schema pool models) )
          in
          let msg c =
            Printf.sprintf
              "%s: the collation is \"utf8mb4_0900_bin\", the model needs \
               utf8mb4_nopad_bin"
              c
          in
          equal (result unit (list string)) (Ok ()) (fst r) ;
          equal
            (result unit (list string))
            (Error
               [ msg "\"user\".\"name\""
               ; msg "\"user\".\"email\""
               ; msg "\"user\".\"nick\""
               ; msg "\"todo\".\"title\""
               ; msg "\"todo\".\"note\"" ] )
            (snd r) )
    ; test "no session reset after check_schema" (fun () ->
          let r, _ =
            with_pool ~size:1
              ~setup:(script [(check_full, D.Rows rows)])
              (fun _ db pool ->
                ignore (Db.check_schema pool models) ;
                ignore (Db.check_schema pool models) ;
                (db.D.resets, db.D.connects) )
          in
          equal bool true (r = (0, 1)) )
    ; test "a server error: Cistern_error, not a message" (fun () ->
          let r, _ =
            with_pool
              ~setup:(script [(check_full, D.Err 1142)])
              (fun _ _ pool ->
                outcome (fun () -> ignore (Db.check_schema pool models)) )
          in
          equal string "Cistern_error Driver 1142 28000: scripted" r ) ]

let tests = group "the schema" [tests; check_tests]
