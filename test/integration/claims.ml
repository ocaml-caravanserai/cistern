(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* The claims of DESIGN.md, "Verification of claims (phase 1.1)", on the real
   servers, with Cistern's own client (the plan of phase 1.1, section 7). Each
   test names its claim. The group [deterministic] runs in each integration job.
   The groups [timing] (C8a, C8b, C9, C34) and [shutdown] (C8c, which stops the
   server) run only in the nightly db.yml.

   A test asserts what the server gives today: the error number and SQLSTATE of
   the server (Connection.Private.last_error), and the meaning that Cistern
   gives to them. *)

open Windtrap
open Support
module Value = Cistern_core.Value
module Message = Cistern_mysql.Message

let sql = Raw.sql

(* The result of a command: "ok", or the number and the SQLSTATE of the error
   packet (if one came) and the meaning. *)
let outcome c = function
  | Ok _ ->
      "ok"
  | Error e -> (
    match Conn.Private.last_error c with
    | Some p ->
        Printf.sprintf "%d %s: %s" p.code p.sqlstate (error_text e)
    | None ->
        error_text e )

let exec c s = outcome c (Conn.exec c s)

let rows c s = rows_text (Conn.query c s)

(* The first value of the first row of [s]. *)
let one c s =
  match Conn.query c s with
  | Ok {rows= row :: _; _} ->
      row.(0)
  | Ok _ ->
      failwith "no row"
  | Error e ->
      failwith (error_text e)

let one_text c s = value_text (one c s)

let mariadb = not mysql

(* A test that runs only on the servers of [only], a list of the names of the
   servers, else passes at once. *)
let on only name f =
  test name (fun () ->
      if List.mem server only then in_weft f
      else skip ~reason:("a claim of " ^ String.concat " and " only) () )

(* Phase 1.4: each claim of MySQL 8.4 and MariaDB 11.8 also runs on MariaDB 11.4
   (it holds there: the probe T10 of the plan of phase 1.4). *)
let both = ["mysql84"; "mariadb114"; "mariadb118"]

let all = ["mysql84"; "mariadb114"; "mariadb118"]

let user_conn () = on_socket ~clear:true ()

(* A table of the test user, made again for each test. *)
let table c name ddl =
  exec_ok c ("DROP TABLE IF EXISTS " ^ name) ;
  exec_ok c ("CREATE TABLE " ^ name ^ " " ^ ddl)

let in_trans c = Conn.status c land Message.status_bit In_trans <> 0

(* The greeting of the server on the socket, read with the packet layer. *)
let greeting () =
  let flow = Weft_flow.Tcp.connect (Weft.Net.Unix socket) in
  Fun.protect
    ~finally:(fun () -> Weft_flow.Flow.close flow)
    (fun () ->
      match
        Cistern_mysql.Packet.read (Cistern_mysql.Packet.reader flow) ~seq:0
      with
      | Error _ ->
          failwith "no greeting"
      | Ok p ->
          Result.get_ok (Message.greeting p) )

(* The deterministic claims. *)

let c1 =
  on all "C1, C7: the default plugins, CLIENT_MYSQL and the dialect" (fun () ->
      let g = greeting () in
      equal string
        (if mysql then "caching_sha2_password" else "mysql_native_password")
        g.auth_plugin ;
      equal bool mysql (Message.has g.capabilities Client_mysql) ;
      warm () ;
      with_conn (user_conn ()) (fun c ->
          equal string
            (if mysql then "mysql" else "mariadb")
            (dialect_text (Conn.info c)) ) ;
      with_conn (root ()) (fun c ->
          exec_ok c "DROP USER IF EXISTS 'c1'@'localhost'" ;
          exec_ok c "CREATE USER 'c1'@'localhost' IDENTIFIED BY 'c1-password'" ;
          let plugin =
            one_text c (sql "SELECT plugin FROM mysql.user WHERE user = 'c1'")
          in
          exec_ok c "DROP USER 'c1'@'localhost'" ;
          equal string
            ( if mysql then "Text \"caching_sha2_password\""
              else "Text \"mysql_native_password\"" )
            plugin ) )

let c2 =
  on both "C2: START TRANSACTION in a transaction commits" (fun () ->
      warm () ;
      with_conn (user_conn ()) (fun c ->
          table c "c2" "(id INT PRIMARY KEY) ENGINE=InnoDB" ;
          exec_ok c "START TRANSACTION" ;
          exec_ok c "INSERT INTO c2 VALUES (1)" ;
          exec_ok c "START TRANSACTION" ;
          exec_ok c "ROLLBACK" ;
          equal string "Int 1" (one_text c (sql "SELECT COUNT(*) FROM c2")) ;
          exec_ok c "DROP TABLE c2" ) )

let c3 =
  on all "C3: COM_RESET_CONNECTION keeps the database" (fun () ->
      warm () ;
      with_conn (user_conn ()) (fun c ->
          table c "c3" "(id INT PRIMARY KEY) ENGINE=InnoDB" ;
          exec_ok c "SET @v = 1" ;
          exec_ok c "USE information_schema" ;
          equal string "ok" (unit_text (Conn.Private.reset_connection c)) ;
          (* The collation: MySQL gives the global value. MariaDB gives the
             collation of the character set of the handshake (45, utf8mb4):
             utf8mb4_general_ci on 11.4 and utf8mb4_uca1400_ai_ci on 11.8. It is
             never the collation of Cistern. *)
          let collation =
            match server with
            | "mysql84" ->
                "utf8mb4_0900_ai_ci"
            | "mariadb114" ->
                "utf8mb4_general_ci"
            | _ ->
                "utf8mb4_uca1400_ai_ci"
          in
          equal string
            ( "d,v,m,c | Text \"information_schema\", Int 1, Int 1, Text \""
            ^ collation ^ "\"" )
            (rows c
               (sql
                  "SELECT DATABASE() AS d, @v IS NULL AS v, @@session.sql_mode \
                   = @@global.sql_mode AS m, @@collation_connection AS c" ) ) ;
          (* A reset in a transaction rolls it back. *)
          equal string "ok" (unit_text (Conn.init_db c database)) ;
          exec_ok c "START TRANSACTION" ;
          exec_ok c "INSERT INTO c3 VALUES (1)" ;
          is_true (in_trans c) ;
          equal string "ok" (unit_text (Conn.Private.reset_connection c)) ;
          is_false (in_trans c) ;
          equal string "Int 0" (one_text c (sql "SELECT COUNT(*) FROM c3")) ;
          exec_ok c "DROP TABLE c3" ) )

let c4 =
  on all "C4: IN_TRANS in the OK of COM_PING" (fun () ->
      warm () ;
      with_conn (user_conn ()) (fun c ->
          table c "c4" "(id INT PRIMARY KEY) ENGINE=InnoDB" ;
          let ping () =
            equal string "ok" (unit_text (Conn.ping c)) ;
            in_trans c
          in
          exec_ok c "START TRANSACTION" ;
          is_true (ping ()) ;
          exec_ok c "INSERT INTO c4 VALUES (1)" ;
          is_true (ping ()) ;
          exec_ok c "COMMIT" ;
          is_false (ping ()) ;
          exec_ok c "DROP TABLE c4" ) )

let identity_sql () =
  if mysql then
    "SELECT @@server_uuid, UNIX_TIMESTAMP() - (SELECT VARIABLE_VALUE FROM \
     performance_schema.global_status WHERE VARIABLE_NAME = 'Uptime')"
  else
    "SELECT UNIX_TIMESTAMP() - (SELECT VARIABLE_VALUE FROM \
     information_schema.GLOBAL_STATUS WHERE VARIABLE_NAME = 'UPTIME')"

let c5 =
  on all "C5, C23: one start time, and global_status for the test user"
    (fun () ->
      warm () ;
      let values = Hashtbl.create 1 in
      let read c = Hashtbl.replace values (rows c (sql (identity_sql ()))) () in
      with_conn (user_conn ()) (fun c ->
          for _ = 1 to 1000 do
            read c
          done ) ;
      with_conn (user_conn ()) read ;
      equal int 1 (Hashtbl.length values) )

let c6 =
  on all "C6: the thread id of the greeting and CONNECTION_ID()" (fun () ->
      warm () ;
      List.iter
        (fun config ->
          with_conn config (fun c ->
              let i = Conn.info c in
              equal int64 i.connection_id (Int64.of_int i.thread_id) ;
              equal string
                (Printf.sprintf "Int %Ld" i.connection_id)
                (one_text c (sql "SELECT CONNECTION_ID()")) ) )
        [user_conn (); on_tcp ()] )

(* C10: seeded random doubles and the edge values. *)
let doubles () =
  let st = Random.State.make [|10|] in
  let edges =
    [ 0.0
    ; -0.0
    ; 1.0
    ; 0.1
    ; 1e15
    ; 1e14
    ; 123456789012345.6
    ; 1e-4
    ; 1e-5
    ; 5e-324
    ; Float.min_float
    ; Float.max_float
    ; -.Float.max_float
    ; Float.epsilon
    ; 2.0 ** 53.0
    ; Float.ldexp 1.0 (-1017) ]
  in
  let rec random acc n =
    if n = 0 then acc
    else
      let f = Int64.float_of_bits (Random.State.bits64 st) in
      if Float.is_finite f then random (f :: acc) (n - 1) else random acc n
  in
  (* 1 to 17 digits at each decimal exponent near the bounds of the positional
     form (-15 and 15). *)
  let bounds =
    List.concat_map
      (fun e ->
        List.init 17 (fun n ->
            float_of_string
              ( String.sub "12345678901234567" 0 (n + 1)
              ^ "e"
              ^ string_of_int (e - n) ) ) )
      (List.init 41 (fun i -> i - 20))
  in
  edges @ bounds @ random [] 2000

let c10 =
  on both "C10: the text forms of DOUBLE" (fun () ->
      warm () ;
      with_conn (user_conn ()) (fun c ->
          table c "c10" "(i INT PRIMARY KEY, x DOUBLE)" ;
          let ds = Array.of_list (doubles ()) in
          let values =
            Array.to_list
              (Array.mapi
                 (fun i f ->
                   Raw.(sql (Printf.sprintf "(%d," i) ++ v (Float f) ++ sql ")") )
                 ds )
          in
          let insert =
            List.fold_left
              (fun acc r -> Raw.(acc ++ sql "," ++ r))
              (Raw.( ++ ) (sql "INSERT INTO c10 VALUES ") (List.hd values))
              (List.tl values)
          in
          equal string "ok" (exec c insert) ;
          let select = sql "SELECT x FROM c10 ORDER BY i" in
          (* MariaDB stores -0 as +0 (D11). *)
          let stored f = if mariadb && f = 0.0 then 0.0 else f in
          (* The values decode to the same bits. *)
          ( match Conn.query c select with
          | Error e ->
              fail (error_text e)
          | Ok r ->
              equal (list int64)
                (Array.to_list
                   (Array.map (fun f -> Int64.bits_of_float (stored f)) ds) )
                (List.map
                   (function
                     | [|Value.Float x|] -> Int64.bits_of_float x | _ -> 0L )
                   r.rows ) ) ;
          (* The text of the server is that of the model. *)
          ( match Conn.Private.query_text c select with
          | Error e ->
              fail (error_text e)
          | Ok rows ->
              equal (list string)
                (Array.to_list
                   (Array.map (fun f -> Server_text.double (stored f)) ds) )
                (List.map
                   (function [|Some t|] -> t | _ -> "a row with no text")
                   rows ) ) ;
          exec_ok c "DROP TABLE c10" ) )

let c11 =
  on all "C11: the literals -0e0, 5e-324 and -9223372036854775808" (fun () ->
      warm () ;
      with_conn (user_conn ()) (fun c ->
          table c "c11" "(i INT PRIMARY KEY, d DOUBLE, b BIGINT)" ;
          equal string "ok"
            (exec c
               (sql
                  "INSERT INTO c11 VALUES (1, -0e0, -9223372036854775808), (2, \
                   5e-324, 0)" ) ) ;
          equal string
            ( if mysql then
                "d,a,b | Float -0x0p+0, Float -0x1.921fb54442d18p+1, Int \
                 -9223372036854775808 / Float 0x0.0000000000001p-1022, Float \
                 0x1.921fb54442d18p+1, Int 0"
              else
                "d,a,b | Float 0x0p+0, Float 0x1.921fb54442d18p+1, Int \
                 -9223372036854775808 / Float 0x0.0000000000001p-1022, Float \
                 0x1.921fb54442d18p+1, Int 0" )
            (rows c (sql "SELECT d, ATAN2(d, -1) AS a, b FROM c11 ORDER BY i")) ;
          equal string
            ( if mysql then "z,t | Float -0x0p+0, Text \"-0\""
              else "z,t | Float 0x0p+0, Text \"0\"" )
            (rows c (sql "SELECT -0e0 AS z, CAST(-0e0 AS CHAR) AS t")) ;
          exec_ok c "DROP TABLE c11" ) )

let c12 =
  on both "C12: an encoded surrogate in a utf8mb4 column" (fun () ->
      warm () ;
      with_conn (user_conn ()) (fun c ->
          table c "c12"
            "(i INT PRIMARY KEY, t VARCHAR(10) CHARACTER SET utf8mb4)" ;
          let ins i lit =
            exec c
              Raw.(
                sql (Printf.sprintf "INSERT INTO c12 VALUES (%d," i)
                ++ lit ++ sql ")" )
          in
          let results =
            [ ins 1 (Raw.v (Value.Bytes "\xed\xa0\x80"))
            ; ins 2 (sql "_binary X'EDA080'")
            ; ins 3 (sql "_utf8mb4 X'EDA080'") ]
          in
          if mysql then
            equal (list string)
              [ "1366 HY000: Invalid_value 1366"
              ; "1366 HY000: Invalid_value 1366"
              ; "1300 HY000: Driver 1300 HY000" ]
              results
          else (
            equal (list string) ["ok"; "ok"; "ok"] results ;
            (* The read gives Decode, and the connection stays usable. *)
            equal string "Decode" (rows c (sql "SELECT t FROM c12")) ;
            equal string "ok" (unit_text (Conn.ping c)) ) ;
          exec_ok c "DROP TABLE c12" ) )

let c13 =
  on both "C13: 1265 and 1365 in strict mode" (fun () ->
      warm () ;
      with_conn (user_conn ()) (fun c ->
          table c "c13" "(e ENUM('a'), n INT)" ;
          equal string "1265 01000: Invalid_value 1265"
            (exec c (sql "INSERT INTO c13 (e) VALUES ('x')")) ;
          equal string "1365 22012: Invalid_value 1365"
            (exec c (sql "INSERT INTO c13 (n) VALUES (1/0)")) ;
          exec_ok c "DROP TABLE c13" ) )

let c14 =
  on both "C14: the read-only codes, with no restart" (fun () ->
      warm () ;
      let set s =
        with_conn (root ()) (fun c -> exec_ok c ("SET GLOBAL " ^ s))
      in
      let writable () =
        if mysql then set "super_read_only = 0" ;
        set "read_only = 0"
      in
      (* A run that stops in the middle can leave the server read-only: the test
         makes the state known first. *)
      writable () ;
      with_conn (user_conn ()) (fun c -> table c "c14" "(id INT)") ;
      let insert () =
        with_conn (user_conn ()) (fun c ->
            exec c (sql "INSERT INTO c14 VALUES (1)") )
      in
      Fun.protect ~finally:writable (fun () ->
          set "read_only = 1" ;
          equal string "1290 HY000: Read_only" (insert ()) ;
          if mysql then (
            set "super_read_only = 1" ;
            equal string "1290 HY000: Read_only" (insert ()) ;
            set "super_read_only = 0" ) ;
          set "read_only = 0" ) ;
      with_conn (user_conn ()) (fun c ->
          exec_ok c "START TRANSACTION READ ONLY" ;
          equal string "1792 25006: Read_only"
            (exec c (sql "INSERT INTO c14 VALUES (1)")) ;
          exec_ok c "ROLLBACK" ;
          exec_ok c "DROP TABLE c14" ) )

let c15 =
  on both "C15: NOWAIT" (fun () ->
      warm () ;
      with_conn (user_conn ()) (fun a ->
          table a "c15" "(id INT PRIMARY KEY) ENGINE=InnoDB" ;
          exec_ok a "INSERT INTO c15 VALUES (1)" ;
          exec_ok a "START TRANSACTION" ;
          ignore (one a (sql "SELECT id FROM c15 WHERE id = 1 FOR UPDATE")) ;
          with_conn (user_conn ()) (fun b ->
              equal string
                ( if mysql then "3572 HY000: Driver 3572 HY000"
                  else "1205 HY000: Retryable lock timeout" )
                (outcome b
                   (Conn.query b
                      (sql "SELECT id FROM c15 WHERE id = 1 FOR UPDATE NOWAIT") ) ) ;
              equal string "ok" (unit_text (Conn.ping b)) ;
              is_false (in_trans b) ) ;
          exec_ok a "ROLLBACK" ;
          exec_ok a "DROP TABLE c15" ) )

let c17 =
  on both "C17: 1052, 1761, 1762, 1169, 1859 and 3673" (fun () ->
      warm () ;
      with_conn (user_conn ()) (fun c ->
          List.iter
            (fun t -> exec_ok c ("DROP TABLE IF EXISTS " ^ t))
            ["c17c"; "c17p"; "c17a"; "c17b"; "c17u"; "c17m"; "c17d"; "c17n"] ;
          (* 1052: an ambiguous column. *)
          exec_ok c "CREATE TABLE c17a (id INT, x INT)" ;
          exec_ok c "CREATE TABLE c17b (id INT, x INT)" ;
          equal string "1052 23000: Driver 1052 23000"
            (outcome c
               (Conn.query c (sql "SELECT x FROM c17a JOIN c17b USING (id)")) ) ;
          (* 1761: ON UPDATE CASCADE into a unique key of the child. The key of
             the parent is not unique. *)
          if mysql then
            exec_ok c "SET SESSION restrict_fk_on_non_standard_key = OFF" ;
          exec_ok c
            "CREATE TABLE c17p (id INT PRIMARY KEY, k INT, KEY (k)) \
             ENGINE=InnoDB" ;
          exec_ok c
            "CREATE TABLE c17c (id INT PRIMARY KEY, pk INT, UNIQUE KEY (pk), \
             FOREIGN KEY (pk) REFERENCES c17p (k) ON UPDATE CASCADE) \
             ENGINE=InnoDB" ;
          exec_ok c "INSERT INTO c17p VALUES (1, 10), (2, 20)" ;
          exec_ok c "INSERT INTO c17c VALUES (1, 10), (2, 20)" ;
          let update = sql "UPDATE c17p SET k = 20 WHERE id = 1" in
          equal string "1761 23000: Constraint_violation Unique" (exec c update) ;
          (* 1762: a user with rights only on the parent. *)
          with_conn (root ()) (fun r ->
              exec_ok r "DROP USER IF EXISTS 'c17'@'localhost'" ;
              exec_ok r "CREATE USER 'c17'@'localhost' IDENTIFIED BY 'c17-pw'" ;
              exec_ok r
                ( "GRANT SELECT, UPDATE ON " ^ database
                ^ ".c17p TO 'c17'@'localhost'" ) ) ;
          Fun.protect
            ~finally:(fun () ->
              with_conn (root ()) (fun r ->
                  exec_ok r "DROP USER IF EXISTS 'c17'@'localhost'" ) )
            (fun () ->
              with_conn
                (on_socket ~user:"c17" ~password:"c17-pw" ~clear:true ())
                (fun u ->
                  if mysql then
                    exec_ok u
                      "SET SESSION restrict_fk_on_non_standard_key = OFF" ;
                  equal string "1761 23000: Constraint_violation Unique"
                    (exec u update) ) ) ;
          (* 1169: a unique key on a BLOB, and a MEMORY unique key. *)
          exec_ok c "CREATE TABLE c17u (b BLOB, UNIQUE KEY (b(10)))" ;
          exec_ok c "INSERT INTO c17u VALUES ('a')" ;
          equal string "1062 23000: Constraint_violation Unique"
            (exec c (sql "INSERT INTO c17u VALUES ('a')")) ;
          exec_ok c "CREATE TABLE c17m (id INT, UNIQUE KEY (id)) ENGINE=MEMORY" ;
          exec_ok c "INSERT INTO c17m VALUES (1)" ;
          equal string "1062 23000: Constraint_violation Unique"
            (exec c (sql "INSERT INTO c17m VALUES (1)")) ;
          (* 1859: ADD UNIQUE with duplicates. *)
          exec_ok c "CREATE TABLE c17d (id INT)" ;
          exec_ok c "INSERT INTO c17d VALUES (1), (1)" ;
          equal string "1062 23000: Constraint_violation Unique"
            (exec c (sql "ALTER TABLE c17d ADD UNIQUE (id)")) ;
          (* 3673: IGNORE gives warnings, and a plain INSERT gives 1048. *)
          exec_ok c "CREATE TABLE c17n (id INT NOT NULL)" ;
          exec_ok c "INSERT INTO c17n VALUES (1)" ;
          equal string "ok"
            (exec c (sql "INSERT IGNORE INTO c17n VALUES (NULL)")) ;
          equal string "ok" (exec c (sql "UPDATE IGNORE c17n SET id = NULL")) ;
          equal string "1048 23000: Constraint_violation Not_null"
            (exec c (sql "INSERT INTO c17n VALUES (NULL)")) ;
          List.iter
            (fun t -> exec_ok c ("DROP TABLE " ^ t))
            ["c17c"; "c17p"; "c17a"; "c17b"; "c17u"; "c17m"; "c17d"; "c17n"] ) )

let c18 =
  on both "C18: the rules of names" (fun () ->
      warm () ;
      with_conn (user_conn ()) (fun c ->
          List.iter
            (fun t -> exec_ok c ("DROP TABLE IF EXISTS " ^ t))
            ["c18a"; "c18b"; "c18c"] ;
          let long = String.make 65 't' in
          equal string
            ( if mysql then "1059 42000: Driver 1059 42000"
              else "1103 42000: Driver 1103 42000" )
            (exec c (sql ("CREATE TABLE " ^ long ^ " (id INT)"))) ;
          let emoji = "a\xf0\x9f\x98\x80" and emoji2 = "a\xf0\x9f\x98\x81" in
          equal string
            (if mysql then "ok" else "1300 HY000: Driver 1300 HY000")
            (exec c (sql ("CREATE TABLE c18a (`" ^ emoji ^ "` INT)"))) ;
          if mysql then
            equal string "1060 42S21: Driver 1060 42S21"
              (exec c
                 (sql
                    ( "CREATE TABLE c18b (`" ^ emoji ^ "` INT, `" ^ emoji2
                    ^ "` INT)" ) ) ) ;
          let euros =
            String.concat "" (List.init 64 (fun _ -> "\xe2\x82\xac"))
          in
          equal string
            ( if mysql then "1030 HY000: Driver 1030 HY000"
              else "1005 HY000: Driver 1005 HY000" )
            (exec c (sql ("CREATE TABLE `" ^ euros ^ "` (id INT)"))) ;
          equal string "ok"
            (exec c (sql ("CREATE TABLE c18c (`" ^ euros ^ "` INT)"))) ;
          List.iter
            (fun t -> exec_ok c ("DROP TABLE IF EXISTS " ^ t))
            ["c18a"; "c18b"; "c18c"] ) )

let c19 =
  on all "C19, C24: no padding, and COM_PING" (fun () ->
      warm () ;
      with_conn (user_conn ()) (fun c ->
          equal string "Int 0" (one_text c (sql "SELECT 'a ' = 'a'")) ;
          equal string "ok" (unit_text (Conn.ping c)) ) )

let c20 =
  on both "C20: PAD_ATTRIBUTE" (fun () ->
      warm () ;
      with_conn (user_conn ()) (fun c ->
          let r =
            outcome c
              (Conn.query c
                 (sql
                    "SELECT PAD_ATTRIBUTE FROM information_schema.COLLATIONS \
                     LIMIT 1" ) )
          in
          equal string
            (if mysql then "ok" else "1054 42S22: Driver 1054 42S22")
            r ) )

let c21 =
  on both "C21: FOR SHARE" (fun () ->
      warm () ;
      with_conn (user_conn ()) (fun c ->
          let q s = outcome c (Conn.query c (sql s)) in
          equal string
            (if mysql then "ok" else "1064 42000: Driver 1064 42000")
            (q "SELECT 1 FOR SHARE") ;
          equal string "ok" (q "SELECT 1 LOCK IN SHARE MODE") ) )

let c22 =
  on both "C22: CHECK and the statement timeout" (fun () ->
      warm () ;
      with_conn (user_conn ()) (fun c ->
          table c "c22" "(n INT, CHECK (n > 0))" ;
          equal string
            ( if mysql then "3819 HY000: Constraint_violation Check"
              else "4025 23000: Constraint_violation Check" )
            (exec c (sql "INSERT INTO c22 VALUES (0)")) ;
          exec_ok c "DROP TABLE c22" ;
          let cte =
            "WITH RECURSIVE r (n) AS (SELECT 1 UNION ALL SELECT n + 1 FROM r \
             WHERE n < 50000000) SELECT COUNT(*) FROM r"
          in
          let r =
            if mysql then (
              exec_ok c "SET SESSION cte_max_recursion_depth = 100000000" ;
              outcome c
                (Conn.query c
                   (sql
                      ( "SELECT /*+ MAX_EXECUTION_TIME(100) */ * FROM (" ^ cte
                      ^ ") AS x" ) ) ) )
            else (
              exec_ok c "SET SESSION max_recursive_iterations = 100000000" ;
              outcome c
                (Conn.query c
                   (sql ("SET STATEMENT max_statement_time = 0.1 FOR " ^ cte)) )
              )
          in
          equal string
            ( if mysql then "3024 HY000: Statement_timeout"
              else "1969 70100: Statement_timeout" )
            r ;
          equal string "ok" (unit_text (Conn.ping c)) ) )

(* C26: a global completion_type = CHAIN chains a new transaction at each COMMIT
   (its OK has IN_TRANS set). The session of Cistern sets NO_CHAIN in round trip
   2 and after a reset. The global value comes back to DEFAULT after the test,
   also if an earlier run stopped with CHAIN. *)
let c26 =
  on all "C26: completion_type = NO_CHAIN in the session" (fun () ->
      warm () ;
      Fun.protect
        ~finally:(fun () ->
          ignore (admin "SET GLOBAL completion_type = DEFAULT") )
        (fun () ->
          ignore (admin "SET GLOBAL completion_type = 'CHAIN'") ;
          with_conn (user_conn ()) (fun c ->
              table c "c26" "(id INT PRIMARY KEY) ENGINE=InnoDB" ;
              let commit_then_flag () =
                exec_ok c "START TRANSACTION" ;
                exec_ok c "INSERT INTO c26 SELECT COUNT(*) FROM c26" ;
                exec_ok c "COMMIT" ;
                in_trans c
              in
              equal string "Text \"NO_CHAIN\""
                (one_text c (sql "SELECT @@session.completion_type")) ;
              is_false (commit_then_flag ()) ;
              (* The reset gives the global CHAIN, then the SET of Cistern. *)
              equal string "ok" (unit_text (Conn.reset c)) ;
              equal string "Text \"NO_CHAIN\""
                (one_text c (sql "SELECT @@session.completion_type")) ;
              is_false (commit_then_flag ()) ;
              (* The control: CHAIN in the session. The OK of COMMIT has
                 IN_TRANS set (a new transaction), and the write is committed.
                 Then a reset of the same connection gives NO_CHAIN again. *)
              exec_ok c "SET SESSION completion_type = 'CHAIN'" ;
              is_true ~msg:"CHAIN in the session" (commit_then_flag ()) ;
              equal string "ok" (unit_text (Conn.reset c)) ;
              equal string "Text \"NO_CHAIN\""
                (one_text c (sql "SELECT @@session.completion_type")) ;
              is_false (commit_then_flag ()) ;
              equal string "4"
                (admin (Printf.sprintf "SELECT COUNT(*) FROM %s.c26" database)) ;
              exec_ok c "DROP TABLE c26" ) ) )

(* C27 (the fact R4 of the plan review of phase 1.3): root holds FLUSH TABLES
   WITH READ LOCK on its own connection, the COMMIT of the test user waits, and
   root polls PROCESSLIST until it sees the COMMIT, then KILL QUERY. *)
let c27 =
  on all "C27: KILL QUERY of a COMMIT that waits: 1317, a rollback" (fun () ->
      warm () ;
      with_conn (user_conn ()) (fun c ->
          table c "c27" "(id INT PRIMARY KEY) ENGINE=InnoDB" ;
          let id = (Conn.info c).connection_id in
          exec_ok c "START TRANSACTION" ;
          exec_ok c "INSERT INTO c27 VALUES (1)" ;
          let commit =
            with_conn (root ()) (fun r ->
                exec_ok r "FLUSH TABLES WITH READ LOCK" ;
                Fun.protect
                  ~finally:(fun () -> exec_ok r "UNLOCK TABLES")
                  (fun () ->
                    Weft.Fiber.scope (fun s ->
                        let p =
                          Weft.Fiber.fork_promise s (fun () ->
                              exec c (sql "COMMIT") )
                        in
                        let waits () =
                          admin
                            (Printf.sprintf
                               "SELECT COUNT(*) FROM \
                                information_schema.PROCESSLIST WHERE ID = %Ld \
                                AND INFO = 'COMMIT'"
                               id )
                          = "1"
                        in
                        let rec poll n =
                          if not (waits ()) then
                            if n = 0 then failwith "c27: no COMMIT that waits"
                            else (
                              Weft.Time.sleep Weft.Clock.Mono (Weft.Span.ms 50) ;
                              poll (n - 1) )
                        in
                        poll 100 ;
                        ignore (admin (Printf.sprintf "KILL QUERY %Ld" id)) ;
                        Weft.Promise.await p ) ) )
          in
          is_true ~msg:("1317: " ^ commit)
            (String.starts_with ~prefix:"1317 70100" commit) ;
          (* The connection stays, with no transaction, and the write is
             gone. *)
          equal string "ok" (unit_text (Conn.ping c)) ;
          is_false (in_trans c) ;
          equal string "0"
            (admin (Printf.sprintf "SELECT COUNT(*) FROM %s.c27" database)) ;
          exec_ok c "DROP TABLE c27" ) )

(* C28 (the facts R1 and P7 of the plan of phase 1.3, and V4). *)
let c28 =
  on all "C28: a failed DDL commits first, TRUNCATE commits" (fun () ->
      warm () ;
      with_conn (user_conn ()) (fun c ->
          table c "c28" "(id INT PRIMARY KEY) ENGINE=InnoDB" ;
          table c "c28b" "(id INT) ENGINE=InnoDB" ;
          exec_ok c "START TRANSACTION" ;
          exec_ok c "INSERT INTO c28 VALUES (1)" ;
          equal string "1050 42S01: Driver 1050 42S01"
            (exec c (sql "CREATE TABLE c28 (id INT)")) ;
          equal string "ok" (unit_text (Conn.ping c)) ;
          is_false ~msg:"the ping after 1050" (in_trans c) ;
          exec_ok c "START TRANSACTION" ;
          exec_ok c "INSERT INTO c28 VALUES (2)" ;
          is_true (in_trans c) ;
          exec_ok c "TRUNCATE TABLE c28b" ;
          is_false ~msg:"the OK of TRUNCATE" (in_trans c) ;
          exec_ok c "ROLLBACK" ;
          equal string "1,2"
            (admin
               (Printf.sprintf "SELECT GROUP_CONCAT(id ORDER BY id) FROM %s.c28"
                  database ) ) ;
          exec_ok c "DROP TABLE c28, c28b" ) )

(* A raw TCP connection to the server, its greeting, and the SSL request
   (sequence id 1), with Cistern's packet layer. [f flow reader greeting]. *)
let after_ssl_request f =
  let flow = Weft_flow.Tcp.connect (Weft.Net.Tcp ("127.0.0.1", port)) in
  Fun.protect
    ~finally:(fun () -> Weft_flow.Flow.close flow)
    (fun () ->
      let r = Cistern_mysql.Packet.reader flow
      and w = Cistern_mysql.Packet.writer flow in
      let g =
        match Cistern_mysql.Packet.read r ~seq:0 with
        | Ok p ->
            Result.get_ok (Message.greeting p)
        | Error _ ->
            failwith "no greeting"
      in
      is_true ~msg:"CLIENT_SSL in the greeting" (Message.has g.capabilities Ssl) ;
      let capabilities =
        List.fold_left
          (fun acc c -> acc lor Message.bit c)
          0
          [Message.Protocol_41; Secure_connection; Plugin_auth; Ssl]
        land g.capabilities
      in
      Cistern_mysql.Packet.write w ~seq:1 (fun w ->
          Message.write_ssl_request w ~capabilities ~charset:45 ) ;
      Weft_flow.Writer.flush w ;
      f flow r g )

(* C29 (the probe T1 of the plan of phase 1.4): a raw check. *)
let c29 =
  on all "C29: no byte from the server after the SSL request" (fun () ->
      warm () ;
      after_ssl_request (fun flow r _ ->
          Weft.Time.sleep Weft.Clock.Mono (Weft.Span.ms 300) ;
          equal ~msg:"the reader" int 0 (Weft_flow.Reader.buffered r) ;
          equal ~msg:"read_now" (option int) None
            (Weft_flow.Flow.read_now flow (Bytes.create 1) 0 1) ) )

(* C30 (the probe T8): the server ends a TLS session with no close_notify. The
   session is killed before the authentication: the greeting gives its id (C6).
   A read then gives the plaintext that came before the end (maybe an error
   packet), then Flow.Closed: a truncation, not the end of the stream (0). *)
let c30 =
  on all "C30: no close_notify at the end of a TLS session" (fun () ->
      warm () ;
      let setup =
        match
          Cistern_mysql.Tls_setup.make ~time:Cistern_mysql.Tls_setup.now
            ~cas:
              (Result.get_ok
                 (Cistern_mysql.Tls_setup.ca_certificates
                    (In_channel.with_open_bin ca In_channel.input_all) ) )
            (on_tls ())
        with
        | Ok s ->
            s
        | Error m ->
            failwith m
      in
      let result =
        after_ssl_request (fun flow _ g ->
            let tls, _ = Weft_tls.client setup.config flow in
            kill (Int64.of_int g.thread_id) ;
            let b = Bytes.create 4096 in
            let rec read n =
              match Weft_flow.Flow.read tls b 0 4096 with
              | 0 ->
                  "the end of the stream (close_notify)"
              | k ->
                  read (n + k)
              | exception Weft_flow.Flow.Closed ->
                  "Flow.Closed (no close_notify)"
            in
            read 0 )
      in
      equal string "Flow.Closed (no close_notify)" result )

(* C31 (the probe T7): each server sends two session tickets (records with no
   data) after the handshake, before its first data. The client reads past them
   in the authentication, thus a query works after them, and the connection is
   not stale after a query. A record with no data at an idle time is tested in
   the simulator ("stale on TLS", "session tickets after the handshake"). *)
let c31 =
  on all "C31: the tickets after the TLS handshake come before the data"
    (fun () ->
      warm () ;
      with_conn (on_tls ()) (fun c ->
          equal string "Int 1" (one_text c (sql "SELECT 1")) ;
          Weft.Time.sleep Weft.Clock.Mono (Weft.Span.ms 100) ;
          is_false ~msg:"stale" (Conn.stale c) ;
          equal string "Int 2" (one_text c (sql "SELECT 2")) ) )

(* The claims of KILL (phase 1.5d), with Connection.kill of Cistern's client.
   [sleeping c s f]: a SELECT SLEEP of [s] seconds on [c] in a fiber, [f] while
   it runs (root sees it in PROCESSLIST), and the result of the SELECT. *)
let sleeping c s f =
  let info = Printf.sprintf "SELECT SLEEP(%d)" s in
  Weft.Fiber.scope (fun s ->
      let p = Weft.Fiber.fork_promise s (fun () -> exec c (sql info)) in
      let id = (Conn.info c).connection_id in
      let rec poll n =
        if
          admin
            (Printf.sprintf
               "SELECT COUNT(*) FROM information_schema.PROCESSLIST WHERE ID = \
                %Ld AND INFO = '%s'"
               id info )
          <> "1"
        then
          if n = 0 then failwith "integration: no SLEEP that runs"
          else (
            Weft.Time.sleep Weft.Clock.Mono (Weft.Span.ms 10) ;
            poll (n - 1) )
      in
      poll 500 ;
      let r = f id in
      (r, Weft.Promise.await p) )

let kill_text = function
  | Ok Conn.Killed ->
      "killed"
  | Ok Conn.Gone ->
      "gone"
  | Ok Conn.Not_owner ->
      "not_owner"
  | Error e ->
      error_text e

(* C32 (K1, K3 of the plan of phase 1.5). *)
let c32 =
  on all "C32: the test user kills its own session; 1095 for root's" (fun () ->
      warm () ;
      with_conn (user_conn ()) (fun killer ->
          with_conn (user_conn ()) (fun victim ->
              let k, r =
                sleeping victim 5 (fun id -> kill_text (Conn.kill killer id))
              in
              equal string "killed" k ;
              equal string "Outcome_unknown statement" r ) ;
          with_conn (root ()) (fun r ->
              let k, _ =
                sleeping r 1 (fun id -> kill_text (Conn.kill killer id))
              in
              equal string "not_owner" k ;
              (* The connection of the test user stays usable. *)
              equal string "ok" (unit_text (Conn.ping killer)) ) ) )

(* C33 (K2). *)
let c33 =
  on all "C33: KILL of an id that does not exist: 1094" (fun () ->
      warm () ;
      with_conn (user_conn ()) (fun c ->
          equal string "gone" (kill_text (Conn.kill c 4_000_000_000L)) ;
          equal string "ok" (unit_text (Conn.ping c)) ) )

(* C35 (V7, the probe P1 of review A of the plan of phase 1.5): the form of C27,
   with KILL CONNECTION of Cistern's client. *)
let c35 =
  on all "C35: KILL CONNECTION of a COMMIT that waits: a rollback" (fun () ->
      warm () ;
      with_conn (user_conn ()) (fun killer ->
          with_conn (user_conn ()) (fun c ->
              table c "c35" "(id INT PRIMARY KEY) ENGINE=InnoDB" ;
              let id = (Conn.info c).connection_id in
              exec_ok c "START TRANSACTION" ;
              exec_ok c "INSERT INTO c35 VALUES (1)" ;
              let commit, gone_with_lock =
                with_conn (root ()) (fun r ->
                    exec_ok r "FLUSH TABLES WITH READ LOCK" ;
                    Fun.protect
                      ~finally:(fun () -> exec_ok r "UNLOCK TABLES")
                      (fun () ->
                        Weft.Fiber.scope (fun s ->
                            let p =
                              Weft.Fiber.fork_promise s (fun () ->
                                  exec c (sql "COMMIT") )
                            in
                            let count () =
                              admin
                                (Printf.sprintf
                                   "SELECT COUNT(*) FROM \
                                    information_schema.PROCESSLIST WHERE ID = \
                                    %Ld"
                                   id )
                            in
                            let rec poll n =
                              if
                                admin
                                  (Printf.sprintf
                                     "SELECT COUNT(*) FROM \
                                      information_schema.PROCESSLIST WHERE ID \
                                      = %Ld AND INFO = 'COMMIT'"
                                     id )
                                <> "1"
                              then
                                if n = 0 then
                                  failwith "c35: no COMMIT that waits"
                                else (
                                  Weft.Time.sleep Weft.Clock.Mono
                                    (Weft.Span.ms 50) ;
                                  poll (n - 1) )
                            in
                            poll 100 ;
                            equal string "killed"
                              (kill_text (Conn.kill killer id)) ;
                            let r = Weft.Promise.await p in
                            let rec gone n =
                              count () = "0"
                              || n > 0
                                 &&
                                 ( Weft.Time.sleep Weft.Clock.Mono
                                     (Weft.Span.ms 10) ;
                                   gone (n - 1) )
                            in
                            (r, gone 100) ) ) )
              in
              (* The end of the stream after the flush of COMMIT. *)
              equal string "Outcome_unknown statement" commit ;
              is_true ~msg:"the session goes while the lock is held"
                gone_with_lock ) ;
          equal string "Int 0"
            (one_text killer (sql "SELECT COUNT(*) FROM c35")) ;
          exec_ok killer "DROP TABLE c35" ) )

(* C36 (V7 of the plan of phase 2, the probe of 2f): a filter with <=> on a
   nullable column with an index uses the index (EXPLAIN gives the type ref and
   the key), as =. Thus ==. on an option field is not a scan. *)
let c36 =
  on all "C36: <=> on an indexed nullable column uses the index" (fun () ->
      warm () ;
      with_conn (user_conn ()) (fun c ->
          table c "c36"
            "(id BIGINT NOT NULL AUTO_INCREMENT PRIMARY KEY, n BIGINT NULL, \
             KEY c36_n_idx (n)) ENGINE=InnoDB" ;
          exec_ok c
            "INSERT INTO c36 (n) VALUES (1), (2), (3), (NULL), (5), (6), (7), \
             (8), (9), (10), (NULL), (12)" ;
          ignore (Conn.query c (sql "ANALYZE TABLE c36")) ;
          let explain where =
            match
              Conn.query c (sql ("EXPLAIN SELECT id FROM c36 WHERE " ^ where))
            with
            | Ok {columns; rows= row :: _} ->
                let at name =
                  let rec find i =
                    if i >= Array.length columns then "none"
                    else if String.equal columns.(i) name then
                      value_text row.(i)
                    else find (i + 1)
                  in
                  find 0
                in
                at "type" ^ " " ^ at "key"
            | Ok _ ->
                "no row"
            | Error e ->
                error_text e
          in
          equal string "Text \"ref\" Text \"c36_n_idx\"" (explain "c36.n <=> 5") ;
          equal string "Text \"ref\" Text \"c36_n_idx\"" (explain "c36.n = 5") ;
          exec_ok c "DROP TABLE c36" ) )

let deterministic =
  group "claims"
    [ c1
    ; c36
    ; c2
    ; c3
    ; c4
    ; c5
    ; c6
    ; c10
    ; c11
    ; c12
    ; c13
    ; c14
    ; c15
    ; c17
    ; c18
    ; c19
    ; c20
    ; c21
    ; c22
    ; c26
    ; c27
    ; c28
    ; c29
    ; c30
    ; c31
    ; c32
    ; c33
    ; c35 ]

(* The timing claims. A connection A waits on a row lock that the admin holds;
   the killer polls the server until A waits, then acts. *)

let lock_wait_sql id =
  Printf.sprintf
    "SELECT COUNT(*) FROM information_schema.PROCESSLIST p JOIN \
     information_schema.INNODB_TRX x ON x.trx_mysql_thread_id = p.ID WHERE \
     p.ID = %Ld AND p.INFO LIKE 'INSERT%%' AND x.trx_state = 'LOCK WAIT'"
    id

(* Poll every 250 ms, at most 10 s, until the statement of [id] waits on the
   lock. InnoDB fills information_schema.INNODB_TRX again only when nobody read
   it for 100 ms, thus a faster poll sees the same old rows each time. *)
let wait_for_lock killer id =
  let rec go n =
    match one killer (sql (lock_wait_sql id)) with
    | Value.Int 1L ->
        ()
    | _ when n > 0 ->
        Weft.Time.sleep Weft.Clock.Mono (Weft.Span.ms 250) ;
        go (n - 1)
    | _ ->
        failwith "the statement does not wait on the lock"
  in
  go 40

(* [lock_wait act]: A runs an autocommit INSERT that waits on the lock; then
   [act killer a_id] runs; the result of A, then [after a]. *)
let lock_wait ~act ~after =
  with_conn (user_conn ()) (fun c ->
      table c "c8l" "(id INT PRIMARY KEY) ENGINE=InnoDB" ;
      table c "c8t" "(id INT PRIMARY KEY) ENGINE=InnoDB" ;
      exec_ok c "INSERT INTO c8l VALUES (1)" ) ;
  with_conn (root ()) (fun holder ->
      exec_ok holder "START TRANSACTION" ;
      ignore (one holder (sql "SELECT id FROM c8l WHERE id = 1 FOR UPDATE")) ;
      with_conn (root ()) (fun killer ->
          match Conn.connect (user_conn ()) with
          | Error e ->
              failwith (error_text e)
          | Ok a ->
              let id = (Conn.info a).connection_id in
              let result =
                Weft.Fiber.scope (fun s ->
                    let p =
                      Weft.Fiber.fork_promise s (fun () ->
                          outcome a
                            (Conn.exec a
                               (sql
                                  "INSERT INTO c8t SELECT id FROM c8l FOR \
                                   UPDATE" ) ) )
                    in
                    wait_for_lock killer id ;
                    act killer id ;
                    Weft.Promise.await p )
              in
              let later = after a in
              ignore (Conn.close a) ;
              exec_ok holder "ROLLBACK" ;
              let written = one_text killer (sql "SELECT COUNT(*) FROM c8t") in
              exec_ok killer "DROP TABLE c8t" ;
              exec_ok killer "DROP TABLE c8l" ;
              (result, later, written) ) )

let c8a =
  on both "C8a: KILL CONNECTION of an autocommit write" (fun () ->
      warm () ;
      let result, later, written =
        lock_wait
          ~act:(fun k id -> exec_ok k (Printf.sprintf "KILL CONNECTION %Ld" id))
          ~after:(fun a -> unit_text (Conn.ping a))
      in
      (* The end of the stream with no ERR: no 1927. *)
      equal string "Outcome_unknown statement" result ;
      equal string "Connection_lost" later ;
      equal string "Int 0" written )

let c8b =
  on both "C8b: KILL QUERY" (fun () ->
      warm () ;
      let result, later, written =
        lock_wait
          ~act:(fun k id -> exec_ok k (Printf.sprintf "KILL QUERY %Ld" id))
          ~after:(fun a -> unit_text (Conn.ping a))
      in
      equal string "1317 70100: Driver 1317 70100" result ;
      equal string "ok" later ;
      equal string "Int 0" written )

let c9 =
  on both "C9: the idle timeout" (fun () ->
      warm () ;
      with_conn (user_conn ()) (fun c -> table c "c9" "(id INT)") ;
      let run config command =
        with_conn config (fun c ->
            exec_ok c "SET SESSION wait_timeout = 1" ;
            (* The only fixed wait of the claims: the timeout of the server is
               the subject of the test. *)
            Weft.Time.sleep Weft.Clock.Mono (Weft.Span.s 3) ;
            command c )
      in
      List.iter
        (fun config ->
          equal string "Connection_lost"
            (run config (fun c -> unit_text (Conn.ping c))) ;
          equal string "Connection_lost"
            (run config (fun c -> exec c (sql "INSERT INTO c9 VALUES (1)"))) )
        [user_conn (); on_tcp ()] ;
      with_conn (user_conn ()) (fun c ->
          equal string "Int 0" (one_text c (sql "SELECT COUNT(*) FROM c9")) ;
          exec_ok c "DROP TABLE c9" ) )

(* C34 (K4, P1 of phase 1.5): a killed session leaves PROCESSLIST within 1 s
   (the probes: 1 to 64 ms). *)
let c34 =
  on all "C34: a killed session leaves PROCESSLIST within 1 s" (fun () ->
      warm () ;
      with_conn (user_conn ()) (fun killer ->
          with_conn (user_conn ()) (fun victim ->
              let left, _ =
                sleeping victim 5 (fun id ->
                    let t0 = Weft.Time.now Weft.Clock.Mono in
                    ignore (Conn.kill killer id) ;
                    (* At most 5 s. *)
                    let rec wait n =
                      if
                        n > 0
                        && admin
                             (Printf.sprintf
                                "SELECT COUNT(*) FROM \
                                 information_schema.PROCESSLIST WHERE ID = %Ld"
                                id )
                           <> "0"
                      then (
                        Weft.Time.sleep Weft.Clock.Mono (Weft.Span.ms 5) ;
                        wait (n - 1) )
                    in
                    wait 1000 ;
                    Int64.to_float
                      (Weft.Span.to_ns
                         (Weft.Time.diff Weft.Clock.Mono
                            (Weft.Time.now Weft.Clock.Mono)
                            t0 ) )
                    /. 1e9 )
              in
              is_true ~msg:(Printf.sprintf "%.3f s" left) (left < 1.0) ) ) )

let timing = group "timing claims" [c8a; c8b; c9; c34]

(* C8c stops the server: the nightly job runs it last, then make db-up. *)
let c8c =
  on both "C8c: SHUTDOWN during a write" (fun () ->
      warm () ;
      with_conn (user_conn ()) (fun c ->
          table c "c8l" "(id INT PRIMARY KEY) ENGINE=InnoDB" ;
          table c "c8t" "(id INT PRIMARY KEY) ENGINE=InnoDB" ;
          exec_ok c "INSERT INTO c8l VALUES (1)" ) ;
      let connect config =
        match Conn.connect config with
        | Ok c ->
            c
        | Error e ->
            failwith (error_text e)
      in
      let holder = connect (root ())
      and killer = connect (root ())
      and a = connect (user_conn ())
      and idle = connect (on_tcp ()) in
      exec_ok holder "START TRANSACTION" ;
      ignore (one holder (sql "SELECT id FROM c8l WHERE id = 1 FOR UPDATE")) ;
      let id = (Conn.info a).connection_id in
      let result =
        Weft.Fiber.scope (fun s ->
            let p =
              Weft.Fiber.fork_promise s (fun () ->
                  outcome a
                    (Conn.exec a
                       (sql "INSERT INTO c8t SELECT id FROM c8l FOR UPDATE") ) )
            in
            wait_for_lock killer id ;
            ignore (Conn.exec killer (sql "SHUTDOWN")) ;
            Weft.Promise.await p )
      in
      (* The idle connection gets the end of the stream when the server closes
         it, a short time after the running statement. Ping it every 250 ms, at
         most 30 s. *)
      let rec later n =
        match unit_text (Conn.ping idle) with
        | "ok" when n > 0 ->
            Weft.Time.sleep Weft.Clock.Mono (Weft.Span.ms 250) ;
            later (n - 1)
        | r ->
            r
      in
      let later = later 120 in
      List.iter (fun c -> ignore (Conn.close c)) [holder; killer; a; idle] ;
      (* MySQL sends 1053 to the running statement, then the end of the stream.
         MariaDB sends only the end of the stream. *)
      equal string
        ( if mysql then "1053 08S01: Outcome_unknown statement"
          else "Outcome_unknown statement" )
        result ;
      equal string "Connection_lost" later )

let shutdown = group "shutdown claim" [c8c]
