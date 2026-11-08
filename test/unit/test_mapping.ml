(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* Error.of_server and Error.interrupts: the table of DESIGN.md, "Error
   mapping". perror of the pinned servers confirmed each number. *)

open Windtrap
open Cistern_core

let error = Testable.make ~pp:Error.pp ~equal:( = )

let name : Dialect.t -> string = function
  | Mysql ->
      "MySQL"
  | Mariadb ->
      "MariaDB"

let both = [Dialect.Mysql; Dialect.Mariadb]

(* A row: the dialect, the number, the SQLSTATE, the message, and the expected
   error. *)
type row = Dialect.t * int * string * string * Error.t

let row_id ((d, code, sqlstate, message, _) : row) =
  Printf.sprintf "%s %d %S %S" (name d) code sqlstate message

let on dialects code expected : row list =
  List.map (fun d -> (d, code, "HY000", "m", expected)) dialects

let violation kind = Error.Constraint_violation {kind; message= "m"}

let invalid code = Error.Invalid_value {code= Some code; message= "m"}

let driver ?(sqlstate = "HY000") ?(message = "m") code =
  Error.Driver {code; sqlstate; message}

(* Each cell of the table. *)
let table : row list =
  List.concat
    [ List.concat_map
        (fun c -> on both c (violation Unique))
        [1062; 1586; 1022; 1169; 1859; 1761; 1762]
    ; List.concat_map
        (fun c -> on both c (violation Foreign_key))
        [1451; 1452; 1216; 1217]
    ; List.concat_map (fun c -> on both c (violation Not_null)) [1048; 1364]
    ; on [Mysql] 3673 (violation Not_null)
    ; on [Mysql] 3819 (violation Check)
    ; on [Mariadb] 4025 (violation Check)
    ; List.concat_map
        (fun c -> on both c (invalid c))
        [1406; 1264; 1265; 1365; 1366; 1292; 1690; 1367; 1301; 1153]
    ; on both 1213 (Retryable `Deadlock)
    ; on both 1205 (Retryable `Lock_timeout)
    ; on [Mysql] 3101 (Retryable `Rolled_back)
    ; on [Mariadb] 1020 (Retryable `Rolled_back)
    ; on [Mysql] 3024 Statement_timeout
    ; on [Mariadb] 1969 Statement_timeout
    ; on [Mysql] 4031 Connection_lost
    ; on [Mariadb] 1927 Connection_lost
    ; on both 1053 Connection_lost
    ; List.concat_map (fun c -> on both c Read_only) [1792; 1836; 1874]
    ; List.concat_map
        (fun c -> on both c (driver c))
        [1317; 1052; 1036; 1180; 1305; 1094; 1064]
    ; on [Mysql] 3572 (driver 3572) ]

(* A number of one server is another error, or no error, on the other server. *)
let crossed : row list =
  [ (Mariadb, 3819, "HY000", "m", driver 3819)
  ; (Mysql, 4025, "HY000", "m", driver 4025)
  ; (Mariadb, 3024, "HY000", "m", driver 3024)
  ; (Mysql, 1969, "HY000", "m", driver 1969)
  ; (Mariadb, 4031, "HY000", "m", driver 4031)
  ; (Mysql, 1927, "HY000", "m", driver 1927)
  ; (Mariadb, 3101, "HY000", "m", driver 3101)
  ; (Mysql, 1020, "HY000", "m", driver 1020)
  ; (Mariadb, 3673, "HY000", "m", driver 3673) ]

(* 1290 is Read_only only for the options --read-only and --super-read-only. *)
let option_1290 : row list =
  List.concat_map
    (fun d ->
      [ (d, 1290, "HY000", "--read-only", Error.Read_only)
      ; (d, 1290, "HY000", "x --super-read-only", Error.Read_only)
      ; ( d
        , 1290
        , "HY000"
        , "The MySQL server is running with the --read-only option so it \
           cannot execute this statement"
        , Error.Read_only )
      ; (d, 1290, "HY000", "--read-onl", driver ~message:"--read-onl" 1290)
      ; ( d
        , 1290
        , "HY000"
        , "--secure-file-priv"
        , driver ~message:"--secure-file-priv" 1290 ) ] )
    both

(* A number that the table does not have: only an exact SQLSTATE of 5 characters
   takes the fallback. *)
let sqlstate : row list =
  List.map
    (fun (s, e) -> (Dialect.Mysql, 9999, s, "m", e))
    [ ("", driver ~sqlstate:"" 9999)
    ; ("2", driver ~sqlstate:"2" 9999)
    ; ("23", driver ~sqlstate:"23" 9999)
    ; ("2300", driver ~sqlstate:"2300" 9999)
    ; ("4000", driver ~sqlstate:"4000" 9999)
    ; ("22001", driver ~sqlstate:"22001" 9999)
    ; ("24000", driver ~sqlstate:"24000" 9999)
    ; ("230000", driver ~sqlstate:"230000" 9999)
    ; ("23000", violation Other_constraint)
    ; ("23505", violation Other_constraint)
    ; ("40001", Retryable `Deadlock)
    ; ("40002", driver ~sqlstate:"40002" 9999)
    ; ("HY000", driver ~sqlstate:"HY000" 9999) ]

(* A number of the table wins over its SQLSTATE. *)
let order : row list =
  [ (Mariadb, 1062, "23000", "m", violation Unique)
  ; (Mysql, 1052, "23000", "m", driver ~sqlstate:"23000" 1052)
  ; (Mariadb, 1052, "23000", "m", driver ~sqlstate:"23000" 1052)
  ; (Mysql, 3673, "23000", "m", violation Not_null)
  ; (Mariadb, 1761, "23000", "m", violation Unique)
  ; (Mysql, 1048, "23000", "m", violation Not_null)
  ; (Mysql, 1213, "40001", "m", Retryable `Deadlock)
  ; (Mysql, 1317, "23000", "m", driver ~sqlstate:"23000" 1317)
  ; ( Mariadb
    , 1290
    , "23000"
    , "--secure-file-priv"
    , driver ~sqlstate:"23000" ~message:"--secure-file-priv" 1290 ) ]

let mapped name rows =
  cases name ~id:row_id rows (fun (d, code, sqlstate, message, expected) ->
      equal error expected (Error.of_server d ~code ~sqlstate ~message) )

let interrupts =
  [ (Dialect.Mysql, 1053, true)
  ; (Mysql, 1317, true)
  ; (Mysql, 4031, true)
  ; (Mysql, 1927, false)
  ; (Mysql, 1062, false)
  ; (Mariadb, 1053, true)
  ; (Mariadb, 1317, true)
  ; (Mariadb, 1927, true)
  ; (Mariadb, 4031, false)
  ; (Mariadb, 1062, false) ]

let tests =
  group "Error mapping"
    [ mapped "of_server: the table" table
    ; mapped "of_server: the crossed numbers" crossed
    ; mapped "of_server: 1290" option_1290
    ; mapped "of_server: the SQLSTATE" sqlstate
    ; mapped "of_server: the table before the SQLSTATE" order
    ; cases "interrupts"
        ~id:(fun (d, code, _) -> Printf.sprintf "%s %d" (name d) code)
        interrupts
        (fun (d, code, expected) ->
          equal bool expected (Error.interrupts d code) )
    ; test "the exception printer prints no message of a 1062" (fun () ->
          let text =
            Printexc.to_string
              (Error.Cistern_error
                 (Error.of_server Mysql ~code:1062 ~sqlstate:"23000"
                    ~message:"Duplicate entry 'secret' for key 'email'" ) )
          in
          equal string "Cistern_error: Constraint_violation Unique" text ;
          not_contains ~sub:"secret" text ) ]
