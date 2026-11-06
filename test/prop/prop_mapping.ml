(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* Error.of_server for any packet: no exception, the fields stay, and a number
   that the table does not have takes the class of its SQLSTATE. *)

open Windtrap
open Cistern_core

(* The numbers of the table of DESIGN.md, on one dialect or on the two. *)
let table =
  [ 1062
  ; 1586
  ; 1022
  ; 1451
  ; 1452
  ; 1216
  ; 1217
  ; 1048
  ; 1364
  ; 3819
  ; 4025
  ; 1406
  ; 1264
  ; 1265
  ; 1365
  ; 1366
  ; 1292
  ; 1690
  ; 1367
  ; 1301
  ; 1153
  ; 1213
  ; 1205
  ; 3101
  ; 3024
  ; 1969
  ; 4031
  ; 1927
  ; 1053
  ; 1290
  ; 1792
  ; 1836
  ; 1874
  ; 1317
  ; 1169
  ; 1859
  ; 1761
  ; 1762
  ; 3673
  ; 1052 ]

let dialect = Gen.of_list [Dialect.Mysql; Dialect.Mariadb]

(* A random SQLSTATE of the characters of the protocol. *)
let any_sqlstate =
  Gen.string_of ~size:(Gen.int_range 0 6)
    (Gen.of_list
       (List.of_seq (String.to_seq "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ")) )

(* The class of a SQLSTATE, from the rule of DESIGN.md: a small table. *)
let class_of s =
  if String.length s = 5 && String.sub s 0 2 = "23" then `Other_constraint
  else if s = "40001" then `Deadlock
  else `Driver

(* A SQLSTATE and the result of the fallback for it: a table of classes. *)
let sqlstate : (string * [`Other_constraint | `Deadlock | `Driver]) Gen.t =
  Gen.frequency
    [ (2, Gen.constant ("23000", `Other_constraint))
    ; (1, Gen.constant ("23505", `Other_constraint))
    ; (2, Gen.constant ("40001", `Deadlock))
    ; (1, Gen.constant ("40000", `Driver))
    ; (1, Gen.constant ("HY000", `Driver))
    ; (1, Gen.constant ("", `Driver))
    ; (1, Gen.constant ("2", `Driver))
    ; (1, Gen.constant ("22001", `Driver))
    ; (1, Gen.constant ("24000", `Driver))
    ; (4, Gen.map (fun s -> (s, class_of s)) any_sqlstate) ]

(* A number near the table, in the table, or anywhere. *)
let code =
  Gen.frequency
    [ (2, Gen.of_list table)
    ; (1, Gen.map (fun c -> c + 1) (Gen.of_list table))
    ; (2, Gen.int_range 0 5000) ]

let fields (e : Error.t) ~code ~sqlstate ~message =
  match e with
  | Constraint_violation {message= m; _} | Connect m | Decode m ->
      equal string message m
  | Invalid_value {code= c; message= m} ->
      equal (option int) (Some code) c ;
      equal string message m
  | Driver {code= c; sqlstate= s; message= m} ->
      equal int code c ; equal string sqlstate s ; equal string message m
  | _ ->
      ()

let tests =
  group "Error mapping"
    [ prop "of_server gives no exception and keeps the fields" ~count:500
        (Gen.quad dialect code sqlstate Gen.string)
        (fun (d, code, (sqlstate, _), message) ->
          fields
            (Error.of_server d ~code ~sqlstate ~message)
            ~code ~sqlstate ~message )
    ; prop
        "a number that the table does not have takes the class of its SQLSTATE"
        ~count:500
        (Gen.triple dialect
           (Gen.such_that
              (fun c -> not (List.mem c table))
              (Gen.int_range 0 9999) )
           sqlstate )
        (fun (d, code, (sqlstate, cls)) ->
          let e = Error.of_server d ~code ~sqlstate ~message:"m" in
          match (cls, e) with
          | `Other_constraint, Constraint_violation {kind= Other_constraint; _}
          | `Deadlock, Retryable `Deadlock
          | `Driver, Driver _ ->
              ()
          | _ ->
              failf "%d %S gave %a" code sqlstate Error.pp e ) ]
