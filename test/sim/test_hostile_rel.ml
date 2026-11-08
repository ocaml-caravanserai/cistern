(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* The hostile relation group of test/ppx/driver on Fake_db (phase 4g, the
   review of 4g, M1 and L1): include_ through the Rel values of a type with two
   belongs_to, whose key functions to_sql cannot see, and the groups of create
   of a type with two has_many to one child type. *)

open Windtrap
module D = Fake_db
module Db = Cistern.Db
module Query = Cistern.Query
module R = Ppx_driver_check.Relations

let with_pool = Test_pool.with_pool

(* The script of the SQL texts that start with a prefix of [l]. *)
let answer l db =
  db.D.faults.script <-
    (fun sql ->
      List.find_map
        (fun (p, r) -> if String.starts_with ~prefix:p sql then Some r else None)
        l )

let kcols = "SELECT `k`.`id`, `k`.`a key`, `k`.`b_id`, `k`.`c` FROM `k`"

(* A row of k: its a_id is 1, and its b_id [b]. *)
let krow b = [Some "10"; Some "1"; b; Some "x"]

let get = function
  | Ok v ->
      v
  | Error e ->
      fail (Cistern_core.Error.to_string e)

let tests =
  group "the hostile relation group on Fake_db"
    [ test "include_ B.Rel.r: the children by b_id, not by a_id" (fun () ->
          let r, _ =
            with_pool
              ~setup:
                (answer
                   [ (kcols ^ " WHERE `k`.`b_id` IN (", D.Rows [krow (Some "2")])
                   ; ("SELECT `b`.`bk` FROM `b`", D.Rows [[Some "2"]]) ] )
              (fun _ _ pool ->
                Db.exec pool
                  (R.B.all |> Query.include_ R.B.Rel.r |> Query.to_list) )
          in
          match get r with
          | [{bk= 2L; r= Loaded [k]}] ->
              is_true (k.b_id = Some 2L && k.a_id = 1L)
          | _ ->
              fail "the children of b 2" )
    ; test "include_ K.Rel.decode: the parent by b_id, not by a_id" (fun () ->
          let r, _ =
            with_pool
              ~setup:
                (answer
                   [ ( "SELECT `b`.`bk` FROM `b` WHERE `b`.`bk` IN ("
                     , D.Rows [[Some "1"]; [Some "2"]] )
                   ; (kcols, D.Rows [krow (Some "2")]) ] )
              (fun _ _ pool ->
                Db.exec pool
                  (R.K.all |> Query.include_ R.K.Rel.decode |> Query.to_list) )
          in
          match get r with
          | [{decode= Loaded (Some p); _}] ->
              equal int64 2L p.bk
          | _ ->
              fail "the parent of k" )
    ; test "create with two has_many: each group, in order, with its setter"
        (fun () ->
          let ids = ref 0 in
          let (r, inserts), _ =
            with_pool
              ~setup:(fun db ->
                db.D.faults.script <-
                  (fun sql ->
                    if String.starts_with ~prefix:"INSERT INTO `p`" sql then
                      Some (D.Counts {affected= 1L; insert= 5L})
                    else if String.starts_with ~prefix:"INSERT INTO `q`" sql
                    then (
                      incr ids ;
                      Some
                        (D.Counts
                           {affected= 1L; insert= Int64.of_int (100 + !ids)} ) )
                    else None ) )
              (fun _ db pool ->
                let r =
                  Db.exec pool
                    (R.P.create
                       ~x1:[R.Q.draft ~v:1 ()]
                       ~x2:[R.Q.draft ~v:2 (); R.Q.draft ~v:3 ()]
                       () )
                in
                ( r
                , List.filter
                    (String.starts_with ~prefix:"INSERT INTO `q`")
                    (List.rev db.D.log) ) )
          in
          let vs = function
            | Cistern.Deferred.Loaded l ->
                List.map (fun (q : R.q) -> (q.v, q.p_id)) l
            | Unloaded ->
                []
          in
          let p = get r in
          equal (list (pair int int64)) [(1, 5L)] (vs p.x1) ;
          equal (list (pair int int64)) [(2, 5L); (3, 5L)] (vs p.x2) ;
          (* The commands in the order of the labels: x1, then x2. *)
          equal (list string)
            [ "INSERT INTO `q` (`p_id`, `v`) VALUES (5, 1)"
            ; "INSERT INTO `q` (`p_id`, `v`) VALUES (5, 2)"
            ; "INSERT INTO `q` (`p_id`, `v`) VALUES (5, 3)" ]
            inserts ) ]
