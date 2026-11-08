(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* Raw SQL from fragments. *)

open Cistern

(* README begin: raw *)
(* Raw.v gives a literal of the client for a value. Put each value of a
   user in Raw.v, never in the text of Raw.sql. *)
let titles_with_views db min =
  let open Raw in
  let r =
    Raw.query
      ( sql "SELECT `title` FROM `post` WHERE `views` >= "
      ++ v (Value.Int (Int64.of_int min))
      ++ sql " ORDER BY `title`" )
    |> Db.exec_exn db
  in
  List.map
    (function [|Value.Text t|] -> t | _ -> failwith "a row of one text")
    r.rows
(* README end *)

let run db =
  let open Blog in
  ignore
    ( Author.create ~name:"Raw" ~email:"raw@example.com"
        ~posts:
          [ Post.draft ~title:"b" ~views:5 ()
          ; Post.draft ~title:"a" ~views:9 ()
          ; Post.draft ~title:"c" ~views:1 () ]
        ()
    |> Db.exec_exn db ) ;
  assert (titles_with_views db 5 = ["a"; "b"])
