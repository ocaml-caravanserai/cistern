(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* Queries and writes on the models of Blog. *)

open Cistern
open Blog

(* README begin: queries *)
(* The field names of [Post.Q] hide the values of the same names in
   [Post.Q.( ... )]. Thus the author id has another name here. *)
let published_titles db aid =
  Post.where Post.Q.(author_id ==. aid && published ==. true)
  |> Query.order_by Post.Q.views `Desc
  |> Query.limit 10 |> Query.to_list |> Db.exec_exn db
  |> List.map (fun (p : Post.t) -> p.title)

let popular db =
  Post.where Post.Q.(views >=. 100) |> Query.count |> Db.exec_exn db

let by_email db email = Author.find_by_email email |> Db.exec_exn db
(* README end *)

(* README begin: writes *)
let writes db =
  let ada =
    Author.create ~name:"Ada" ~email:"ada@example.com" () |> Db.exec_exn db
  in
  let ada = Author.update ~name:"Ada L." ada |> Db.exec_exn db in
  let post =
    Post.create ~author_id:ada.id ~title:"Notes" () |> Db.exec_exn db
  in
  let _post = Post.update ~views:1 post |> Db.exec_exn db in
  (* An update of each row that the query finds: the number of rows. *)
  let changed =
    Post.where Post.Q.(author_id ==. ada.id)
    |> Query.update (Update.set Post.Q.published true)
    |> Db.exec_exn db
  in
  Printf.printf "published %d posts\n" changed ;
  Post.where Post.Q.(author_id ==. ada.id)
  |> Query.delete |> Db.exec_exn db |> ignore ;
  Author.delete ada |> Db.exec_exn db
(* README end *)

let run db =
  writes db ;
  let a =
    Author.create ~name:"Grace" ~email:"grace@example.com"
      ~posts:
        [ Post.draft ~title:"A" ~views:150 ~published:true ()
        ; Post.draft ~title:"B" ~views:20 ~published:true ()
        ; Post.draft ~title:"C" ~views:500 () ]
      ()
    |> Db.exec_exn db
  in
  assert (published_titles db a.id = ["A"; "B"]) ;
  assert (popular db = 2) ;
  assert (
    Option.map (fun (x : Author.t) -> x.id) (by_email db "grace@example.com")
    = Some a.id ) ;
  assert (by_email db "ada@example.com" = None)
