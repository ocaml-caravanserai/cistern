(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* The relations of Blog: a nested create, the queries of a relation, and
   include_. *)

open Cistern
open Blog

(* README begin: relations *)
(* A nested create: the author and the posts in one transaction. *)
let create_with_posts db =
  Author.create ~name:"Mo" ~email:"mo@example.com"
    ~posts:[Post.draft ~title:"One" (); Post.draft ~title:"Two" ()]
    ()
  |> Db.exec_exn db

(* The query of a relation: the posts of an author, and the author of a post. *)
let posts_of db (a : Author.t) =
  Author.posts a |> Query.to_list |> Db.exec_exn db

let author_of db (p : Post.t) = Post.author p |> Db.exec_exn db

(* include_ loads the posts of each author with one more query, into the field
   [posts]. All the rows stay in memory: put a limit on the main query. *)
let authors_with_posts db =
  Author.all
  |> Query.include_ Author.Rel.posts
  |> Query.order_by Author.Q.id `Asc
  |> Query.limit 100 |> Query.to_list |> Db.exec_exn db
  |> List.map (fun (a : Author.t) ->
      (a.name, List.length (Deferred.get_exn a.posts)) )
(* README end *)

let run db =
  let a = create_with_posts db in
  (* The record of the nested create has the posts. *)
  assert (List.length (Deferred.get_exn a.posts) = 2) ;
  let posts = posts_of db a in
  assert (List.map (fun (p : Post.t) -> p.title) posts = ["One"; "Two"]) ;
  let first = List.hd posts in
  assert ((author_of db first).id = a.id) ;
  (* A record from a query has no loaded relation. *)
  assert (first.author = Deferred.Unloaded) ;
  assert (authors_with_posts db = [("Mo", 2)])
