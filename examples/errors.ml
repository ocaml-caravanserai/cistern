(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* The errors of a statement, and the events of a pool. *)

open Cistern
open Blog

(* README begin: errors *)
(* Db.exec gives a result. The doc of Cistern.Error tells what each error
   means and what to do. *)
let add_author db ~name ~email =
  match Author.create ~name ~email () |> Db.exec db with
  | Ok a ->
      Ok a
  | Error (Error.Constraint_violation {kind= Unique; _}) ->
      Error "this email has an author"
  | Error e ->
      (* Error.pp_summary prints no value of a row: use it for logs. *)
      Error (Format.asprintf "%a" Error.pp_summary e)
(* README end *)

(* README begin: events *)
(* The hook gets an event for each statement, connect and kill. The SQL of
   an event has [?] in place of each value. *)
let with_logged_db config f =
  let on_query e = Format.eprintf "cistern: %a@." Event.pp e in
  Db.with_db ~on_query config f
(* README end *)

let run db =
  assert (Result.is_ok (add_author db ~name:"E" ~email:"e@example.com")) ;
  assert (
    add_author db ~name:"F" ~email:"e@example.com"
    = Error "this email has an author" )
