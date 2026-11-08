(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* The quick start of README.md: one model, the tables, a create and a query.
   [main] takes the URL of the database. *)

(* README begin: quickstart *)
open Cistern

type note =
  { id: int64 [@key] [@auto]
  ; title: string
  ; body: string option
  ; pinned: bool [@default false] }
[@@deriving cistern]

let main url =
  (* TLS takes its randomness from the default generator. *)
  Mirage_crypto_rng_unix.use_default () ;
  Weft.run (module Weft_os)
  @@ fun () ->
  let config =
    match Config.of_url url with Ok c -> c | Error m -> failwith m
  in
  Db.with_db config
  @@ fun db ->
  Db.push_schema db [Note.model] ;
  let n = Note.create ~title:"First note" ~pinned:true () |> Db.exec_exn db in
  Printf.printf "created the note %Ld\n" n.id ;
  Note.where Note.Q.(pinned ==. true)
  |> Query.order_by Note.Q.id `Asc
  |> Query.to_list |> Db.exec_exn db
  |> List.iter (fun (n : Note.t) -> print_endline n.title)
(* README end *)
