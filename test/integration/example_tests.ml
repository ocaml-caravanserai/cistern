(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* The examples of README.md (examples/, phase 5b) on the server. Each example
   runs on new tables, and the tables go after it. *)

open Windtrap
open Support
module Db = Cistern.Db
module Raw = Cistern.Raw
module Ex = Cistern_examples

let drop db =
  ignore
    (Db.exec_exn db
       (Raw.exec (Raw.sql "DROP TABLE IF EXISTS `post`, `author`, `note`")) )

let with_tables f =
  let pool g = in_weft (fun () -> Db.with_db (Db_tests.config ()) g) in
  pool drop ;
  Fun.protect ~finally:(fun () -> pool drop) f

(* An example on the models of Blog, with its tables. *)
let blog run () =
  with_tables (fun () ->
      in_weft (fun () ->
          Db.with_db (Db_tests.config ()) (fun db ->
              Db.push_schema db Ex.Blog.models ;
              run db ) ) )

(* The URL of the quick start: TLS with the test CA, as a user gives it. *)
let url () =
  Printf.sprintf "mysql://%s:%s@127.0.0.1:%d/%s?ssl-ca=%s" user password port
    database ca

let deterministic =
  group
    ("the examples of README.md on " ^ server)
    [ test "quickstart" (fun () ->
          with_tables (fun () -> Ex.Quickstart.main (url ())) ;
          (* A second run on the table that exists. *)
          with_tables (fun () ->
              Ex.Quickstart.main (url ()) ;
              Ex.Quickstart.main (url ()) ) )
    ; test "queries and writes" (blog Ex.Queries.run)
    ; test "transactions and the retry" (blog Ex.Transactions.run)
    ; test "relations" (blog Ex.Relations.run)
    ; test "raw" (blog Ex.Raw.run)
    ; test "errors" (blog Ex.Errors.run)
    ; test "events" (fun () ->
          let n = ref 0 in
          with_tables (fun () ->
              in_weft (fun () ->
                  Ex.Errors.with_logged_db (Db_tests.config ()) (fun db ->
                      Db.push_schema db Ex.Blog.models ;
                      incr n ) ) ) ;
          equal int 1 !n ) ]
