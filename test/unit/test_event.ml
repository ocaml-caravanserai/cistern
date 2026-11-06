(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* Event.pp: the formats, and no message of a server (decision Q9). *)

open Windtrap
open Cistern_core

let secret = "Duplicate entry 'secret@example.com' for key 'email'"

let printed e = Format.asprintf "%a" Event.pp e

let query outcome =
  Event.Query
    {sql= "SELECT a FROM t WHERE id = ?"; duration= Weft.Span.ns 1500; outcome}

let connect error =
  Event.Connect
    {address= "db.example.com:3306"; duration= Weft.Span.ms 10; error}

(* Each event, and its text. *)
let rows =
  [ ( "a query with rows"
    , (fun () -> query (Done {rows= 3}))
    , {|query 1500ns rows 3 "SELECT a FROM t WHERE id = ?"|} )
  ; ( "a failed query"
    , (fun () ->
        query (Failed (Constraint_violation {kind= Unique; message= secret})) )
    , {|query 1500ns failed Constraint_violation Unique "SELECT a FROM t WHERE id = ?"|}
    )
  ; ( "a cancelled query that was sent"
    , (fun () -> query (Cancelled {sent= true}))
    , {|query 1500ns cancelled sent "SELECT a FROM t WHERE id = ?"|} )
  ; ( "a cancelled query that was not sent"
    , (fun () -> query (Cancelled {sent= false}))
    , {|query 1500ns cancelled not sent "SELECT a FROM t WHERE id = ?"|} )
  ; ( "a connect"
    , (fun () -> connect None)
    , "connect 10000000ns db.example.com:3306 ok" )
  ; ( "a failed connect"
    , (fun () -> connect (Some (Connect secret)))
    , "connect 10000000ns db.example.com:3306 failed Connect" ) ]

(* Each error with a message, in a query and in a connect. *)
let errors =
  [ ( "Constraint_violation"
    , Error.Constraint_violation {kind= Check; message= secret} )
  ; ("Invalid_value", Error.Invalid_value {code= Some 1406; message= secret})
  ; ( "Invalid_value with no code"
    , Error.Invalid_value {code= None; message= secret} )
  ; ("Connect", Error.Connect secret)
  ; ("Decode", Error.Decode secret)
  ; ("Driver", Error.Driver {code= 1064; sqlstate= "42000"; message= secret}) ]

let tests =
  group "Event"
    [ cases "pp"
        ~id:(fun (n, _, _) -> n)
        rows
        (fun (_, e, text) -> equal string text (printed (e ())))
    ; cases "pp prints no message of a query" ~id:fst errors (fun (_, e) ->
          let text = printed (query (Failed e)) in
          contains
            ~sub:(Error.to_string e |> String.split_on_char ':' |> List.hd)
            text ;
          not_contains ~sub:"secret" text )
    ; cases "pp prints no message of a connect" ~id:fst errors (fun (_, e) ->
          not_contains ~sub:"secret" (printed (connect (Some e))) ) ]
