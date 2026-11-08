(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* An event holds no message of a server error packet (the plan of phase 2, D23;
   PR 2a0): the error of a Query event is redacted, and the result keeps the
   full error. "FAILV code" of Fake_db gives an ERR whose message quotes a
   value. *)

open Windtrap
open Weft
module D = Fake_db
module Db = Cistern.Db
module Tx = Cistern.Tx
module E = Cistern_core.Error
module Ev = Cistern_core.Event

let q = Test_pool.q

let secret = "secret@example.com"

(* The texts of the errors of the events, with the full printer, and of the
   events with Event.pp. *)
let event_texts events =
  List.concat_map
    (fun e ->
      let error =
        match e with
        | Ev.Query {outcome= Ev.Failed err; _} | Ev.Connect {error= Some err; _}
          ->
            [E.to_string err]
        | _ ->
            []
      in
      Format.asprintf "%a" Ev.pp e :: error )
    events

let contains s sub =
  let n = String.length s and k = String.length sub in
  let rec at i = i + k <= n && (String.sub s i k = sub || at (i + 1)) in
  at 0

(* A pool on Fake_db with the raw events. *)
let run f =
  let events = ref [] in
  let r =
    D.run (fun _ db ->
        Db.with_db ~pool_size:2
          ~on_query:(fun e -> events := e :: !events)
          (D.config ()) (f db) )
  in
  (r, List.rev !events)

let result = function Ok _ -> "ok" | Error e -> E.to_string e

let tests =
  group "the redaction of the events"
    [ cases "Db.exec: the result keeps the message, the event does not"
        ~id:(fun (code, _) -> string_of_int code)
        [ (1062, "Constraint_violation Unique")
        ; (1366, "Invalid_value 1366")
        ; (1064, "Driver 1064 28000") ]
        (fun (code, summary) ->
          let r, events =
            run (fun _ pool ->
                result (Db.exec pool (q (Printf.sprintf "FAILV %d" code))) )
          in
          equal string
            (summary ^ ": Duplicate entry '" ^ secret ^ "' for key")
            r ;
          let failed =
            List.filter_map
              (function
                | Ev.Query {outcome= Ev.Failed err; _} -> Some err | _ -> None )
              events
          in
          equal (list string) [summary] (List.map E.to_string failed) ;
          List.iter
            (fun t -> is_false ~msg:t (contains t secret))
            (event_texts events) )
    ; test "Tx.exec: the result keeps the message, the event does not"
        (fun () ->
          let r, events =
            run (fun _ pool ->
                let inner = ref "" in
                let t =
                  Db.transaction pool (fun tx ->
                      inner := result (Tx.exec tx (q "FAILV 1062")) )
                in
                (!inner, result t) )
          in
          equal string
            ( "Constraint_violation Unique: Duplicate entry '" ^ secret
            ^ "' for key" )
            (fst r) ;
          equal string "ok" (snd r) ;
          is_true
            (List.exists
               (function
                 | Ev.Query
                     { outcome=
                         Ev.Failed (E.Constraint_violation {message= ""; _})
                     ; _ } ->
                     true
                 | _ ->
                     false )
               events ) ;
          List.iter
            (fun t -> is_false ~msg:t (contains t secret))
            (event_texts events) )
    ; test "a Connect event keeps the message of the client" (fun () ->
          let r, events =
            run (fun db pool ->
                db.D.faults.refuse <- true ;
                result (Db.exec ~timeout:(Span.ms 300) pool (q "SELECT 1")) )
          in
          equal string "Connect: the server closed the connection" r ;
          is_true
            (List.exists
               (function
                 | Ev.Connect
                     { error= Some (E.Connect "the server closed the connection")
                     ; _ } ->
                     true
                 | _ ->
                     false )
               events ) ) ]
