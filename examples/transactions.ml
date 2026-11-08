(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* A transaction, and a retry of a whole transaction. *)

open Cistern
open Blog

(* README begin: transactions *)
(* The posts of the author are locked until the end of the transaction. A
   [Cistern_error] from [Tx.exec_exn] fails the transaction: [Db.transaction]
   rolls back and gives the error. *)
let publish_all db aid =
  Db.transaction db (fun tx ->
      let posts =
        Post.where Post.Q.(author_id ==. aid && published ==. false)
        |> Query.for_update |> Query.to_list |> Tx.exec_exn tx
      in
      List.iter
        (fun p -> ignore (Post.update ~published:true p |> Tx.exec_exn tx))
        posts ;
      List.length posts )
(* README end *)

(* README begin: retry *)
(* [Retryable] (a deadlock, for example): the transaction was rolled back, by
   the server or by Db.transaction when the error left the callback. Thus a new
   run of the whole transaction is safe. Cistern has no helper for this. Do not
   retry [Outcome_unknown] or [Implicit_commit]: a part of the transaction can
   be committed. *)
let rec with_retry ?(tries = 3) db f =
  match Db.transaction db f with
  | Error (Error.Retryable _) when tries > 1 ->
      with_retry ~tries:(tries - 1) db f
  | result ->
      result
(* README end *)

let run db =
  let a =
    Author.create ~name:"Lin" ~email:"lin@example.com"
      ~posts:[Post.draft ~title:"A" (); Post.draft ~title:"B" ()]
      ()
    |> Db.exec_exn db
  in
  assert (publish_all db a.id = Ok 2) ;
  assert (publish_all db a.id = Ok 0) ;
  (* A transaction that fails one time with a deadlock, then commits. *)
  let runs = ref 0 in
  let r =
    with_retry db (fun _tx ->
        incr runs ;
        if !runs = 1 then raise (Cistern_error (Error.Retryable `Deadlock)) ;
        !runs )
  in
  assert (r = Ok 2) ;
  (* No retry of Outcome_unknown and Implicit_commit: one run each. *)
  List.iter
    (fun e ->
      let runs = ref 0 in
      let r = with_retry db (fun _tx -> incr runs ; raise (Cistern_error e)) in
      assert (r = Error e) ;
      assert (!runs = 1) )
    [Error.Outcome_unknown `Statement; Error.Implicit_commit]
