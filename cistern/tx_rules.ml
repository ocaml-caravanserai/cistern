(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

module Error = Cistern_core.Error

type klass = Fails | Statement_only | Ping

let classify ~usable ~rollback_on_timeout ~statement_deadline (e : Error.t) =
  if not usable then Fails
  else
    match e with
    | Retryable `Lock_timeout ->
        if rollback_on_timeout then Fails else Ping
    | Retryable (`Deadlock | `Rolled_back)
    | Read_only
    | Outcome_unknown _
    | Connection_lost
    | Implicit_commit
    | Pool_exhausted
    | Connect _
    | Invalid_value {code= Some 1153; _} ->
        Fails
    | Timeout ->
        if statement_deadline then Statement_only else Fails
    | Constraint_violation _
    | Invalid_value _
    | Statement_timeout
    | Not_found
    | Decode _ ->
        Statement_only
    | Driver _ ->
        Ping

let after_ping ~in_trans = if in_trans then None else Some Error.Implicit_commit

let word_in words = function
  | Some w ->
      List.mem (String.uppercase_ascii w) words
  | None ->
      false

let refused w =
  w = None
  || word_in
       [ "BEGIN"
       ; "START"
       ; "COMMIT"
       ; "ROLLBACK"
       ; "SAVEPOINT"
       ; "RELEASE"
       ; "XA"
       ; "LOCK"
       ; "UNLOCK"
       ; "SET"
       ; "EXECUTE"
       ; "PREPARE" ]
       w

let dml =
  word_in
    [ "INSERT"
    ; "UPDATE"
    ; "DELETE"
    ; "REPLACE"
    ; "SELECT"
    ; "WITH"
    ; "VALUES"
    ; "TABLE" ]

type first = {error: Error.t; final: Error.t; do_not_retry: bool}

let first ~only_dml ~deadline (e : Error.t) =
  match e with
  | Outcome_unknown `Statement when only_dml ->
      { error= e
      ; final= (if deadline then Error.Timeout else Error.Connection_lost)
      ; do_not_retry= false }
  | Implicit_commit | Outcome_unknown _ ->
      {error= e; final= e; do_not_retry= true}
  | _ ->
      {error= e; final= e; do_not_retry= false}

type state = Good | Failed of first * [`Usable | `Aborted]

type outcome = Value | Cistern of Error.t | Fiber_timeout | Other

type command = Rollback | Nothing

type action = Cancel | Reraise of command | Fail of command * Error.t | Commit

let end_of_callback ~cancelled ~deadline_passed state outcome =
  let command =
    match state with
    | Good | Failed (_, `Usable) ->
        Rollback
    | Failed (_, `Aborted) ->
        Nothing
  in
  let first = match state with Good -> None | Failed (f, _) -> Some f in
  if cancelled then Cancel
  else
    match (outcome, first) with
    | Other, _ ->
        Reraise command
    | Fiber_timeout, _ when not deadline_passed ->
        Reraise command
    | _, Some f when f.do_not_retry ->
        Fail (command, f.final)
    | Cistern e, _ ->
        Fail (command, e)
    | (Value | Fiber_timeout), Some f ->
        (* A deadline that passed, or a first error: its final error. *)
        Fail (command, f.final)
    | Fiber_timeout, None ->
        Fail (command, Error.Timeout)
    | Value, None ->
        if deadline_passed then Fail (command, Error.Timeout) else Commit

type savepoint_action =
  | Sp_cancel
  | Sp_reraise of bool
  | Sp_fail of bool * Error.t
  | Release

let end_of_savepoint ~cancelled state outcome =
  let good = state = Good in
  if cancelled then Sp_cancel
  else
    match (outcome, state) with
    | (Other | Fiber_timeout), _ ->
        Sp_reraise good
    | _, Failed (f, _) when f.do_not_retry ->
        Sp_fail (false, f.error)
    | Cistern e, _ ->
        Sp_fail (good, e)
    | Value, Failed (f, _) ->
        Sp_fail (false, f.error)
    | Value, Good ->
        Release

type commit = {result: (unit, Error.t) result; rollback: bool; reusable: bool}

let commit_reply (r : (bool, Error.t) result) =
  match r with
  | Ok in_trans ->
      {result= Ok (); rollback= false; reusable= not in_trans}
  | Error (Retryable _ | Driver {code= 1180; _}) ->
      {result= Error (Retryable `Rolled_back); rollback= false; reusable= true}
  | Error (Driver {code= 1317; _} | Outcome_unknown _) ->
      {result= Error (Outcome_unknown `Commit); rollback= false; reusable= false}
  | Error ((Connection_lost | Timeout) as e) ->
      {result= Error e; rollback= false; reusable= false}
  | Error e ->
      {result= Error e; rollback= true; reusable= true}

let commit_guard = Error.Outcome_unknown `Commit
