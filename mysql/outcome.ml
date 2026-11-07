(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

module Error = Cistern_core.Error

type command = Write | Not_write

type event =
  | Write_failed
  | Write_deadline
  | Read_deadline
  | Before_command
  | Stream_ended
  | Bad_packet of string
  | Local_infile
  | Server_error of Message.err
  | Bad_value of string

type state = Usable | Broken

(* A command with no clear reply. *)
let no_reply = function
  | Write ->
      Error.Outcome_unknown `Statement
  | Not_write ->
      Error.Connection_lost

let packet_too_large = 1153

let decide dialect command = function
  | Write_failed | Before_command ->
      (Error.Connection_lost, Broken)
  | Write_deadline ->
      (Error.Timeout, Broken)
  | Read_deadline ->
      ( ( match command with
        | Write ->
            Error.Outcome_unknown `Statement
        | Not_write ->
            Error.Timeout )
      , Broken )
  | Stream_ended | Bad_packet _ ->
      (no_reply command, Broken)
  | Local_infile ->
      (Error.Decode "LOCAL INFILE request", Broken)
  | Server_error {code; sqlstate; message} -> (
    match Error.of_server dialect ~code ~sqlstate ~message with
    | Error.Connection_lost ->
        (no_reply command, Broken)
    | e ->
        (e, if code = packet_too_large then Broken else Usable) )
  | Bad_value m ->
      (Error.Decode m, Usable)
