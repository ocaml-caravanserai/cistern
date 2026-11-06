(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

type kind = Unique | Foreign_key | Not_null | Check | Other_constraint

type t =
  | Not_found
  | Constraint_violation of {kind: kind; message: string}
  | Invalid_value of {code: int option; message: string}
  | Retryable of [`Deadlock | `Lock_timeout | `Rolled_back]
  | Statement_timeout
  | Timeout
  | Pool_exhausted
  | Outcome_unknown of [`Statement | `Commit]
  | Implicit_commit
  | Connection_lost
  | Read_only
  | Connect of string
  | Decode of string
  | Driver of {code: int; sqlstate: string; message: string}

let kind_name = function
  | Unique ->
      "Unique"
  | Foreign_key ->
      "Foreign_key"
  | Not_null ->
      "Not_null"
  | Check ->
      "Check"
  | Other_constraint ->
      "Other_constraint"

(* A SQLSTATE of the protocol: 5 characters of [0-9A-Z]. A summary prints no
   other text, because the text comes from the server. *)
let valid_sqlstate s =
  String.length s = 5
  && String.for_all (fun c -> Char.Ascii.is_digit c || Char.Ascii.is_upper c) s

let pp_summary ppf = function
  | Not_found ->
      Format.pp_print_string ppf "Not_found"
  | Constraint_violation {kind; _} ->
      Format.fprintf ppf "Constraint_violation %s" (kind_name kind)
  | Invalid_value {code= Some code; _} ->
      Format.fprintf ppf "Invalid_value %d" code
  | Invalid_value {code= None; _} ->
      Format.pp_print_string ppf "Invalid_value"
  | Retryable `Deadlock ->
      Format.pp_print_string ppf "Retryable deadlock"
  | Retryable `Lock_timeout ->
      Format.pp_print_string ppf "Retryable lock timeout"
  | Retryable `Rolled_back ->
      Format.pp_print_string ppf "Retryable rolled back"
  | Statement_timeout ->
      Format.pp_print_string ppf "Statement_timeout"
  | Timeout ->
      Format.pp_print_string ppf "Timeout"
  | Pool_exhausted ->
      Format.pp_print_string ppf "Pool_exhausted"
  | Outcome_unknown `Statement ->
      Format.pp_print_string ppf "Outcome_unknown statement"
  | Outcome_unknown `Commit ->
      Format.pp_print_string ppf "Outcome_unknown commit"
  | Implicit_commit ->
      Format.pp_print_string ppf "Implicit_commit"
  | Connection_lost ->
      Format.pp_print_string ppf "Connection_lost"
  | Read_only ->
      Format.pp_print_string ppf "Read_only"
  | Connect _ ->
      Format.pp_print_string ppf "Connect"
  | Decode _ ->
      Format.pp_print_string ppf "Decode"
  | Driver {code; sqlstate; _} when valid_sqlstate sqlstate ->
      Format.fprintf ppf "Driver %d %s" code sqlstate
  | Driver {code; _} ->
      Format.fprintf ppf "Driver %d" code

(* The message of the server or of the client, if the constructor has one. *)
let message = function
  | Constraint_violation {message; _}
  | Invalid_value {message; _}
  | Connect message
  | Decode message
  | Driver {message; _} ->
      Some message
  | Not_found
  | Retryable _
  | Statement_timeout
  | Timeout
  | Pool_exhausted
  | Outcome_unknown _
  | Implicit_commit
  | Connection_lost
  | Read_only ->
      None

let pp ppf e =
  pp_summary ppf e ;
  Option.iter (Format.fprintf ppf ": %s") (message e)

let to_string e = Format.asprintf "%a" pp e

exception Cistern_error of t

(* Only the registration runs at the initialization of the module. The function
   runs in the tests, thus its mutants are tested. *)
let () =
  Printexc.register_printer (function
    | Cistern_error e ->
        Some (Format.asprintf "Cistern_error: %a" pp_summary e)
    | _ ->
        None )
