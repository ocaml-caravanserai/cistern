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

let interrupts (d : Dialect.t) code =
  match (d, code) with
  | _, (1053 | 1317) | Mysql, 4031 | Mariadb, 1927 ->
      true
  | _ ->
      false

(* True if [part] is in [s]. *)
let contains s part =
  let n = String.length s and k = String.length part in
  let rec at i = i + k <= n && (String.sub s i k = part || at (i + 1)) in
  at 0

(* 1290 names the option that stops the statement ("The MySQL server is running
   with the %s option so it cannot execute this statement"). *)
let read_only_option message =
  contains message "--read-only" || contains message "--super-read-only"

(* The table of DESIGN.md, "Error mapping", in the same order. perror of the
   pinned servers confirmed each number (DESIGN.md). *)
let of_server (d : Dialect.t) ~code ~sqlstate ~message =
  let violation kind = Constraint_violation {kind; message} in
  match (d, code) with
  | _, (1062 | 1586 | 1022 | 1169 | 1859 | 1761 | 1762) ->
      violation Unique
  | _, (1451 | 1452 | 1216 | 1217) ->
      violation Foreign_key
  | _, (1048 | 1364) | Mysql, 3673 ->
      violation Not_null
  | Mysql, 3819 | Mariadb, 4025 ->
      violation Check
  | _, (1406 | 1264 | 1265 | 1365 | 1366 | 1292 | 1690 | 1367 | 1301 | 1153) ->
      Invalid_value {code= Some code; message}
  | _, 1213 ->
      Retryable `Deadlock
  | _, 1205 ->
      Retryable `Lock_timeout
  | Mysql, 3101 ->
      Retryable `Rolled_back
  | Mysql, 3024 | Mariadb, 1969 ->
      Statement_timeout
  | _, 1290 ->
      if read_only_option message then Read_only
      else Driver {code; sqlstate; message}
  | _, (1792 | 1836 | 1874) ->
      Read_only
  | _, (1317 | 1052) ->
      Driver {code; sqlstate; message}
  | _ when interrupts d code ->
      Connection_lost
  | _
    when String.length sqlstate = 5 && String.starts_with ~prefix:"23" sqlstate
    ->
      violation Other_constraint
  | _ when String.equal sqlstate "40001" ->
      Retryable `Deadlock
  | _ ->
      Driver {code; sqlstate; message}

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
