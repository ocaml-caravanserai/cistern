(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* Error: the printers and the exception (DESIGN.md, "Core user types"). *)

open Windtrap
open Cistern_core

(* A message with a marker. No printer for logs may print it. *)
let secret = "Duplicate entry 'SECRET' for key 'email'"

(* Each constructor, the text of pp_summary, and the text of pp. *)
let rows =
  [ ("Not_found", (fun () -> Error.Not_found), "Not_found", "Not_found")
  ; ( "Constraint_violation"
    , (fun () -> Error.Constraint_violation {kind= Unique; message= secret})
    , "Constraint_violation Unique"
    , "Constraint_violation Unique: " ^ secret )
  ; ( "Foreign_key"
    , (fun () -> Error.Constraint_violation {kind= Foreign_key; message= "m"})
    , "Constraint_violation Foreign_key"
    , "Constraint_violation Foreign_key: m" )
  ; ( "Not_null"
    , (fun () -> Error.Constraint_violation {kind= Not_null; message= "m"})
    , "Constraint_violation Not_null"
    , "Constraint_violation Not_null: m" )
  ; ( "Check"
    , (fun () -> Error.Constraint_violation {kind= Check; message= "m"})
    , "Constraint_violation Check"
    , "Constraint_violation Check: m" )
  ; ( "Other_constraint"
    , (fun () ->
        Error.Constraint_violation {kind= Other_constraint; message= "m"} )
    , "Constraint_violation Other_constraint"
    , "Constraint_violation Other_constraint: m" )
  ; ( "Invalid_value with a code"
    , (fun () -> Error.Invalid_value {code= Some 1406; message= secret})
    , "Invalid_value 1406"
    , "Invalid_value 1406: " ^ secret )
  ; ( "Invalid_value with no code"
    , (fun () -> Error.Invalid_value {code= None; message= "m"})
    , "Invalid_value"
    , "Invalid_value: m" )
  ; ( "Retryable deadlock"
    , (fun () -> Error.Retryable `Deadlock)
    , "Retryable deadlock"
    , "Retryable deadlock" )
  ; ( "Retryable lock timeout"
    , (fun () -> Error.Retryable `Lock_timeout)
    , "Retryable lock timeout"
    , "Retryable lock timeout" )
  ; ( "Retryable rolled back"
    , (fun () -> Error.Retryable `Rolled_back)
    , "Retryable rolled back"
    , "Retryable rolled back" )
  ; ( "Statement_timeout"
    , (fun () -> Error.Statement_timeout)
    , "Statement_timeout"
    , "Statement_timeout" )
  ; ("Timeout", (fun () -> Error.Timeout), "Timeout", "Timeout")
  ; ( "Pool_exhausted"
    , (fun () -> Error.Pool_exhausted)
    , "Pool_exhausted"
    , "Pool_exhausted" )
  ; ( "Outcome_unknown statement"
    , (fun () -> Error.Outcome_unknown `Statement)
    , "Outcome_unknown statement"
    , "Outcome_unknown statement" )
  ; ( "Outcome_unknown commit"
    , (fun () -> Error.Outcome_unknown `Commit)
    , "Outcome_unknown commit"
    , "Outcome_unknown commit" )
  ; ( "Implicit_commit"
    , (fun () -> Error.Implicit_commit)
    , "Implicit_commit"
    , "Implicit_commit" )
  ; ( "Connection_lost"
    , (fun () -> Error.Connection_lost)
    , "Connection_lost"
    , "Connection_lost" )
  ; ("Read_only", (fun () -> Error.Read_only), "Read_only", "Read_only")
  ; ( "Connect"
    , (fun () -> Error.Connect secret)
    , "Connect"
    , "Connect: " ^ secret )
  ; ( "Decode"
    , (fun () -> Error.Decode "column 1: m")
    , "Decode"
    , "Decode: column 1: m" )
  ; ( "Driver"
    , (fun () -> Error.Driver {code= 1064; sqlstate= "42000"; message= secret})
    , "Driver 1064 42000"
    , "Driver 1064 42000: " ^ secret )
  ; ( "Driver with an empty SQLSTATE"
    , (fun () -> Error.Driver {code= 1064; sqlstate= ""; message= "m"})
    , "Driver 1064"
    , "Driver 1064: m" )
  ; ( "Driver with a lowercase SQLSTATE"
    , (fun () -> Error.Driver {code= 1064; sqlstate= "4200x"; message= "m"})
    , "Driver 1064"
    , "Driver 1064: m" )
  ; ( "Driver with a SQLSTATE of 6 characters"
    , (fun () -> Error.Driver {code= 1064; sqlstate= "420000"; message= "m"})
    , "Driver 1064"
    , "Driver 1064: m" )
  ; ( "Driver with a SQLSTATE of 4 characters"
    , (fun () -> Error.Driver {code= 1064; sqlstate= "4200"; message= "m"})
    , "Driver 1064"
    , "Driver 1064: m" )
  ; ( "Driver with a space in the SQLSTATE"
    , (fun () -> Error.Driver {code= 1064; sqlstate= "42 00"; message= "m"})
    , "Driver 1064"
    , "Driver 1064: m" ) ]

let summary e = Format.asprintf "%a" Error.pp_summary e

let full e = Format.asprintf "%a" Error.pp e

let tests =
  group "Error"
    [ cases "pp_summary"
        ~id:(fun (n, _, _, _) -> n)
        rows
        (fun (_, e, s, _) ->
          equal string s (summary (e ())) ;
          not_contains ~sub:"SECRET" (summary (e ())) )
    ; cases "pp"
        ~id:(fun (n, _, _, _) -> n)
        rows
        (fun (_, e, _, f) -> equal string f (full (e ())))
    ; test "to_string is the text of pp" (fun () ->
          let e = Error.Driver {code= 1064; sqlstate= "42000"; message= "m"} in
          equal string "Driver 1064 42000: m" (Error.to_string e) )
    ; group "the exception printer"
        [ test "prints the summary" (fun () ->
              equal string "Cistern_error: Driver 1064 42000"
                (Printexc.to_string
                   (Error.Cistern_error
                      (Driver {code= 1064; sqlstate= "42000"; message= secret})
                   ) ) )
        ; test "does not print another exception" (fun () ->
              equal string "Not_found" (Printexc.to_string Stdlib.Not_found) )
        ] ]
