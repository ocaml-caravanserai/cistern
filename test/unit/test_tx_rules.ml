(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* Tx_rules: the tables of the plan of phase 1.3 (3.5 to 3.9), one row each. *)

open Windtrap
module R = Cistern.Private.Tx_rules
module E = Cistern_core.Error

let e_text e = Format.asprintf "%a" E.pp_summary e

let driver code = E.Driver {code; sqlstate= "HY000"; message= "m"}

let klass_text = function
  | R.Fails ->
      "fail"
  | R.Statement_only ->
      "statement"
  | R.Ping ->
      "ping"

let constraint_ = E.Constraint_violation {kind= Unique; message= "m"}

let invalid code = E.Invalid_value {code; message= "m"}

(* Each error, with its class on a usable connection, with
   innodb_rollback_on_timeout off and no deadline of the statement. *)
let classes =
  [ (E.Retryable `Deadlock, "fail")
  ; (E.Retryable `Lock_timeout, "ping")
  ; (E.Retryable `Rolled_back, "fail")
  ; (E.Read_only, "fail")
  ; (E.Outcome_unknown `Statement, "fail")
  ; (E.Outcome_unknown `Commit, "fail")
  ; (E.Connection_lost, "fail")
  ; (E.Implicit_commit, "fail")
  ; (E.Pool_exhausted, "fail")
  ; (E.Connect "x", "fail")
  ; (invalid (Some 1153), "fail")
  ; (E.Timeout, "fail")
  ; (constraint_, "statement")
  ; (invalid None, "statement")
  ; (invalid (Some 1366), "statement")
  ; (E.Statement_timeout, "statement")
  ; (E.Not_found, "statement")
  ; (E.Decode "x", "statement")
  ; (driver 1146, "ping")
  ; (driver 1050, "ping") ]

let first_text (f : R.first) =
  Printf.sprintf "%s/%s/%b" (e_text f.error) (e_text f.final) f.do_not_retry

let command_text = function R.Rollback -> "ROLLBACK" | R.Nothing -> "none"

let action_text = function
  | R.Cancel ->
      "cancel"
  | R.Reraise c ->
      command_text c ^ ", raise"
  | R.Fail (c, e) ->
      command_text c ^ ", " ^ e_text e
  | R.Commit ->
      "COMMIT"

let sp_text = function
  | R.Sp_cancel ->
      "cancel"
  | R.Sp_reraise b ->
      (if b then "ROLLBACK TO" else "none") ^ ", raise"
  | R.Sp_fail (b, e) ->
      (if b then "ROLLBACK TO" else "none") ^ ", " ^ e_text e
  | R.Release ->
      "RELEASE"

(* The first errors of the end rows. *)
let deadlock = R.first ~only_dml:true ~deadline:false (E.Retryable `Deadlock)

let implicit = R.first ~only_dml:true ~deadline:false E.Implicit_commit

let dml_lost =
  R.first ~only_dml:true ~deadline:true (E.Outcome_unknown `Statement)

let user_err = E.Not_found

let commit_text (c : R.commit) =
  Printf.sprintf "%s rollback=%b reusable=%b"
    (match c.result with Ok () -> "ok" | Error e -> e_text e)
    c.rollback c.reusable

let tests =
  group "Tx_rules"
    [ cases "classify: each error on a usable connection"
        ~id:(fun (e, _) -> e_text e)
        classes
        (fun (e, expected) ->
          equal string expected
            (klass_text
               (R.classify ~usable:true ~rollback_on_timeout:false
                  ~statement_deadline:false e ) ) )
    ; test "classify: a connection that is not usable fails, with no ping"
        (fun () ->
          List.iter
            (fun (e, _) ->
              equal string "fail"
                (klass_text
                   (R.classify ~usable:false ~rollback_on_timeout:false
                      ~statement_deadline:true e ) ) )
            classes )
    ; test "classify: 1205 with innodb_rollback_on_timeout fails" (fun () ->
          equal string "fail"
            (klass_text
               (R.classify ~usable:true ~rollback_on_timeout:true
                  ~statement_deadline:false (E.Retryable `Lock_timeout) ) ) )
    ; test "classify: the margin of a statement deadline is statement-only"
        (fun () ->
          equal string "statement"
            (klass_text
               (R.classify ~usable:true ~rollback_on_timeout:false
                  ~statement_deadline:true E.Timeout ) ) ;
          (* The flag changes only Timeout. *)
          equal string "fail"
            (klass_text
               (R.classify ~usable:true ~rollback_on_timeout:false
                  ~statement_deadline:true E.Connection_lost ) ) )
    ; test "after_ping: IN_TRANS clear gives Implicit_commit" (fun () ->
          equal (option string) (Some "Implicit_commit")
            (Option.map e_text (R.after_ping ~in_trans:false)) ;
          equal (option string) None
            (Option.map e_text (R.after_ping ~in_trans:true)) )
    ; cases "refused"
        ~id:(fun (w, _) -> Option.value ~default:"None" w)
        [ (None, true)
        ; (Some "BEGIN", true)
        ; (Some "start", true)
        ; (Some "Commit", true)
        ; (Some "ROLLBACK", true)
        ; (Some "SAVEPOINT", true)
        ; (Some "RELEASE", true)
        ; (Some "xa", true)
        ; (Some "LOCK", true)
        ; (Some "UNLOCK", true)
        ; (Some "SET", true)
        ; (Some "EXECUTE", true)
        ; (Some "PREPARE", true)
        ; (Some "SETTINGS", false)
        ; (Some "SELECT", false)
        ; (Some "INSERT", false)
        ; (Some "CALL", false)
        ; (Some "TRUNCATE", false) ]
        (fun (w, expected) -> equal bool expected (R.refused w))
    ; cases "dml"
        ~id:(fun (w, _) -> Option.value ~default:"None" w)
        [ (None, false)
        ; (Some "insert", true)
        ; (Some "UPDATE", true)
        ; (Some "DELETE", true)
        ; (Some "REPLACE", true)
        ; (Some "Select", true)
        ; (Some "WITH", true)
        ; (Some "VALUES", true)
        ; (Some "TABLE", true)
        ; (Some "TRUNCATE", false)
        ; (Some "CALL", false)
        ; (Some "CREATE", false)
        ; (Some "SHOW", false) ]
        (fun (w, expected) -> equal bool expected (R.dml w))
    ; cases "first: the rule D5 and do-not-retry"
        ~id:(fun (n, _, _) -> n)
        [ ( "a DML statement lost by a deadline"
          , dml_lost
          , "Outcome_unknown statement/Timeout/false" )
        ; ( "a DML statement lost by the end of the stream"
          , R.first ~only_dml:true ~deadline:false (E.Outcome_unknown `Statement)
          , "Outcome_unknown statement/Connection_lost/false" )
        ; ( "a statement that is not DML"
          , R.first ~only_dml:false ~deadline:true (E.Outcome_unknown `Statement)
          , "Outcome_unknown statement/Outcome_unknown statement/true" )
        ; ( "Outcome_unknown of COMMIT"
          , R.first ~only_dml:true ~deadline:false (E.Outcome_unknown `Commit)
          , "Outcome_unknown commit/Outcome_unknown commit/true" )
        ; ("Implicit_commit", implicit, "Implicit_commit/Implicit_commit/true")
        ; ("a deadlock", deadlock, "Retryable deadlock/Retryable deadlock/false")
        ]
        (fun (_, f, expected) -> equal string expected (first_text f))
    ; cases "end_of_callback: the priority rule (3.6.1)"
        ~id:(fun (n, _, _, _, _, _) -> n)
        [ ("1 a cancel", true, false, R.Good, R.Value, "cancel")
        ; ( "1 a cancel, also with an exception"
          , true
          , true
          , R.Failed (implicit, `Usable)
          , R.Other
          , "cancel" )
        ; ( "2 a user exception"
          , false
          , false
          , R.Good
          , R.Other
          , "ROLLBACK, raise" )
        ; ( "2 a user exception after a failed DDL"
          , false
          , false
          , R.Failed (implicit, `Usable)
          , R.Other
          , "ROLLBACK, raise" )
        ; ( "2 a user exception after an abort"
          , false
          , true
          , R.Failed (deadlock, `Aborted)
          , R.Other
          , "none, raise" )
        ; ( "2 a Fiber.Timeout before the deadline"
          , false
          , false
          , R.Good
          , R.Fiber_timeout
          , "ROLLBACK, raise" )
        ; ( "3 a failed DDL, then the deadline"
          , false
          , true
          , R.Failed (implicit, `Usable)
          , R.Fiber_timeout
          , "ROLLBACK, Implicit_commit" )
        ; ( "3 a failed DDL, then a Cistern_error"
          , false
          , false
          , R.Failed (implicit, `Usable)
          , R.Cistern user_err
          , "ROLLBACK, Implicit_commit" )
        ; ( "3 a failed DDL, then a value"
          , false
          , false
          , R.Failed (implicit, `Aborted)
          , R.Value
          , "none, Implicit_commit" )
        ; ( "4 a Cistern_error after a deadlock"
          , false
          , false
          , R.Failed (deadlock, `Usable)
          , R.Cistern user_err
          , "ROLLBACK, Not_found" )
        ; ( "4 a Cistern_error after the deadline"
          , false
          , true
          , R.Good
          , R.Cistern user_err
          , "ROLLBACK, Not_found" )
        ; ( "4 a Cistern_error after an abort"
          , false
          , false
          , R.Failed (dml_lost, `Aborted)
          , R.Cistern user_err
          , "none, Not_found" )
        ; ( "4 a Cistern_error, good"
          , false
          , false
          , R.Good
          , R.Cistern user_err
          , "ROLLBACK, Not_found" )
        ; ( "5 a value after the deadline"
          , false
          , true
          , R.Good
          , R.Value
          , "ROLLBACK, Timeout" )
        ; ( "5 the Fiber.Timeout of the deadline"
          , false
          , true
          , R.Good
          , R.Fiber_timeout
          , "ROLLBACK, Timeout" )
        ; ( "5 the deadline after a DML statement lost"
          , false
          , true
          , R.Failed (dml_lost, `Aborted)
          , R.Fiber_timeout
          , "none, Timeout" )
        ; ( "5 the deadline after a deadlock"
          , false
          , true
          , R.Failed (deadlock, `Usable)
          , R.Value
          , "ROLLBACK, Retryable deadlock" )
        ; ( "6 a value after a deadlock"
          , false
          , false
          , R.Failed (deadlock, `Usable)
          , R.Value
          , "ROLLBACK, Retryable deadlock" )
        ; ( "6 a value after a DML statement lost"
          , false
          , false
          , R.Failed (dml_lost, `Aborted)
          , R.Value
          , "none, Timeout" )
        ; ("7 a value, good", false, false, R.Good, R.Value, "COMMIT") ]
        (fun (_, cancelled, deadline_passed, state, outcome, expected) ->
          equal string expected
            (action_text
               (R.end_of_callback ~cancelled ~deadline_passed state outcome) ) )
    ; cases "end_of_savepoint: the table of 3.9"
        ~id:(fun (n, _, _, _, _) -> n)
        [ ("1 a cancel", true, R.Good, R.Value, "cancel")
        ; ( "2 a user exception, good"
          , false
          , R.Good
          , R.Other
          , "ROLLBACK TO, raise" )
        ; ( "2 a Fiber.Timeout, good"
          , false
          , R.Good
          , R.Fiber_timeout
          , "ROLLBACK TO, raise" )
        ; ( "2 a user exception after a failure"
          , false
          , R.Failed (deadlock, `Usable)
          , R.Other
          , "none, raise" )
        ; ( "3 a failed DDL, then a Cistern_error"
          , false
          , R.Failed (implicit, `Usable)
          , R.Cistern user_err
          , "none, Implicit_commit" )
        ; ( "4 a Cistern_error after a deadlock"
          , false
          , R.Failed (deadlock, `Usable)
          , R.Cistern user_err
          , "none, Not_found" )
        ; ( "4 a Cistern_error, good"
          , false
          , R.Good
          , R.Cistern user_err
          , "ROLLBACK TO, Not_found" )
        ; ( "5 a value after a failure: the error of Tx.exec"
          , false
          , R.Failed (dml_lost, `Aborted)
          , R.Value
          , "none, Outcome_unknown statement" )
        ; ("6 a value, good", false, R.Good, R.Value, "RELEASE") ]
        (fun (_, cancelled, state, outcome, expected) ->
          equal string expected
            (sp_text (R.end_of_savepoint ~cancelled state outcome)) )
    ; cases "commit_reply: the table of 3.8"
        ~id:(fun (n, _, _) -> n)
        [ ("an OK", Ok false, "ok rollback=false reusable=true")
        ; ("an OK with IN_TRANS", Ok true, "ok rollback=false reusable=false")
        ; ( "1213"
          , Error (E.Retryable `Deadlock)
          , "Retryable rolled back rollback=false reusable=true" )
        ; ( "1205"
          , Error (E.Retryable `Lock_timeout)
          , "Retryable rolled back rollback=false reusable=true" )
        ; ( "3101"
          , Error (E.Retryable `Rolled_back)
          , "Retryable rolled back rollback=false reusable=true" )
        ; ( "1180"
          , Error (driver 1180)
          , "Retryable rolled back rollback=false reusable=true" )
        ; ( "1317"
          , Error (driver 1317)
          , "Outcome_unknown commit rollback=false reusable=false" )
        ; ( "no reply"
          , Error (E.Outcome_unknown `Statement)
          , "Outcome_unknown commit rollback=false reusable=false" )
        ; ( "not sent, or 4031 with the id 0"
          , Error E.Connection_lost
          , "Connection_lost rollback=false reusable=false" )
        ; ( "a write deadline"
          , Error E.Timeout
          , "Timeout rollback=false reusable=false" )
        ; ( "another error"
          , Error (driver 1146)
          , "Driver 1146 HY000 rollback=true reusable=true" ) ]
        (fun (_, r, expected) ->
          equal string expected (commit_text (R.commit_reply r)) )
    ; test "commit_guard" (fun () ->
          equal string "Outcome_unknown commit" (e_text R.commit_guard) ) ]
