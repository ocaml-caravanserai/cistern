(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

open Weft
module Connection = Cistern_mysql.Connection
module Message = Cistern_mysql.Message
module Error = Cistern_core.Error
module Event = Cistern_core.Event
module Raw = Cistern_core.Raw
module R = Tx_rules

type t =
  { pool: Pool.t
  ; c: Connection.t
  ; fiber: Fiber.Id.t
  ; deadline: Span.t  (** The transaction deadline, a time of the pool. *)
  ; rollback_on_timeout: bool
  ; mutable active: bool
  ; mutable failed: R.first option
  ; mutable next_savepoint: int
  ; mutable raw_ran: bool
  ; mutable only_dml: bool }

let in_trans c = Connection.status c land Message.status_bit In_trans <> 0

let emit pool ~sql ~t0 outcome =
  Pool.emit pool
    (Event.Query
       {sql; duration= Time.diff Clock.Mono (Time.now Clock.Mono) t0; outcome}
    )

let check tx name =
  if not (Fiber.Id.equal (Fiber.self ()) tx.fiber) then
    invalid_arg ("Tx." ^ name ^ " from another fiber than the fiber of the Tx") ;
  if not tx.active then
    invalid_arg ("Tx." ^ name ^ " after the end of the transaction")

let deadline_passed tx deadline = Span.compare (Pool.now tx.pool) deadline >= 0

(* The first error stays: a later error does not replace it. [at] is the
   deadline of the command: if it passed, a lost DML statement gives Timeout
   (D5). *)
let fail tx ~at e =
  if tx.failed = None then
    tx.failed <-
      Some (R.first ~only_dml:tx.only_dml ~deadline:(deadline_passed tx at) e)

(* D7: a cancel in a command of the Tx. The command broke the connection, which
   closed its flow, and the checkin of the end discards it. The server rolls
   back at the close, thus the first error is Connection_lost, except for a sent
   statement that is not DML, whose outcome is not known. A cancel by the timer
   of the transaction deadline (the deadline passed) stores Timeout in place of
   Connection_lost. Usually the flow deadline of the statement fires first,
   because the timer reads the time later, but the result does not depend on
   this order. *)
let cancelled tx e =
  let e =
    match e with
    | Error.Connection_lost when deadline_passed tx tx.deadline ->
        Error.Timeout
    | e ->
        e
  in
  fail tx ~at:tx.deadline e

(* The error of a command (D6). [strict]: each error fails the Tx (a release or
   a rollback to a savepoint). *)
let command_error tx ~deadline ~strict ~event e =
  let failed e = fail tx ~at:deadline e ; event (Event.Failed e) ; Error e in
  let usable = Connection.usable tx.c in
  match
    if strict then R.Fails
    else
      R.classify ~usable ~rollback_on_timeout:tx.rollback_on_timeout
        ~statement_deadline:
          (false
          [@mutate
            off
              "a Timeout of a statement on a usable connection comes only from \
               the margin, which exec handles before"] )
        e
  with
  | R.Fails ->
      failed e
  | R.Statement_only ->
      event (Event.Failed e) ; Error e
  | R.Ping -> (
    match Connection.ping tx.c with
    | Ok () -> (
      match R.after_ping ~in_trans:(in_trans tx.c) with
      | Some e' ->
          failed e'
      | None ->
          event (Event.Failed e) ; Error e )
    | Error _ ->
        failed e
    | exception (Fiber.Cancelled _ as ex) ->
        cancelled tx Error.Connection_lost ;
        event (Event.Cancelled {sent= true}) ;
        raise ex )

(* An OK with IN_TRANS clear: the server committed (an implicit commit). *)
let committed_in tx ~event =
  fail tx ~at:tx.deadline Error.Implicit_commit ;
  event (Event.Failed Error.Implicit_commit) ;
  Error Error.Implicit_commit

(* One command of the client in the Tx (a savepoint command), under the
   transaction deadline, with the margin check. *)
let command tx ~strict sql =
  let t0 = Time.now Clock.Mono in
  let event = emit tx.pool ~sql ~t0 in
  if not (Pool.time_left tx.pool ~deadline:tx.deadline) then (
    fail tx ~at:tx.deadline Error.Timeout ;
    event (Event.Failed Error.Timeout) ;
    Error Error.Timeout )
  else (
    Connection.set_deadline tx.c (Pool.mono tx.pool tx.deadline) ;
    match Connection.exec tx.c (Raw.sql sql) with
    | Ok _ ->
        if in_trans tx.c then (
          event (Event.Done {rows= 0}) ;
          Ok () )
        else committed_in tx ~event
    | Error e ->
        command_error tx ~deadline:tx.deadline ~strict ~event e
    | exception (Fiber.Cancelled _ as ex) ->
        let sent = Connection.sent tx.c in
        cancelled tx Error.Connection_lost ;
        event (Event.Cancelled {sent}) ;
        raise ex )

let exec ?timeout tx stmt =
  check tx "exec" ;
  match tx.failed with
  | Some f ->
      Error f.error
  | None -> (
      let meta = Stmt.Private.meta stmt in
      let word = Option.map Raw.first_word meta.raw in
      ( match word with
      | Some w when R.refused w ->
          invalid_arg
            "Tx.exec: a raw statement that a Tx refuses (DESIGN.md, \
             \"Transactions\")"
      | _ ->
          () ) ;
      let dialect = (Connection.info tx.c).dialect in
      let t0 = Time.now Clock.Mono in
      let event =
        emit tx.pool ~sql:(String.concat "; " (meta.redacted dialect)) ~t0
      in
      match meta.text with
      | Some (Error message) ->
          let e = Error.Invalid_value {code= None; message} in
          event (Event.Failed e) ; Error e
      | _ -> (
          let deadline =
            match timeout with
            | Some s ->
                let d = Pool.add_span (Pool.now tx.pool) s in
                Span.of_ns (Int64.min (Span.to_ns d) (Span.to_ns tx.deadline))
            | None ->
                tx.deadline
          in
          if not (Pool.time_left tx.pool ~deadline) then (
            (* C-M4: the margin of an earlier statement deadline fails only the
               statement. *)
            if Span.compare deadline tx.deadline >= 0 then
              fail tx ~at:deadline Error.Timeout ;
            event (Event.Failed Error.Timeout) ;
            Error Error.Timeout )
          else
            let dml = match word with Some w -> R.dml w | None -> true in
            if meta.raw <> None then tx.raw_ran <- true ;
            if not dml then tx.only_dml <- false ;
            (* No wait between the margin check and the statement. *)
            Connection.set_deadline tx.c (Pool.mono tx.pool deadline) ;
            let ctx = Exec.make tx.c in
            match Stmt.Private.run stmt ctx with
            | Ok ran ->
                if in_trans tx.c then (
                  event (Event.Done {rows= ran.rows}) ;
                  (* The value comes after the event, as in Db.exec. *)
                  Ok (ran.finish ()) )
                else committed_in tx ~event
            | Error e ->
                command_error tx ~deadline ~strict:false ~event e
            | exception (Fiber.Cancelled _ as ex) ->
                let sent = Exec.sent ctx in
                cancelled tx
                  ( if sent && not dml then Error.Outcome_unknown `Statement
                    else Error.Connection_lost ) ;
                event (Event.Cancelled {sent}) ;
                raise ex ) )

let exec_exn ?timeout tx stmt =
  match exec ?timeout tx stmt with
  | Ok v ->
      v
  | Error e ->
      raise (Error.Cistern_error e)

let state tx =
  match tx.failed with
  | None ->
      R.Good
  | Some f ->
      R.Failed (f, if Connection.usable tx.c then `Usable else `Aborted)

(* The outcome of a callback, with the exception to raise again. *)
type 'a caught =
  | Got of 'a
  | Cistern of Error.t
  | Timeout_exn of Printexc.raw_backtrace
  | Other of exn * Printexc.raw_backtrace

let catch f =
  match f () with
  | v ->
      Got v
  | exception Error.Cistern_error e ->
      Cistern e
  | exception Fiber.Timeout ->
      Timeout_exn (Printexc.get_raw_backtrace ())
  | exception ex ->
      Other (ex, Printexc.get_raw_backtrace ())

let kind = function
  | Got _ ->
      R.Value
  | Cistern e ->
      R.Cistern e
  | Timeout_exn _ ->
      R.Fiber_timeout
  | Other _ ->
      R.Other

let reraise = function
  | Timeout_exn bt ->
      Printexc.raise_with_backtrace Fiber.Timeout bt
  | Other (ex, bt) ->
      Printexc.raise_with_backtrace ex bt
  | Got _ | Cistern _ ->
      invalid_arg "Tx.reraise: no exception"

let cancelled () =
  match Fiber.check_cancel () with
  | () ->
      None
  | exception (Fiber.Cancelled _ as ex) ->
      Some ex

let savepoint tx g =
  check tx "savepoint" ;
  match tx.failed with
  | Some f ->
      Error f.error
  | None -> (
      tx.next_savepoint <- tx.next_savepoint + 1 ;
      let name = Printf.sprintf "cistern_sp_%d" tx.next_savepoint in
      match command tx ~strict:false ("SAVEPOINT " ^ name) with
      | Error e ->
          Error e
      | Ok () -> (
          let outcome = catch (fun () -> g tx) in
          let cancel = cancelled () in
          let rollback_to () =
            ignore (command tx ~strict:true ("ROLLBACK TO SAVEPOINT " ^ name))
          in
          match
            R.end_of_savepoint ~cancelled:(cancel <> None) (state tx)
              (kind outcome)
          with
          | R.Sp_cancel ->
              raise (Option.get cancel)
          | R.Sp_reraise rollback ->
              if rollback then rollback_to () ;
              reraise outcome
          | R.Sp_fail (rollback, e) ->
              if rollback then rollback_to () ;
              Error e
          | R.Release -> (
            match command tx ~strict:true ("RELEASE SAVEPOINT " ^ name) with
            | Ok () -> (
              match outcome with Got v -> Ok v | _ -> assert false )
            | Error e ->
                Error e ) ) )

(* COMMIT or ROLLBACK, in a shield. The flow deadline is the remaining time of
   the transaction, at least 5 s and at most 30 s, so that the sent rule holds.
   The outer with_timeout, 1 s later, is only a guard. *)
let end_command pool c ~deadline sql =
  let left =
    let ns = Int64.sub (Span.to_ns deadline) (Span.to_ns (Pool.now pool)) in
    Span.of_ns
      (Int64.max
         (Span.to_ns (Span.s 5))
         (Int64.min (Span.to_ns (Span.s 30)) ns) )
  in
  Fiber.shield (fun () ->
      Connection.set_deadline c (Time.add Clock.Mono (Time.now Clock.Mono) left) ;
      ( match
          Fiber.with_timeout Clock.Mono
            (Span.of_ns (Int64.add (Span.to_ns left) (Span.to_ns (Span.s 1))))
            (fun () -> Connection.exec c (Raw.sql sql))
        with
      | r ->
          `Done r
      | exception Fiber.Timeout ->
          `Guard )
      [@mutate off "the guard: the flow deadline always comes 1 s before it"] )

(* ROLLBACK at the end: its event (after the checkin) and the end of the entry.
   A ROLLBACK that fails aborts the connection, and never changes the result. *)
let rollback pool c ~deadline ~dirty =
  let t0 = Time.now Clock.Mono in
  let event = emit pool ~sql:"ROLLBACK" ~t0 in
  match end_command pool c ~deadline "ROLLBACK" with
  | `Done (Ok _) ->
      ((fun () -> event (Event.Done {rows= 0})), Pool.Give_back {dirty})
  | `Done (Error e) ->
      ((fun () -> event (Event.Failed e)), Pool.Discard)
  | `Guard ->
      (((fun () -> event (Event.Failed Error.Timeout)), Pool.Discard)
      [@mutate off "the guard: the flow deadline always comes 1 s before it"] )

let commit pool c ~deadline ~dirty =
  let t0 = Time.now Clock.Mono in
  let event = emit pool ~sql:"COMMIT" ~t0 in
  let reply =
    match end_command pool c ~deadline "COMMIT" with
    | `Done (Ok _) ->
        R.commit_reply (Ok (in_trans c))
    | `Done (Error e) ->
        R.commit_reply (Error e)
    | `Guard ->
        {R.result= Error R.commit_guard; rollback= false; reusable= false}
        [@mutate off "the guard: the flow deadline always comes 1 s before it"]
  in
  let commit_event () =
    match reply.result with
    | Ok () ->
        event (Event.Done {rows= 0})
    | Error e ->
        event (Event.Failed e)
  in
  let events, held_end =
    if reply.rollback then
      let rb_event, held_end = rollback pool c ~deadline ~dirty in
      ((fun () -> commit_event () ; rb_event ()), held_end)
    else if reply.reusable then (
      (* After an ERR of COMMIT, the status flags are those of the last OK
         (IN_TRANS set). A ping reads them again, so that the checkin keeps the
         connection. It runs in a shield, under the flow deadline of the end: a
         cancel that came during COMMIT does not stop it. A failed ping breaks
         the connection, and the checkin then aborts it. *)
      ( match reply.result with
      | Error _ ->
          Fiber.shield (fun () ->
              match Connection.ping c with Ok () | Error _ -> () )
      | Ok () ->
          () ) ;
      (commit_event, Pool.Give_back {dirty}) )
    else (commit_event, Pool.Discard)
  in
  (reply.result, events, held_end)

(* The checkout raises Invalid_argument on a closed pool, before any event. *)
let transaction ~timeout pool f =
  (* Before each other check, as in Db.exec. *)
  Pool.check_open pool ;
  if Pool.in_tx pool then invalid_arg "Db.transaction in the fiber of a Tx" ;
  let me = Fiber.self () in
  let body () =
    Fiber.check_cancel () ;
    let start = Pool.now pool in
    let deadline = Pool.add_span start timeout in
    let t0 = Time.now Clock.Mono in
    let begin_event = emit pool ~sql:"START TRANSACTION" ~t0 in
    let run (held : Pool.held) =
      let c = held.c in
      (* No wait between the margin check of with_conn and this command. *)
      match Connection.exec c (Raw.sql "START TRANSACTION") with
      | exception (Fiber.Cancelled _ as ex) ->
          let sent = Connection.sent c in
          ( Pool.Cancel ex
          , (fun () -> begin_event (Event.Cancelled {sent}))
          , Pool.Discard )
      | Error e ->
          ( Pool.Err e
          , (fun () -> begin_event (Event.Failed e))
          , Pool.Give_back {dirty= held.dirty} )
      | Ok _ when not (in_trans c) ->
          ( Pool.Err Error.Connection_lost
          , (fun () -> begin_event (Event.Failed Error.Connection_lost))
          , Pool.Discard )
      | Ok _ -> (
          begin_event (Event.Done {rows= 0}) ;
          let tx =
            { pool
            ; c
            ; fiber= me
            ; deadline
            ; rollback_on_timeout=
                (Connection.info c).innodb_rollback_on_timeout
            ; active= true
            ; failed= None
            ; next_savepoint= 0
            ; raw_ran= false
            ; only_dml= true }
          in
          let left =
            Span.of_ns
              (Int64.sub (Span.to_ns deadline) (Span.to_ns (Pool.now pool)))
          in
          (* C-M2: the callback runs in the fiber of the caller, with the
             transaction deadline. *)
          let outcome =
            catch (fun () ->
                Fiber.with_timeout Clock.Mono left (fun () -> f tx) )
          in
          let deadline_passed = deadline_passed tx deadline in
          let cancel = cancelled () in
          tx.active <- false ;
          let dirty = held.dirty || tx.raw_ran in
          let finish = function
            | R.Rollback ->
                rollback pool c ~deadline ~dirty
            | R.Nothing ->
                (ignore, Pool.Discard)
          in
          match
            R.end_of_callback ~cancelled:(cancel <> None) ~deadline_passed
              (state tx) (kind outcome)
          with
          | R.Cancel ->
              (* The discard aborts the connection, with no COMMIT. *)
              (Pool.Cancel (Option.get cancel), ignore, Pool.Discard)
          | R.Reraise command -> (
              let event, held_end = finish command in
              match outcome with
              | Timeout_exn bt ->
                  (Pool.Reraise (Fiber.Timeout, bt), event, held_end)
              | Other (ex, bt) ->
                  (Pool.Reraise (ex, bt), event, held_end)
              | Got _ | Cistern _ ->
                  assert false )
          | R.Fail (command, e) ->
              let event, held_end = finish command in
              (Pool.Err e, event, held_end)
          | R.Commit -> (
              let result, event, held_end = commit pool c ~deadline ~dirty in
              match (result, outcome) with
              | Ok (), Got v ->
                  (Pool.Value v, event, held_end)
              | Error e, _ ->
                  (Pool.Err e, event, held_end)
              | Ok (), _ ->
                  assert false ) )
    in
    match Pool.with_conn pool ~start ~deadline run with
    | Pool.Checkout_failed e ->
        begin_event (Event.Failed e) ;
        Error e
    | Pool.Margin _ ->
        begin_event (Event.Failed Error.Timeout) ;
        Error Error.Timeout
    | Pool.Reset_failed (_, e) ->
        begin_event (Event.Failed e) ;
        Error e
    | Pool.Cancelled_before ex ->
        begin_event (Event.Cancelled {sent= false}) ;
        raise ex
    | Pool.Ran (outcome, event) -> (
        event () ;
        match outcome with
        | Pool.Value v ->
            Ok v
        | Pool.Err e ->
            Error e
        | Pool.Reraise (ex, bt) ->
            Printexc.raise_with_backtrace ex bt
        | Pool.Cancel ex ->
            raise ex )
  in
  (* D12: one match clears the set on each path. *)
  Pool.enter_tx pool me ;
  match body () with
  | v ->
      Pool.leave_tx pool me ; v
  | exception ex ->
      let bt = Printexc.get_raw_backtrace () in
      Pool.leave_tx pool me ;
      Printexc.raise_with_backtrace ex bt

module Private = struct
  let transaction = transaction
end
