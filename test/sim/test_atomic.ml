(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* The atomic statements of Db.exec (phase 4b1, the plan of phase 4, E4, E5 and
   E7) on Fake_db: the commands run in the transaction of Db.transaction, each
   with its own event; the first error of a command rolls back. *)

open Windtrap
open Weft
module Db = Cistern.Db
module Tx = Cistern.Tx
module D = Fake_db
module T = Test_pool

let contains s p =
  let n = String.length s and k = String.length p in
  let rec at i = i + k <= n && (String.sub s i k = p || at (i + 1)) in
  at 0

let text = function
  | Ok n ->
      "Ok " ^ string_of_int n
  | Error e ->
      T.error_text e

(* The commands of the fake server from [from] on. A ping shows as "ping". *)
let log ~from (db : D.t) =
  List.rev db.log
  |> List.filteri (fun i _ -> i >= from)
  |> List.map (fun s -> if s = "" then "ping" else s)

(* [run ?setup ?timeout ?cancel_ms cmds]: one clean exec (a connection), then
   Db.exec of the atomic statement of [cmds], with a cancel after [cancel_ms],
   then one more clean exec. It gives the result, the commands of the fake
   server of the atomic statement, the connects, and the events of the atomic
   statement. *)
let run ?(setup = ignore) ?(timeout = Span.s 30) ?cancel_ms ?(size = 1) ?around
    ?text:literal cmds =
  let (r, cmds, connects), events =
    T.with_pool ~size ~setup (fun _ db pool ->
        ignore (Db.exec pool (T.clean ())) ;
        let from = List.length db.log in
        let exec () =
          Db.exec ~timeout pool (T.atomic ?around ?text:literal cmds)
        in
        let r =
          match cancel_ms with
          | None ->
              text (exec ())
          | Some ms -> (
            match Fiber.with_timeout Clock.Mono (Span.ms ms) exec with
            | r ->
                text r
            | exception Fiber.Timeout ->
                "cancelled" )
        in
        let cmds = log ~from db in
        db.faults.latency_ms <- 0 ;
        db.faults.commit <- None ;
        db.faults.script <- (fun _ -> None) ;
        ignore (Db.exec pool (T.clean ())) ;
        (r, cmds, db.connects) )
  in
  let events =
    List.filter
      (fun e -> not (String.starts_with ~prefix:"query \"CUSTOM\"" e))
      (T.queries events)
  in
  (r, cmds, connects, events)

let tests =
  group "atomic statements in Db.exec"
    [ test "the commands, the events, and the value" (fun () ->
          let r, cmds, connects, events = run ["ADD a"; "ADD b"] in
          equal string "Ok 2" r ;
          equal (list string)
            ["START TRANSACTION"; "ADD a"; "ADD b"; "COMMIT"]
            cmds ;
          equal int 1 connects ;
          equal (list string)
            [ "query \"START TRANSACTION\" done 0"
            ; "query \"atomic: ADD a\" done 1"
            ; "query \"atomic: ADD b\" done 1"
            ; "query \"COMMIT\" done 0" ]
            events )
    ; test
        "a server error of a command: ROLLBACK, the error, the same connection"
        (fun () ->
          let r, cmds, connects, events =
            run ["ADD a"; "FAILV 1062"; "ADD c"]
          in
          equal string "Constraint_violation Unique" r ;
          equal (list string)
            ["START TRANSACTION"; "ADD a"; "FAILV 1062"; "ROLLBACK"]
            cmds ;
          (* No new connect: the connection went back to the pool. *)
          equal int 1 connects ;
          equal (list string)
            [ "query \"START TRANSACTION\" done 0"
            ; "query \"atomic: ADD a\" done 1"
            ; "query \"atomic: FAILV 1062\" failed Constraint_violation Unique"
            ; "query \"ROLLBACK\" done 0" ]
            events ;
          (* The event of a 1062 holds no value of the row. *)
          is_false (List.exists (fun e -> contains e "secret") events) )
    ; test "an error of START TRANSACTION: no command, no ROLLBACK" (fun () ->
          let r, cmds, _, events =
            run ~setup:(fun db -> db.faults.begin_err <- Some 1146) ["ADD a"]
          in
          equal string "Driver 1146 28000" r ;
          equal (list string) ["START TRANSACTION"] cmds ;
          equal (list string)
            ["query \"START TRANSACTION\" failed Driver 1146 28000"]
            events )
    ; test "a 1213 of COMMIT: the reply table of Db.transaction, no ROLLBACK"
        (fun () ->
          let r, cmds, _, _ =
            run
              ~setup:(fun db -> db.faults.commit <- Some (D.Commit_err 1213))
              ["ADD a"]
          in
          equal string "Retryable rolled back" r ;
          equal (list string)
            ["START TRANSACTION"; "ADD a"; "COMMIT"; "ping"]
            cmds )
    ; test "a 1020 of a command on MariaDB: ROLLBACK, then Retryable" (fun () ->
          let r, cmds, _, _ =
            run
              ~setup:(fun db -> db.faults.version <- "11.8.8-MariaDB")
              ["ADD a"; "SNAPSHOT"]
          in
          equal string "Retryable rolled back" r ;
          equal (list string)
            ["START TRANSACTION"; "ADD a"; "SNAPSHOT"; "ROLLBACK"]
            cmds )
    ; test "a failed ROLLBACK: the connection closes" (fun () ->
          let r, cmds, connects, _ =
            run
              ~setup:(fun db -> db.faults.rollback_err <- Some 1146)
              ["FAIL 1062"]
          in
          equal string "Constraint_violation Unique" r ;
          equal (list string)
            ["START TRANSACTION"; "FAIL 1062"; "ROLLBACK"]
            cmds ;
          equal int 2 connects )
    ; test "an OK of COMMIT with IN_TRANS: the value, the connection closes"
        (fun () ->
          let r, _, connects, _ =
            run
              ~setup:(fun db -> db.faults.commit <- Some D.Commit_in_trans)
              ["ADD a"]
          in
          equal string "Ok 1" r ; equal int 2 connects )
    ; cases "a lost reply of a command: Connection_lost, no ROLLBACK"
        ~id:(fun (c, _) -> c)
        [ ("ADD a", ["START TRANSACTION"; "ADD a"])
        ; ("ADD b", ["START TRANSACTION"; "ADD a"; "ADD b"]) ]
        (fun (lost, expected) ->
          let r, cmds, connects, events =
            run
              ~setup:(fun db ->
                db.faults.script <-
                  (fun sql -> if sql = lost then Some D.No_reply else None) )
              ["ADD a"; "ADD b"]
          in
          (* Not committed: the server rolls back at the close. *)
          equal string "Connection_lost" r ;
          equal (list string) expected cmds ;
          equal int 2 connects ;
          is_true
            (List.mem
               (Printf.sprintf
                  "query \"atomic: %s\" failed Outcome_unknown statement" lost )
               events ) )
    ; test "a lost reply of COMMIT: Outcome_unknown commit" (fun () ->
          let r, _, connects, _ =
            run
              ~setup:(fun db -> db.faults.commit <- Some D.Commit_close)
              ["ADD a"]
          in
          equal string "Outcome_unknown commit" r ;
          equal int 2 connects )
    ; test "a deadline in a command: Timeout, the connection closes" (fun () ->
          let r, cmds, connects, events =
            run ~timeout:(Span.ms 200) ["ADD a"; "SLEEP 500"]
          in
          equal string "Timeout" r ;
          equal (list string) ["START TRANSACTION"; "ADD a"; "SLEEP 500"] cmds ;
          equal int 2 connects ;
          (* As Tx.exec: no ROLLBACK event, because no ROLLBACK went. *)
          equal (list string)
            [ "query \"START TRANSACTION\" done 0"
            ; "query \"atomic: ADD a\" done 1"
            ; "query \"atomic: SLEEP 500\" cancelled sent=true" ]
            events )
    ; test "the flow deadline before the timer of the Tx: Timeout, no ROLLBACK"
        (fun () ->
          (* The shield holds back the timer of the Tx, thus the flow deadline
             of the command fires first, as it can on a real clock. *)
          let r, cmds, connects, events =
            run ~timeout:(Span.ms 200)
              ~around:(fun c f ->
                if c = "SLEEP 500" then Fiber.shield f else f () )
              ["ADD a"; "SLEEP 500"]
          in
          equal string "Timeout" r ;
          equal (list string) ["START TRANSACTION"; "ADD a"; "SLEEP 500"] cmds ;
          equal int 2 connects ;
          equal (list string)
            [ "query \"START TRANSACTION\" done 0"
            ; "query \"atomic: ADD a\" done 1"
            ; (* The event of the command tells the state of the server; the
                 result is the final error of the Tx. *)
              "query \"atomic: SLEEP 500\" failed Outcome_unknown statement" ]
            events )
    ; cases "an error that breaks the connection: the error, no ROLLBACK"
        ~id:(fun (c, _) -> c)
        [("FAIL 1153", "Invalid_value 1153"); ("INFILE", "Decode")]
        (fun (cmd, expected) ->
          let r, cmds, connects, events = run ["ADD a"; cmd] in
          equal string expected r ;
          equal (list string) ["START TRANSACTION"; "ADD a"; cmd] cmds ;
          equal int 2 connects ;
          is_false
            (List.exists
               (String.starts_with ~prefix:"query \"ROLLBACK\"")
               events ) )
    ; test "an implicit commit in a command: Implicit_commit" (fun () ->
          let r, cmds, _, events = run ["ADD a"; "CREATE t"; "FAIL 1062"] in
          equal string "Implicit_commit" r ;
          equal (list string)
            ["START TRANSACTION"; "ADD a"; "CREATE t"; "ROLLBACK"]
            cmds ;
          is_true
            (List.mem "query \"atomic: CREATE t\" failed Implicit_commit" events) )
    ; test "the rows of the event of each form of command" (fun () ->
          let _, _, _, events =
            run
              ~setup:(fun db ->
                db.faults.script <-
                  (function
                  | "TWO" ->
                      Some (D.Rows [[Some "1"]; [Some "2"]])
                  | "NONE" ->
                      Some (D.Rows [])
                  | "ZERO" ->
                      Some (D.Counts {affected= 0L; insert= 0L})
                  | _ ->
                      None ) )
              ["INSERT x"; "ZERO"; "QUERY TWO"; "ROWS TWO"; "ROWS NONE"]
          in
          equal (list string)
            [ "query \"atomic: INSERT x\" done 2"
            ; "query \"atomic: ZERO\" done 0"
            ; "query \"atomic: QUERY TWO\" done 2"
            ; "query \"atomic: ROWS TWO\" done 2"
            ; "query \"atomic: ROWS NONE\" done 0" ]
            (List.filter (fun e -> contains e "atomic: ") events) )
    ; test "a cancel before the bytes of a command: sent=false" (fun () ->
          (* The shield holds back the cancel until the second command
             starts. *)
          let r, cmds, _, events =
            run ~cancel_ms:20
              ~around:(fun c f ->
                if c = "ADD b" then (
                  Fiber.shield (fun () -> Time.sleep Clock.Mono (Span.ms 50)) ;
                  f () )
                else f () )
              ["ADD a"; "ADD b"]
          in
          equal string "cancelled" r ;
          equal (list string) ["START TRANSACTION"; "ADD a"] cmds ;
          is_true
            (List.mem "query \"atomic: ADD b\" cancelled sent=false" events) )
    ; test "a value with no literal: Invalid_value, with no transaction"
        (fun () ->
          let r, cmds, connects, events =
            run ~text:(Error "a NaN is not a SQL value") ["ADD a"]
          in
          equal string "Invalid_value" r ;
          equal (list string) [] cmds ;
          equal int 1 connects ;
          equal (list string)
            ["query \"atomic: ADD a\" failed Invalid_value"]
            events )
    ; test "the margin before a command: Timeout, no byte, ROLLBACK" (fun () ->
          let r, cmds, connects, events =
            run ~timeout:(Span.ms 120) ["SLEEP 80"; "ADD b"]
          in
          equal string "Timeout" r ;
          equal (list string) ["START TRANSACTION"; "SLEEP 80"; "ROLLBACK"] cmds ;
          equal int 1 connects ;
          is_true (List.mem "query \"atomic: ADD b\" failed Timeout" events) )
    ; cases "a cancel in a command: the cancel, the connection closes"
        ~id:(fun (n, _, _) -> n)
        [ ("START TRANSACTION", ["ADD a"], "START TRANSACTION")
        ; ("the first command", ["SLEEP 100"; "ADD b"], "atomic: SLEEP 100")
        ; ("the last command", ["ADD a"; "SLEEP 100"], "atomic: SLEEP 100") ]
        (fun (_, cmds, cancelled) ->
          let r, _, connects, events =
            run
              ~setup:(fun db ->
                if cancelled = "START TRANSACTION" then
                  db.faults.latency_ms <- 100 )
              ~cancel_ms:20 cmds
          in
          equal string "cancelled" r ;
          equal int 2 connects ;
          is_true
            (List.mem
               (Printf.sprintf "query %S cancelled sent=true" cancelled)
               events ) ;
          (* No COMMIT and no ROLLBACK went. *)
          is_false
            (List.exists
               (fun e ->
                 String.starts_with ~prefix:"query \"COMMIT\"" e
                 || String.starts_with ~prefix:"query \"ROLLBACK\"" e )
               events ) )
    ; test "a cancel during a slow COMMIT: the shield keeps its result"
        (fun () ->
          let r, cmds, connects, _ =
            run
              ~setup:(fun db -> db.faults.commit <- Some (D.Commit_delay 100))
              ~cancel_ms:20 ["ADD a"]
          in
          equal string "Ok 1" r ;
          equal (list string) ["START TRANSACTION"; "ADD a"; "COMMIT"] cmds ;
          (* No KILL and no abort: the connection went back. *)
          equal int 1 connects )
    ; test "the seam on_hold sees each holder" (fun () ->
          let holders = ref 0 in
          let r, _ =
            T.with_pool (fun _ _ pool ->
                (Cistern.Private.Pool.Private.on_hold := fun _ -> incr holders) ;
                Fun.protect
                  ~finally:(fun () ->
                    Cistern.Private.Pool.Private.on_hold := ignore )
                  (fun () ->
                    ignore (Db.exec pool (T.atomic ["ADD a"])) ;
                    ignore (Db.exec pool (T.clean ())) ;
                    !holders ) )
          in
          equal int 2 r )
    ; test "the checks of misuse" (fun () ->
          let r, _ =
            T.with_pool (fun _ _ pool ->
                let in_tx =
                  match
                    Db.transaction pool (fun _ ->
                        Db.exec pool (T.atomic ["ADD a"]) )
                  with
                  | _ ->
                      "no error"
                  | exception Invalid_argument m ->
                      m
                in
                let tx_exec =
                  (* Tx.exec of an atomic statement after the end of its Tx. *)
                  match Db.transaction pool Fun.id with
                  | Ok tx -> (
                    match Tx.exec tx (T.atomic ["ADD a"]) with
                    | _ ->
                        "no error"
                    | exception Invalid_argument m ->
                        m )
                  | Error e ->
                      T.error_text e
                in
                (in_tx, tx_exec) )
          in
          equal string "Db.exec in the fiber of a Tx" (fst r) ;
          equal string "Tx.exec after the end of the transaction" (snd r) )
    ; test "an atomic statement on a closed pool: the misuse of with_db"
        (fun () ->
          let r = ref "" in
          D.run (fun _ _ ->
              let last = ref None in
              Db.with_db ~pool_size:1 (D.config ()) (fun pool ->
                  last := Some pool ) ;
              r :=
                match Db.exec (Option.get !last) (T.atomic ["ADD a"]) with
                | _ ->
                    "no error"
                | exception Invalid_argument m ->
                    m ) ;
          equal string "Db.t used after with_db" !r ) ]
