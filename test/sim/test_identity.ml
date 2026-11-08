(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* Round trip 3 of the session, Connection.kill, and the event Kill_off of the
   pool (the plan of phase 1.5, D6, D6a and 3.4; PR 1.5b). The scripts of
   Fake_server check the exact SQL and each check of round trip 3. Fake_db
   checks the pool and the KILL of a session. *)

open Windtrap
open Weft
module C = Cistern_mysql.Config
module Conn = Cistern_mysql.Connection
module E = Cistern_core.Error
module Ev = Cistern_core.Event
module F = Fake_server
module D = Fake_db
module Db = Cistern.Db
module Pool = Cistern.Private.Pool

let config ?(kill = true) () =
  Result.get_ok
    (C.make ~kill ~tls:C.No_tls
       ~address:(C.Tcp {host= "10.0.0.1"; port= 3306})
       ~user:"u" ~password:"pw" ~database:"db" () )

let error_text = function
  | E.Connect m ->
      "Connect " ^ m
  | e ->
      Format.asprintf "%a" E.pp_summary e

let check_text = function
  | Conn.Unchecked ->
      "unchecked"
  | Conn.Checked (Mysql_server {uuid; start}) ->
      Printf.sprintf "checked mysql %s %Ld" uuid start
  | Conn.Checked (Mariadb_server {server_uid; hostname; port; server_id; start})
    ->
      Printf.sprintf "checked mariadb %s %s %d %Ld %Ld" server_uid hostname port
        server_id start
  | Conn.Refused r ->
      (* The text of the event is "kill off <reason>". *)
      let s = Format.asprintf "%a" Ev.pp (Ev.Kill_off {reason= r}) in
      "refused " ^ String.sub s 9 (String.length s - 9)

(* The grantee text of the review A2 (LN1), and the SQL of round trip 3, written
   out here with no code of the library. *)
let grantee =
  "CONCAT('''', LEFT(CURRENT_USER(), CHAR_LENGTH(CURRENT_USER()) - \
   CHAR_LENGTH(SUBSTRING_INDEX(CURRENT_USER(), '@', -1)) - 1), '''@''', \
   SUBSTRING_INDEX(CURRENT_USER(), '@', -1), '''')"

let counts admin =
  "(SELECT COUNT(*) FROM information_schema.USER_PRIVILEGES WHERE GRANTEE = "
  ^ grantee
  ^ "), (SELECT COUNT(*) FROM information_schema.USER_PRIVILEGES WHERE GRANTEE \
     = " ^ grantee ^ " AND PRIVILEGE_TYPE IN ('SUPER', '" ^ admin
  ^ "')), (SELECT COUNT(ROLE_NAME) FROM information_schema.ENABLED_ROLES)"

let rt3_mysql =
  "\x03SELECT CONNECTION_ID(), @@server_uuid, UNIX_TIMESTAMP() - (SELECT \
   VARIABLE_VALUE FROM performance_schema.global_status WHERE VARIABLE_NAME = \
   'Uptime'), " ^ counts "CONNECTION_ADMIN"

let rt3_mariadb =
  "\x03SELECT CONNECTION_ID(), @@server_uid, @@hostname, @@port, @@server_id, \
   UNIX_TIMESTAMP() - (SELECT VARIABLE_VALUE FROM \
   information_schema.GLOBAL_STATUS WHERE VARIABLE_NAME = 'UPTIME'), "
  ^ counts "CONNECTION ADMIN"

(* A MySQL handshake with the thread id [thread] and the CONNECTION_ID() [id] of
   round trip 1, then round trip 3 with [reply]. *)
let mysql_script ?(thread = 42) ?(id = "42") reply =
  F.handshake ~greet:(F.greeting ~thread ()) ~values:(F.session ~id ()) ()
  @ (F.Recv :: reply)

(* The row of round trip 3 on MySQL. *)
let row ?(id = "42") ?(uuid = "u1") ?(start = "1794097300") ?(grantee = "1")
    ?(admin = "0") ?(roles = "0") () =
  [ F.Sends
      ( 1
      , F.result_set
          (List.map Option.some [id; uuid; start; grantee; admin; roles]) ) ]

(* Connect, read the identity, close. *)
let identity ?(kill = true) script =
  F.run script (fun _ ->
      match Conn.connect (config ~kill ()) with
      | Ok c ->
          let i = check_text (Conn.info c).identity in
          ignore (Conn.close c) ;
          i
      | Error e ->
          error_text e )

let two_rows =
  let n = 6 in
  let r = F.row (List.init n (fun i -> Some (string_of_int i))) in
  [ F.Sends
      ( 1
      , String.make 1 (Char.chr n)
        :: List.init n (fun i -> F.column (Printf.sprintf "c%d" i))
        @ [r; r; F.terminator] ) ]

let five_values =
  [ F.Sends
      ( 1
      , F.result_set (List.map Option.some ["42"; "u1"; "1794097300"; "1"; "0"])
      ) ]

(* Each row: the name, the script of round trip 3, and the identity. *)
let mysql_rows =
  [ ("each check passes", mysql_script (row ()), "checked mysql u1 1794097300")
  ; ( "a server error"
    , mysql_script [F.Send (1, F.err 1142 "denied")]
    , "refused identity_failed" )
  ; ( "an OK in place of a row"
    , mysql_script [F.Send (1, F.ok ())]
    , "refused identity_failed" )
  ; ("two rows", mysql_script two_rows, "refused identity_failed")
  ; ("five values", mysql_script five_values, "refused identity_failed")
  ; ( "a start time that does not decode"
    , mysql_script (row ~start:"1794097300.5" ())
    , "refused identity_failed" )
  ; ("another CONNECTION_ID()", mysql_script (row ~id:"43" ()), "refused proxy")
  ; ( "another thread id in the greeting"
    , mysql_script ~thread:7 (row ())
    , "refused proxy" )
  ; ( "a 64-bit id whose low 32 bits are the thread id"
    , mysql_script ~id:"4294967338" (row ~id:"4294967338" ())
    , "checked mysql u1 1794097300" )
  ; ( "a 64-bit id whose low 32 bits are not the thread id"
    , mysql_script ~id:"4294967339" (row ~id:"4294967339" ())
    , "refused proxy" )
  ; ( "the low 32 bits of round trip 1, but not its id"
    , mysql_script (row ~id:"4294967338" ())
    , "refused proxy" )
  ; ( "no row of the grantee"
    , mysql_script (row ~grantee:"0" ())
    , "refused grantee_unknown" )
  ; ( "a direct grant of SUPER or CONNECTION_ADMIN"
    , mysql_script (row ~admin:"1" ())
    , "refused privileged" )
  ; ( "two admin rows"
    , mysql_script (row ~grantee:"3" ~admin:"2" ())
    , "refused privileged" )
  ; ("an enabled role", mysql_script (row ~roles:"1" ()), "refused role")
  ; ( "the proxy check first"
    , mysql_script (row ~id:"43" ~grantee:"0" ~admin:"1" ~roles:"1" ())
    , "refused proxy" )
  ; ( "then the grantee"
    , mysql_script (row ~grantee:"0" ~admin:"1" ~roles:"1" ())
    , "refused grantee_unknown" )
  ; ( "then the privilege"
    , mysql_script (row ~admin:"1" ~roles:"1" ())
    , "refused privileged" )
  ; ( "the end of the stream in round trip 3"
    , mysql_script [F.Close]
    , "Connect the server closed the connection" ) ]

let mariadb_greeting =
  F.greeting ~version:"11.8.8-MariaDB" ~caps:F.caps_mariadb ~ext:0x3d ~thread:9
    ()

let mariadb_session =
  F.session ~id:"9" ~version:"11.8.8-MariaDB" ~comment:"MariaDB Server"
    ~max_packet:"16777216" ()

(* A script of Connection.kill after a handshake with no round trip 3. *)
let kill_script reply = F.handshake () @ [F.Recv; reply]

let kill_text = function
  | Ok Conn.Killed ->
      "killed"
  | Ok Conn.Gone ->
      "gone"
  | Ok Conn.Not_owner ->
      "not_owner"
  | Error e ->
      "error " ^ error_text e

let events_text evs =
  List.rev_map (fun e -> Format.asprintf "%a" Ev.pp e) evs
  |> List.filter (fun s -> not (String.starts_with ~prefix:"query" s))
  |> List.map (fun s ->
      if String.starts_with ~prefix:"connect" s then "connect" else s )

let q s = Cistern_core.Raw.sql s

(* A pool of [size] on Fake_db with the fault [fault], [n] statements in [n]
   fibers. It gives the events (no query), the count of the identity SELECTs,
   and the flag of the pool. *)
let pool_run ?(kill = true) ?(domains = 1) ~size ~n fault =
  let evs = ref [] in
  D.run ~domains (fun _ db ->
      db.D.faults.identity <- fault ;
      let off =
        Db.with_db ~pool_size:size
          ~on_query:(fun e -> evs := e :: !evs)
          (Result.get_ok
             (C.make ~kill ~tls:C.No_tls
                ~address:(C.Tcp {host= "10.0.0.9"; port= 3306})
                ~user:"u" ~password:"pw" ~database:"db" () ) )
          (fun pool ->
            Fiber.scope (fun s ->
                for _ = 1 to n do
                  Fiber.fork s (fun () ->
                      ignore
                        (Db.exec pool
                           (Cistern.Raw.query (Cistern.Raw.sql "SLEEP 10")) ) )
                done ) ;
            Pool.Private.kill_off (Db.Private.pool pool) )
      in
      (events_text !evs, db.D.identities, off) )

let tests =
  group "Round trip 3 and KILL"
    [ cases "round trip 3 on MySQL"
        ~id:(fun (n, _, _) -> n)
        mysql_rows
        (fun (_, script, expected) ->
          let result, _ = identity script in
          equal string expected result )
    ; test "the SQL of round trip 3 on MySQL" (fun () ->
          let _, received = identity (mysql_script (row ())) in
          equal string rt3_mysql (snd (List.nth received 3)) ;
          (* Then COM_QUIT. *)
          equal (list string) ["\x01"]
            (List.map snd (List.filteri (fun i _ -> i > 3) received)) )
    ; test "round trip 3 on MariaDB, and its SQL" (fun () ->
          let result, received =
            identity
              ( F.handshake ~greet:mariadb_greeting ~values:mariadb_session ()
              @ [ F.Recv
                ; F.Sends
                    ( 1
                    , F.result_set
                        (List.map Option.some
                           [ "9"
                           ; "LWgKzj9uImKMBIPInvf0c+s4SNU="
                           ; "host1"
                           ; "3307"
                           ; "4294967296"
                           ; "1794097306"
                           ; "1"
                           ; "0"
                           ; "0" ] ) ) ] )
          in
          equal string
            "checked mariadb LWgKzj9uImKMBIPInvf0c+s4SNU= host1 3307 \
             4294967296 1794097306"
            result ;
          equal string rt3_mariadb (snd (List.nth received 3)) )
    ; test "kill=false: no round trip 3" (fun () ->
          let result, received = identity ~kill:false (F.handshake ()) in
          equal string "unchecked" result ;
          (* Round trips 1 and 2, then COM_QUIT. *)
          equal string "\x01" (snd (List.nth received 3)) ;
          equal int 4 (List.length received) )
    ; cases "Connection.kill"
        ~id:(fun (n, _, _) -> n)
        [ ("an OK", F.Send (1, F.ok ()), "killed")
        ; ("1094", F.Send (1, F.err 1094 "unknown thread"), "gone")
        ; ("1095", F.Send (1, F.err 1095 "not owner"), "not_owner")
        ; ( "another error"
          , F.Send (1, F.err 1317 "interrupted")
          , "error Driver 1317 28000" )
        ; ("the end of the stream", F.Close, "error Connection_lost") ]
        (fun (_, reply, expected) ->
          let result, received =
            F.run (kill_script reply) (fun _ ->
                match Conn.connect (config ~kill:false ()) with
                | Ok c ->
                    let r = kill_text (Conn.kill c 4294967338L) in
                    ignore (Conn.close c) ;
                    r
                | Error e ->
                    error_text e )
          in
          equal string expected result ;
          equal string "\x03KILL CONNECTION 4294967338"
            (snd (List.nth received 3)) )
    ; test "Fake_db: KILL ends the session of another connection" (fun () ->
          let r =
            D.run (fun _ db ->
                let c1 =
                  Result.get_ok (Conn.connect (D.config ~kill:true ()))
                in
                let c2 =
                  Result.get_ok (Conn.connect (D.config ~kill:true ()))
                in
                let id1 = (Conn.info c1).connection_id in
                let slept = Ivar.create () in
                Fiber.scope (fun s ->
                    Fiber.fork s (fun () ->
                        Ivar.fill slept
                          ( match Conn.exec c1 (q "SLEEP 5000") with
                          | Ok _ ->
                              "ok"
                          | Error e ->
                              error_text e ) ) ;
                    Time.sleep Mono (Span.ms 10) ;
                    let first = kill_text (Conn.kill c2 id1) in
                    let s1 = sync (Ivar.read slept) in
                    let again = kill_text (Conn.kill c2 id1) in
                    db.D.faults.kill_err <- Some 1095 ;
                    let owner =
                      kill_text (Conn.kill c2 (Conn.info c2).connection_id)
                    in
                    ignore (Conn.close c1) ;
                    ignore (Conn.close c2) ;
                    [ Int64.to_string id1
                    ; first
                    ; s1
                    ; again
                    ; owner
                    ; string_of_int db.D.killed
                    ; string_of_int db.D.identities ] ) )
          in
          equal (list string)
            [ "1"
            ; "killed"
            ; "Outcome_unknown statement"
            ; "gone"
            ; "not_owner"
            ; "1"
            ; "2" ]
            r )
    ; cases "the pool: one Kill_off for each refused identity"
        ~id:(fun (n, _, _) -> n)
        [ ("a server error", D.Identity_err, "kill off identity_failed")
        ; ("a bad row", D.Identity_bad_row, "kill off identity_failed")
        ; ("another id", D.Identity_other_id, "kill off proxy")
        ; ("another greeting id", D.Identity_greeting, "kill off proxy")
        ; ("no grantee", D.Identity_no_grantee, "kill off grantee_unknown")
        ; ("privileged", D.Identity_privileged, "kill off privileged")
        ; ("a role", D.Identity_role, "kill off role") ]
        (fun (_, fault, expected) ->
          let evs, identities, off = pool_run ~size:1 ~n:1 fault in
          (* The connection of the pool and the control connection of the killer
             (phase 1.5c1a): one Kill_off. *)
          equal (list string)
            ["connect"; "connect"; expected]
            (List.sort compare evs) ;
          equal int 2 identities ;
          equal bool true off )
    ; test "the pool: one Kill_off for 4 connections on 4 domains" (fun () ->
          let evs, identities, off =
            pool_run ~domains:4 ~size:4 ~n:8 D.Identity_other_id
          in
          (* And the control connection. *)
          equal int 5 identities ;
          equal int 5 (List.length (List.filter (( = ) "connect") evs)) ;
          equal (list string) ["kill off proxy"]
            (List.filter (( <> ) "connect") evs) ;
          equal bool true off )
    ; test "the pool: no Kill_off when each check passes" (fun () ->
          let evs, identities, off = pool_run ~size:2 ~n:4 D.Identity_ok in
          equal (list string) ["connect"; "connect"; "connect"] evs ;
          equal int 3 identities ;
          equal bool false off )
    ; test "the pool with kill=false: no round trip 3 and no Kill_off"
        (fun () ->
          let evs, identities, off =
            pool_run ~kill:false ~size:1 ~n:1 D.Identity_err
          in
          equal (list string) ["connect"] evs ;
          equal int 0 identities ;
          equal bool false off ) ]
