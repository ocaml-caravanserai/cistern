(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* TLS on one connection (the plan of phase 1.4, PR 1.4b1): the handshake with
   the SSL request and the buffer check, each row of the table of the verified
   configurations, the clear-text rule, the CA file, the generator, the escape
   of a TLS failure, and the rules of stale, close, the write and the deadlines
   on a TLS flow. The fake server does the TLS of ocaml-tls
   (Fake_server.Start_tls). *)

open Windtrap
open Weft
module C = Cistern_mysql.Config
module Conn = Cistern_mysql.Connection
module F = Fake_server
module X = Tls_fixture
module T = Test_connect

let sql = Cistern_core.Raw.sql

(* The capabilities of the client with CLIENT_SSL, for the greeting of MySQL. *)
let caps = 0x10AAA0A

let ssl_request = F.u32 caps ^ F.u32 0x100_0000 ^ "\x2d" ^ String.make 23 '\x00'

let native_response =
  T.response ~caps ~plugin:"mysql_native_password" ~auth:(T.native F.nonce) ()

let sha2_response =
  T.response ~caps ~plugin:"caching_sha2_password" ~auth:(T.sha2 F.nonce) ()

(* The script of a server with TLS, native authentication, and the two round
   trips. *)
let tls_script ?(server = X.server_config X.pki) ?(greet = F.greeting ()) () =
  [ F.Send (0, greet)
  ; F.Recv
  ; F.Start_tls server
  ; F.Recv
  ; F.Send (3, F.ok ())
  ; F.Recv
  ; F.Sends (1, F.result_set (F.session ()))
  ; F.Recv
  ; F.Send (1, F.ok ()) ]

let rt2 = T.rt2 "utf8mb4_0900_bin"

(* [run ?listen ?dns script client]: Fake_server.run_raw at the time 2025. *)
let run ?listen ?dns script client =
  (* The same random bytes in each run of a test, also in a mutation run that
     runs only some tests. *)
  X.seed () ;
  F.run_raw ?listen ?dns script (fun sim ->
      X.at_2025 sim ; X.install () ; client sim )

(* [with_tls_pool f]: a pool of 1 on the fake server of many connections with
   TLS (Test_pool.with_pool ~tls), and its Query events. *)
let with_tls_pool f =
  let r, events = Test_pool.with_pool ~tls:true ~size:1 f in
  (r, Test_pool.queries events)

(* A cancel that comes in the seam after the checkout, thus between the checkout
   and the stale check (the latch of 1.2). *)
let cancelled_in_checkout run =
  (Cistern.Private.Pool.Private.after_op :=
     fun () -> Fiber.shield (fun () -> Time.sleep Mono (Span.ms 5)) ) ;
  Fun.protect
    ~finally:(fun () -> Cistern.Private.Pool.Private.after_op := ignore)
    (fun () ->
      match Fiber.with_timeout Mono (Span.ms 2) run with
      | _ ->
          "no cancel"
      | exception Fiber.Timeout ->
          "cancelled" )

(* Connect with [config], run [f] on the connection, and close it. *)
let with_tls config f sim =
  match Conn.connect config with
  | Ok c ->
      let r = f c in
      ignore (Conn.close c) ;
      T.no_client_fd sim ;
      r
  | Error e ->
      T.no_client_fd sim ; T.error_text e

let connect_close config = with_tls config (fun _ -> "connected")

let any : X509.Authenticator.t = fun ?ip:_ ~host:_ _ -> Ok None

let starts prefix s = String.starts_with ~prefix s

(* The rows of the table of the configurations (DESIGN.md, "Configuration"),
   with "verified" for the clear-text rule. *)
let rows =
  [ ("REQUIRED", (fun () -> X.config Required), false)
  ; ( "REQUIRED with a pin"
    , (fun () -> X.config ~fingerprint:(X.pin X.pki.cert) Required)
    , true )
  ; ("VERIFY_CA", (fun () -> X.config ~ca:X.ca_file Verify_ca), false)
  ; ( "VERIFY_CA with a pin"
    , (fun () -> X.config ~fingerprint:(X.pin X.pki.cert) Verify_ca)
    , true )
  ; ( "VERIFY_IDENTITY with the CA, by the IP address"
    , (fun () -> X.config ~ca:X.ca_file Verify_identity)
    , true )
  ; ( "VERIFY_IDENTITY with the CA, by the name"
    , (fun () -> X.config ~host:"db.test" ~ca:X.ca_file Verify_identity)
    , true )
  ; ( "VERIFY_IDENTITY with a pin"
    , (fun () -> X.config ~fingerprint:(X.pin X.pki.cert) Verify_identity)
    , true )
  ; ( "VERIFY_IDENTITY with only an authenticator"
    , (fun () -> X.config ~authenticator:any Verify_identity)
    , false )
  ; ( "VERIFY_IDENTITY with an authenticator and a pin"
    , (fun () ->
        X.config ~authenticator:any ~fingerprint:(X.pin X.pki.cert)
          Verify_identity )
    , true ) ]

let dns = [("db.test", Weft_sim.Addresses ["10.0.0.1"])]

(* A failed TLS handshake: the server got the SSL request and no other
   packet. *)
let tls_failure ?listen ?(dns = dns) ?(server = X.pki) config =
  let result, received, _ =
    run ?listen ~dns
      (tls_script ~server:(X.server_config server) ())
      (connect_close config)
  in
  is_true ~msg:("a TLS failure: " ^ result) (starts "Connect TLS: " result) ;
  equal (list (pair int string)) [(1, ssl_request)] received ;
  result

(* A CA file that the client refuses before the network: no socket, no time. *)
let ca_refused setup =
  let result, received, raw =
    run [] (fun sim ->
        let path = setup () in
        let fds = Weft_sim.open_fds sim and t0 = Time.now Mono in
        let r = connect_close (X.config ~ca:path Verify_identity) sim in
        Printf.sprintf "%s|%b %b" r
          (Weft_sim.open_fds sim = fds)
          (Span.equal (Time.diff Mono (Time.now Mono) t0) Span.zero) )
  in
  equal (list (pair int string)) [] received ;
  equal string "" raw ;
  result

let big = sql ("SELECT '" ^ String.make 100_000 'a' ^ "'")

let tests =
  group "TLS on a connection"
    [ cases "each configuration connects" ~id:T.id3 rows (fun (_, config, _) ->
          let result, received, _ =
            run ~dns (tls_script ()) (connect_close (config ()))
          in
          equal string "connected" result ;
          (* The SSL request (1), the response (2) after TLS, and the two round
             trips. *)
          equal
            (list (pair int string))
            [ (1, ssl_request)
            ; (2, native_response)
            ; (0, T.rt1)
            ; (0, rt2)
            ; (0, "\x01") ]
            received )
    ; cases "the full path of caching_sha2_password" ~id:T.id3 rows
        (fun (_, config, verified) ->
          let script =
            [ F.Send (0, F.greeting ~plugin:"caching_sha2_password" ())
            ; F.Recv
            ; F.Start_tls (X.server_config X.pki)
            ; F.Recv
            ; F.Send (3, "\x01\x04")
            ; F.Recv
            ; F.Send (5, F.ok ())
            ; F.Recv
            ; F.Sends (1, F.result_set (F.session ()))
            ; F.Recv
            ; F.Send (1, F.ok ()) ]
          in
          let result, received, _ =
            run ~dns script (connect_close (config ()))
          in
          if verified then (
            equal string "connected" result ;
            equal
              (list (pair int string))
              [ (1, ssl_request)
              ; (2, sha2_response)
              ; (4, X.password ^ "\x00")
              ; (0, T.rt1)
              ; (0, rt2)
              ; (0, "\x01") ]
              received )
          else (
            (* The clear-text rule: no password byte goes. *)
            equal string T.gate result ;
            equal
              (list (pair int string))
              [(1, ssl_request); (2, sha2_response)]
              received ) )
    ; test "a server with no CLIENT_SSL" (fun () ->
          let result, received, raw =
            run
              [ F.Send (0, F.greeting ~caps:(F.caps_mysql land lnot 0x800) ())
              ; F.Recv ]
              (connect_close (X.config ~ca:X.ca_file Verify_identity))
          in
          equal string "Connect the server has no TLS" result ;
          equal (list (pair int string)) [] received ;
          equal string "" raw )
    ; test "a byte from the server before the TLS handshake" (fun () ->
          let result, received, raw =
            run
              [F.Raw (F.frame 0 (F.greeting ()) ^ "x"); F.Recv; F.Recv]
              (connect_close (X.config ~ca:X.ca_file Verify_identity))
          in
          equal string "Connect bytes from the server before the TLS handshake"
            result ;
          (* The SSL request, and no ClientHello. *)
          equal (list (pair int string)) [(1, ssl_request)] received ;
          equal string (F.frame 1 ssl_request) raw )
    ; test "another CA" (fun () ->
          ignore (tls_failure (X.config ~ca:X.other_ca_file Verify_ca)) )
    ; test "another name: the cause stays in the text" (fun () ->
          let result =
            tls_failure
              ~dns:[("db.example", Weft_sim.Addresses ["10.0.0.1"])]
              (X.config ~host:"db.example" ~ca:X.ca_file Verify_identity)
          in
          is_true ~msg:("the cause: " ^ result) (T.contains result "db.example") )
    ; test "another IP address" (fun () ->
          ignore
            (tls_failure ~listen:"10.0.0.2:3306"
               (X.config ~host:"10.0.0.2" ~ca:X.ca_file Verify_identity) ) )
    ; test "another pin: the cause stays in the text" (fun () ->
          let result =
            tls_failure (X.config ~fingerprint:(X.pin X.pki.ca) Required)
          in
          is_true ~msg:("the cause: " ^ result)
            (T.contains result "fingerprint") )
    ; test "a hostile certificate: the text is escaped, with its head and tail"
        (fun () ->
          let result =
            tls_failure ~server:X.hostile
              (X.config ~ca:X.hostile_ca_file Verify_identity)
          in
          is_true
            ~msg:("printable ASCII: " ^ result)
            (String.for_all (fun c -> c >= ' ' && c <= '~') result) ;
          (* "Connect TLS: ", then the first 100 and the last 100 bytes of the
             text, each escaped between quotes, with "..." between them. *)
          let text = String.sub result 13 (String.length result - 13) in
          let parts = String.split_on_char '"' text in
          let bytes s =
            let rec go i n =
              if i >= String.length s then n
              else if s.[i] = '\\' then go (i + 4) (n + 1)
              else go (i + 1) (n + 1)
            in
            go 0 0
          in
          ( match parts with
          | [""; head; "..."; tail; ""] ->
              equal ~msg:"the head" int 100 (bytes head) ;
              equal ~msg:"the tail" int 100 (bytes tail)
          | _ ->
              fail ("the form of the text: " ^ result) ) ;
          (* The text of x509 has newlines: they are escaped. *)
          is_true ~msg:"the newlines, escaped" (T.contains result "\\x0a") ;
          (* x509 puts the cause at the end: the address that the certificate
             does not have. *)
          is_true ~msg:("the cause: " ^ result) (T.contains result "10.0.0.1") )
    ; cases "the CA file" ~id:T.id3
        [ ( "missing"
          , (fun () -> "/nonexistent/ca.pem")
          , "Connect the CA file does not exist" )
        ; ( "a path through a file"
          , (fun () -> "/ca.pem/x")
          , "Connect a part of the path of the CA file is not a directory" )
        ; ( "a directory"
          , (fun () -> Weft.Dir.mkdir "/dir" ; "/dir")
          , "Connect the CA file is not a regular file of at most 1 MiB" )
        ; ( "larger than 1 MiB"
          , (fun () ->
              X.write "/big.pem" (String.make ((1 lsl 20) + 1) 'a') ;
              "/big.pem" )
          , "Connect the CA file is not a regular file of at most 1 MiB" )
        ; ( "exactly 1 MiB: read, and no certificate in it"
          , (fun () ->
              X.write "/limit.pem" (String.make (1 lsl 20) 'a') ;
              "/limit.pem" )
          , "Connect the CA file has no valid certificate" )
        ; ( "empty"
          , (fun () -> X.write "/empty.pem" "" ; "/empty.pem")
          , "Connect the CA file has no valid certificate" )
        ; ( "a key and no certificate"
          , (fun () ->
              X.write "/key.pem" (X509.Private_key.encode_pem X.pki.key) ;
              "/key.pem" )
          , "Connect the CA file has no valid certificate" )
        ; ( "a block with bad content (no part of it in the message)"
          , (fun () ->
              X.write "/bad.pem"
                "-----BEGIN CERTIFICATE-----\n\
                 aHVudGVyMiBzZWNyZXQ=\n\
                 -----END CERTIFICATE-----\n" ;
              "/bad.pem" )
          , "Connect the CA file has no valid certificate" ) ]
        (fun (_, setup, expected) ->
          equal string (expected ^ "|true true") (ca_refused setup) )
    ; test "no generator: Invalid_argument before the network" (fun () ->
          let message name =
            name
            ^ ": no default generator for TLS: call \
               Mirage_crypto_rng_unix.use_default ()"
          in
          Fun.protect ~finally:X.seed (fun () ->
              let result, received, _ =
                run [] (fun sim ->
                    (* After the seed of [run]. *)
                    Mirage_crypto_rng.unset_default_generator () ;
                    let fds = Weft_sim.open_fds sim in
                    let a =
                      match
                        Conn.connect (X.config ~ca:X.ca_file Verify_identity)
                      with
                      | _ ->
                          "no exception"
                      | exception Invalid_argument m ->
                          m
                    in
                    let b =
                      match
                        Cistern.Db.with_db
                          (X.config ~ca:X.ca_file Verify_identity) (fun _ ->
                            () )
                      with
                      | () ->
                          "no exception"
                      | exception Invalid_argument m ->
                          m
                    in
                    Printf.sprintf "%s|%s|%b" a b (Weft_sim.open_fds sim = fds) )
              in
              equal string
                ( message "Connection.connect"
                ^ "|" ^ message "Db.with_db" ^ "|true" )
                result ;
              equal (list (pair int string)) [] received ) )
    ; test "session tickets after the handshake: not stale" (fun () ->
          let tickets = ref 0 in
          let ticket_cache : Tls.Config.ticket_cache =
            { lookup= (fun _ -> None)
            ; ticket_granted= (fun _ _ -> incr tickets)
            ; lifetime= 300l
            ; timestamp=
                (fun () ->
                  Option.get
                    (Ptime.of_float_s
                       (Weft.Time.posix_to_unix_s (Weft.Time.now Posix)) ) ) }
          in
          let result, _, _ =
            run
              ( tls_script ~server:(X.server_config ~ticket_cache X.pki) ()
              @ [F.Hang] )
              (with_tls (X.config ~ca:X.ca_file Verify_identity) (fun c ->
                   Time.sleep Mono (Span.ms 50) ;
                   string_of_bool (Conn.stale c) ) )
          in
          equal string "false" result ;
          is_true ~msg:"the server gave tickets" (!tickets > 0) )
    ; cases "stale on TLS" ~id:T.id3
        [ ( "plaintext: an ERR 4031"
          , [F.Wait_ms 10; F.Send (0, F.err 4031 "idle"); F.Hang]
          , "true" )
        ; ("the end with no close_notify", [F.Wait_ms 10; F.Close], "true")
        ; ("nothing", [F.Hang], "false") ]
        (fun (_, after, expected) ->
          let result, _, _ =
            run
              (tls_script () @ after)
              (with_tls (X.config ~ca:X.ca_file Verify_identity) (fun c ->
                   Time.sleep Mono (Span.ms 50) ;
                   string_of_bool (Conn.stale c) ) )
          in
          equal string expected result )
    ; test
        "a pending cancel in stale: the connection breaks, the cancel goes up"
        (fun () ->
          let result, _, _ =
            run
              ( tls_script ()
              @ [F.Wait_ms 10; F.Send (0, F.err 4031 "idle"); F.Hang] )
              (with_tls (X.config ~ca:X.ca_file Verify_identity) (fun c ->
                   Time.sleep Mono (Span.ms 50) ;
                   (* The record is complete in the socket: read_now takes the
                      lock of the session, which raises the pending cancel. *)
                   let r =
                     match
                       Fiber.with_timeout Mono (Span.ms 1) (fun () ->
                           ( try Time.sleep Mono (Span.s 1)
                             with Fiber.Cancelled _ -> () ) ;
                           Conn.stale c )
                     with
                     | s ->
                         "no cancel " ^ string_of_bool s
                     | exception Fiber.Timeout ->
                         "cancelled"
                   in
                   r ^ " " ^ string_of_bool (Conn.usable c) ) )
          in
          equal string "cancelled false" result )
    ; test "close on TLS: a server that closed, and a server that reads nothing"
        (fun () ->
          List.iter
            (fun after ->
              let result, _, _ =
                run
                  (tls_script () @ after)
                  (fun sim ->
                    match
                      Conn.connect (X.config ~ca:X.ca_file Verify_identity)
                    with
                    | Error e ->
                        T.error_text e
                    | Ok c ->
                        Time.sleep Mono (Span.ms 10) ;
                        let r = Conn.close c in
                        T.no_client_fd sim ; string_of_bool r )
              in
              equal string "true" result )
            [[F.Close]; [F.Hang]] )
    ; test "close in a cancelled fiber: the cancel at the write lock" (fun () ->
          let result, received, _ =
            run
              (tls_script () @ [F.Recv])
              (fun sim ->
                match Conn.connect (X.config ~ca:X.ca_file Verify_identity) with
                | Error e ->
                    T.error_text e
                | Ok c ->
                    let r =
                      match
                        Fiber.with_timeout Mono (Span.ms 1) (fun () ->
                            ( try Time.sleep Mono (Span.s 1)
                              with Fiber.Cancelled _ -> () ) ;
                            Conn.close c )
                      with
                      | _ ->
                          "no cancel"
                      | exception Fiber.Timeout ->
                          "cancelled"
                    in
                    T.no_client_fd sim ; r )
          in
          equal string "cancelled" result ;
          (* No COM_QUIT: the write lock raised before any byte. *)
          equal int 4 (List.length received) )
    ; test "the write rule: a cancel at the write lock, before any byte"
        (fun () ->
          let result, received, _ =
            run
              (tls_script () @ [F.Recv])
              (with_tls (X.config ~ca:X.ca_file Verify_identity) (fun c ->
                   let r =
                     match
                       Fiber.with_timeout Mono (Span.ms 1) (fun () ->
                           ( try Time.sleep Mono (Span.s 1)
                             with Fiber.Cancelled _ -> () ) ;
                           Conn.exec c (sql "INSERT") )
                     with
                     | _ ->
                         "no cancel"
                     | exception Fiber.Timeout ->
                         "cancelled"
                   in
                   Printf.sprintf "%s %b %b" r (Conn.sent c) (Conn.usable c) )
              )
          in
          equal string "cancelled false false" result ;
          equal int 4 (List.length received) )
    ; test "the write rule: a cancel in the TCP write of 100 KB" (fun () ->
          let result, received, _ =
            run
              (tls_script () @ [F.Hang])
              (with_tls (X.config ~ca:X.ca_file Verify_identity) (fun c ->
                   let r =
                     match
                       Fiber.with_timeout Mono (Span.ms 100) (fun () ->
                           Conn.exec c big )
                     with
                     | _ ->
                         "no cancel"
                     | exception Fiber.Timeout ->
                         "cancelled"
                   in
                   (* The connection is broken: close sends no COM_QUIT. *)
                   Printf.sprintf "%s %b %b %b" r (Conn.sent c) (Conn.usable c)
                     (Conn.close c) ) )
          in
          equal string "cancelled false false true" result ;
          equal int 4 (List.length received) )
    ; test "a read deadline and a write deadline on TLS" (fun () ->
          let deadline c =
            Conn.set_deadline c (Time.add Mono (Time.now Mono) (Span.s 1))
          in
          let read, received, _ =
            run
              (tls_script () @ [F.Recv; F.Hang])
              (with_tls (X.config ~ca:X.ca_file Verify_identity) (fun c ->
                   deadline c ;
                   T.error_text (Result.get_error (Conn.exec c (sql "INSERT"))) )
              )
          in
          equal string "Outcome_unknown statement" read ;
          equal int 5 (List.length received) ;
          let write, _, _ =
            run
              (tls_script () @ [F.Hang])
              (with_tls (X.config ~ca:X.ca_file Verify_identity) (fun c ->
                   deadline c ;
                   T.error_text (Result.get_error (Conn.exec c big)) ) )
          in
          equal string "Timeout" write )
    ; test "a slow CA file: the limit of 10 s" (fun () ->
          (* Each File job of the simulator takes up to 1000 s: the read of the
             CA file is inside the limit of the connect. *)
          let result, received, _ =
            run [] (fun sim ->
                Weft_sim.set_blocking_cost sim (Span.s 1000) ;
                let t0 = Time.now Mono in
                let r =
                  connect_close (X.config ~ca:X.ca_file Verify_identity) sim
                in
                Printf.sprintf "%s %Ld" r
                  (Int64.div
                     (Span.to_ns (Time.diff Mono (Time.now Mono) t0))
                     1_000_000_000L ) )
          in
          equal string "Connect timeout 10" result ;
          equal (list (pair int string)) [] received )
    ; cases "a cancel at the lock of the TLS session in the stale check"
        ~id:T.id3
        [ ( "Db.exec"
          , (fun pool -> ignore (Cistern.Db.exec pool (Test_pool.q "SELECT 1")))
          , "query \"SELECT 1\" cancelled sent=false" )
        ; ( "Db.transaction"
          , (fun pool -> ignore (Cistern.Db.transaction pool ignore))
          , "query \"START TRANSACTION\" cancelled sent=false" ) ]
        (fun (_, run, expected) ->
          (* The server sends 4031 on the idle connection: a complete record
             waits in the socket. The stale check then takes the lock of the
             session, which raises the pending cancel. It is a cancel at the
             checkout: its event comes, and the entry is discarded. *)
          let (r, connects), events =
            with_tls_pool (fun _ db pool ->
                db.faults.idle_4031_ms <- Some 20 ;
                ignore (Cistern.Db.exec pool (Test_pool.clean ())) ;
                db.faults.idle_4031_ms <- None ;
                Time.sleep Mono (Span.ms 50) ;
                Cistern.Private.Pool.Private.reset_counters () ;
                let r = cancelled_in_checkout (fun () -> run pool) in
                let in_stale =
                  (Cistern.Private.Pool.Private.counters ()).cancel_in_stale
                in
                ignore (Cistern.Db.exec pool (Test_pool.clean ())) ;
                ((r, in_stale), db.connects) )
          in
          equal string "cancelled" (fst r) ;
          equal ~msg:"the counter cancel_in_stale" int 1 (snd r) ;
          equal int 2 connects ;
          is_true
            ~msg:("the event: " ^ String.concat "; " events)
            (List.mem expected events) )
    ; test "the pool on TLS: Db.exec, Db.transaction and their events"
        (fun () ->
          let (a, b, sessions), events =
            with_tls_pool (fun _ db pool ->
                let a =
                  Test_pool.result_text
                    (Cistern.Db.exec pool (Test_pool.q "SELECT 5"))
                in
                let b =
                  match
                    Cistern.Db.transaction pool (fun tx ->
                        Cistern.Tx.exec_exn tx
                          (Cistern.Raw.exec (sql "UPDATE a")) )
                  with
                  | Ok _ ->
                      "Ok"
                  | Error e ->
                      T.error_text e
                in
                (a, b, db.tls_sessions) )
          in
          equal string "5" a ;
          equal string "Ok" b ;
          equal int 1 sessions ;
          equal (list string)
            [ "query \"SELECT 5\" done 1"
            ; "query \"START TRANSACTION\" done 0"
            ; "query \"UPDATE a\" done 1"
            ; "query \"COMMIT\" done 0" ]
            events )
    ; test "the pool on TLS: an idle connection that the server ends" (fun () ->
          (* The server sends 4031 and ends the idle TLS session with no
             close_notify. The stale check finds it, and the next exec takes a
             new connection with no error. *)
          let (r, connects, stale), _ =
            with_tls_pool (fun _ db pool ->
                db.faults.idle_4031_ms <- Some 20 ;
                ignore (Cistern.Db.exec pool (Test_pool.clean ())) ;
                db.faults.idle_4031_ms <- None ;
                Time.sleep Mono (Span.ms 50) ;
                Cistern.Private.Pool.Private.reset_counters () ;
                let r =
                  Test_pool.result_text
                    (Cistern.Db.exec pool (Test_pool.q "SELECT 6"))
                in
                ( r
                , db.connects
                , (Cistern.Private.Pool.Private.counters ()).stale_at_checkout
                ) )
          in
          equal string "6" r ; equal int 2 connects ; equal int 1 stale )
    ; test "close: the cancel in the TCP write of COM_QUIT" (fun () ->
          (* The deaf method of 1.2: the server sends the OK of the next
             command, then reads nothing. The command fills the receive buffer
             of the server (65536 bytes) exactly: 65448 bytes of plaintext in 4
             records of TLS 1.3 (22 bytes of each record are not plaintext). The
             command ends, and the connection is usable with a full buffer. Then
             COM_QUIT waits in the TCP write, and the cancel comes there. *)
          let fill = sql ("SELECT '" ^ String.make (65443 - 9) 'a' ^ "'") in
          let result, _, _ =
            run
              (tls_script () @ [F.Send (1, F.ok ()); F.Hang])
              (fun sim ->
                match Conn.connect (X.config ~ca:X.ca_file Verify_identity) with
                | Error e ->
                    T.error_text e
                | Ok c ->
                    let a =
                      match
                        Fiber.with_timeout Mono (Span.ms 100) (fun () ->
                            Conn.exec c fill )
                      with
                      | Ok _ ->
                          "ok"
                      | Error e ->
                          T.error_text e
                      | exception Fiber.Timeout ->
                          "the command waited"
                    in
                    let b =
                      match
                        Fiber.with_timeout Mono (Span.ms 500) (fun () ->
                            Conn.close c )
                      with
                      | _ ->
                          "no cancel"
                      | exception Fiber.Timeout ->
                          "cancelled"
                    in
                    T.no_client_fd sim ;
                    a ^ " " ^ b ^ " " ^ string_of_bool (Conn.usable c) )
          in
          equal string "ok cancelled false" result )
    ; test "a TLS handshake past the limit of 10 s" (fun () ->
          let result, _, _ =
            run
              [F.Send (0, F.greeting ()); F.Recv; F.Hang]
              (fun sim ->
                let t0 = Time.now Mono in
                let r =
                  connect_close (X.config ~ca:X.ca_file Verify_identity) sim
                in
                Printf.sprintf "%s %Ld" r
                  (Int64.div
                     (Span.to_ns (Time.diff Mono (Time.now Mono) t0))
                     1_000_000_000L ) )
          in
          equal string "Connect timeout 10" result ) ]
