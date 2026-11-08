(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* The integration tests of TLS on the real servers (the plan of phase 1.4,
   section 4.3): each row of the table of the verified configurations
   (DESIGN.md, "Configuration"), the failures of a CA of another PKI and of
   another pin, the full path of caching_sha2_password through the clear-text
   rule on MySQL, and the pool on TLS. The group [timing] (nightly) has the idle
   timeout over TLS. *)

open Windtrap
open Support
module Db = Cistern.Db
module Tx = Cistern.Tx
module Raw = Cistern.Raw
module Span = Weft.Span
module Time = Weft.Time

let mono = Weft.Clock.Mono

(* An authenticator of the program: the chain of the CA file [path]. *)
let chain_of path =
  match
    Cistern_mysql.Tls_setup.ca_certificates
      (In_channel.with_open_bin path In_channel.input_all)
  with
  | Ok cas ->
      X509.Authenticator.chain_of_trust ~time:Cistern_mysql.Tls_setup.now cas
  | Error m ->
      failwith m

let chain = chain_of ca

(* The authenticator "none" of x509: it accepts each certificate. *)
let none =
  match X509.Authenticator.of_string "none" with
  | Ok f ->
      f Cistern_mysql.Tls_setup.now
  | Error (`Msg m) ->
      failwith m

(* The rows of the table of the verified configurations, with "verified" for the
   clear-text rule. The address is 127.0.0.1, or localhost (a name of the
   certificate). *)
let rows =
  [ ("REQUIRED", (fun () -> on_tls ~mode:Required ~ca:None ()), false)
  ; ( "REQUIRED with the pin"
    , (fun () -> on_tls ~mode:Required ~ca:None ~fingerprint ())
    , true )
  ; ("VERIFY_CA with the CA", (fun () -> on_tls ~mode:Verify_ca ()), false)
  ; ( "VERIFY_CA with the pin"
    , (fun () -> on_tls ~mode:Verify_ca ~ca:None ~fingerprint ())
    , true )
  ; ( "VERIFY_IDENTITY with the CA, by the IP address"
    , (fun () -> on_tls ())
    , true )
  ; ( "VERIFY_IDENTITY with the CA, by the name localhost"
    , (fun () -> on_tls ~host:"localhost" ())
    , true )
  ; ( "VERIFY_IDENTITY with the pin"
    , (fun () -> on_tls ~ca:None ~fingerprint ())
    , true )
  ; ( "VERIFY_IDENTITY with only an authenticator"
    , (fun () -> on_tls ~ca:None ~authenticator:chain ())
    , false )
  ; ( "VERIFY_IDENTITY with the authenticator none"
    , (fun () -> on_tls ~ca:None ~authenticator:none ())
    , false ) ]

(* The IPv4-mapped IPv6 address of the server: an IPv6 socket reaches the
   server, and the certificate does not have this address. *)
let mapped = "::ffff:127.0.0.1"

(* A plain TCP connect to [mapped] works: true on Linux. *)
let mapped_reachable () =
  match Weft_flow.Tcp.connect (Weft.Net.Tcp (mapped, port)) with
  | flow ->
      Weft_flow.Flow.close flow ; true
  | exception Weft.Net.Error _ ->
      false

let contains s sub =
  let n = String.length s and k = String.length sub in
  let rec at i = i + k <= n && (String.sub s i k = sub || at (i + 1)) in
  at 0

let deterministic =
  group ("TLS on " ^ server)
    [ cases "each configuration connects"
        ~id:(fun (n, _, _) -> n)
        rows
        (fun (_, config, _) ->
          in_weft (fun () ->
              (* On MySQL, the cache of caching_sha2_password is warm: the fast
                 path. *)
              warm () ;
              equal string "connected" (connect_ping (config ())) ) )
    ; cases "the full path of caching_sha2_password"
        ~id:(fun (n, _, _) -> n)
        rows
        (fun (_, config, verified) ->
          in_weft (fun () ->
              (* An empty cache on MySQL: the server asks for the full path, and
                 the password goes only in a verified session. MariaDB uses
                 mysql_native_password: each row connects. *)
              flush_privileges () ;
              equal string
                (if verified || not mysql then "connected" else gate)
                (connect_ping (config ())) ) )
    ; test "a CA of another PKI, and another pin: TLS failures" (fun () ->
          in_weft (fun () ->
              let other =
                connect_ping (on_tls ~mode:Verify_ca ~ca:(Some other_ca) ())
              in
              is_true ~msg:other
                ( String.starts_with ~prefix:"Connect TLS: " other
                && contains other "invalid certificate chain" ) ;
              (* The authenticator of the program runs: one of another PKI
                 refuses the server. *)
              equal string
                "Connect TLS: authentication failure: invalid certificate chain"
                (connect_ping
                   (on_tls ~ca:None ~authenticator:(chain_of other_ca) ()) ) ;
              let pin =
                connect_ping
                  (on_tls ~mode:Required ~ca:None
                     ~fingerprint:(String.make 32 'x') () )
              in
              (* The cause stays at the end of the text. *)
              is_true ~msg:pin
                ( String.starts_with ~prefix:"Connect TLS: " pin
                && contains pin "fingerprint"
                && contains pin "expected 7878" ) ) )
    ; test "a wrong identity: the IPv4-mapped address ::ffff:127.0.0.1"
        (fun () ->
          in_weft (fun () ->
              if not (mapped_reachable ()) then
                skip
                  ~reason:
                    "a plain TCP connect to ::ffff:127.0.0.1 does not work here"
                  () ;
              (* The connection reaches the server: REQUIRED and VERIFY_CA check
                 no name (the gate message with an empty cache on MySQL).
                 VERIFY_IDENTITY fails, with the cause at the end. *)
              flush_privileges () ;
              let reach = if mysql then gate else "connected" in
              equal ~msg:"REQUIRED" string reach
                (connect_ping (on_tls ~host:mapped ~mode:Required ~ca:None ())) ;
              flush_privileges () ;
              equal ~msg:"VERIFY_CA" string reach
                (connect_ping (on_tls ~host:mapped ~mode:Verify_ca ())) ;
              List.iter
                (fun (name, config) ->
                  let r = connect_ping config in
                  is_true
                    ~msg:(name ^ ": " ^ r)
                    ( String.starts_with ~prefix:"Connect TLS: " r
                    && contains r ("does not contain the IP " ^ mapped) ) )
                [ ("VERIFY_IDENTITY with the CA", on_tls ~host:mapped ())
                ; ( "VERIFY_IDENTITY with the pin"
                  , on_tls ~host:mapped ~ca:None ~fingerprint () ) ] ) )
    ; test "the pool on TLS: KILL CONNECTION of an idle connection" (fun () ->
          in_weft (fun () ->
              warm () ;
              Db.with_db ~pool_size:1 (on_tls ()) (fun db ->
                  let id1 = Db_tests.int_text (Db_tests.connection_id db) in
                  with_conn (on_tls ()) (fun direct ->
                      kill (Int64.of_string id1) ;
                      kill (Conn.info direct).connection_id ;
                      (* The counters are global: the suite runs one test at a
                         time. *)
                      Cistern.Private.Pool.Private.reset_counters () ;
                      (* The server ends the TLS session with no close_notify:
                         the stale check finds the end at the checkout. *)
                      let direct_stale = Conn.stale direct in
                      let select =
                        Db_tests.first (Db.exec db (Db_tests.q "SELECT 5"))
                      in
                      let counters = Cistern.Private.Pool.Private.counters () in
                      let id2 = Db_tests.int_text (Db_tests.connection_id db) in
                      let tx =
                        Db.transaction db (fun tx ->
                            Db_tests.first (Tx.exec tx (Db_tests.q "SELECT 6")) )
                      in
                      equal string killed_idle_expected
                        (killed_idle_text ~direct_stale ~counters ~select ~id1
                           ~id2
                           ~tx:(Result.fold ~ok:Fun.id ~error:error_text tx) ) ) ) ) )
    ]

(* The timing tests (nightly). *)
let timing =
  group
    ("TLS timing on " ^ server)
    [ test "the idle timeout over TLS: Connection_lost, then a new connection"
        (fun () ->
          in_weft (fun () ->
              warm () ;
              with_conn (on_tls ()) (fun c ->
                  exec_ok c "SET SESSION wait_timeout = 1" ;
                  Time.sleep mono (Span.ms 2500) ;
                  (* MySQL sends 4031 over TLS, MariaDB ends the session: each
                     with no close_notify. *)
                  equal string "Connection_lost" (unit_text (Conn.ping c)) ) ;
              equal string "connected" (connect_ping (on_tls ())) ) ) ]
