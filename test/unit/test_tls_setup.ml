(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* Tls_setup: the check of the certificate for each ssl-mode, and the verified
   flag (the plan of phase 1.4, D4, table 3.1, and the review changes A-M1, A-L1
   and A-L4). Each row calls the authenticator of the result as ocaml-tls calls
   it: with the IP address and the peer name of the configuration. The
   certificates come from Cistern_certgen.Pki, with a fixed seed. *)

open Windtrap
module C = Cistern_mysql.Config
module S = Cistern_mysql.Tls_setup

let () =
  Mirage_crypto_rng.set_default_generator
    (Mirage_crypto_rng.create ~seed:"cistern unit tls_setup"
       (module Mirage_crypto_rng.Fortuna) )

module Pki = Cistern_certgen.Pki

let pki = Pki.make ()

(* A certificate of another PKI, with the same names. *)
let other = Pki.make ()

(* A certificate with no host name and no IP address, as the certificate that
   MySQL makes. *)
let bare = Pki.make ~cn:"MySQL_Server_Auto_Generated" ~dns:[] ~ips:[] ()

let pin cert = X509.Certificate.fingerprint `SHA256 cert

let at s =
  match Ptime.of_rfc3339 s with Ok (t, _, _) -> t | Error _ -> assert false

let today = at "2026-11-07T00:00:00Z"

(* An authenticator that accepts each certificate. *)
let any : X509.Authenticator.t = fun ?ip:_ ~host:_ _ -> Ok None

let config ?ca ?fingerprint ?authenticator ~host mode =
  Result.get_ok
    (C.make
       ~tls:(C.Tls {mode; ca= Option.map (fun _ -> "/ca.pem") ca; fingerprint})
       ?authenticator
       ~address:(C.Tcp {host; port= 3306})
       ~user:"u" ~password:"p" ~database:"db" () )

(* The result for [cert]: "pass" or "fail", and "verified" or "not verified".
   [ca] is the CA certificate of the ssl-ca file. *)
let run ?(time = Some today) ?ca ?fingerprint ?authenticator ~host mode cert =
  let c = config ?ca ?fingerprint ?authenticator ~host mode in
  match S.make ~time:(fun () -> time) ~cas:(Option.to_list ca) c with
  | Error m ->
      "error " ^ m
  | Ok {config; verified} ->
      let tc = Tls.Config.of_client config in
      let checked =
        (Option.get tc.authenticator) ?ip:tc.ip ~host:tc.peer_name [cert]
      in
      Printf.sprintf "%s %s"
        (if Result.is_ok checked then "pass" else "fail")
        (if verified then "verified" else "not verified")

let rows =
  [ ( "REQUIRED: any certificate"
    , (fun () -> run ~host:"127.0.0.1" Required other.cert)
    , "pass not verified" )
  ; ( "REQUIRED with a pin: the pinned certificate"
    , (fun () ->
        run ~fingerprint:(pin pki.cert) ~host:"127.0.0.1" Required pki.cert )
    , "pass verified" )
  ; ( "REQUIRED with a pin: another certificate"
    , (fun () ->
        run ~fingerprint:(pin pki.cert) ~host:"127.0.0.1" Required other.cert )
    , "fail verified" )
  ; ( "REQUIRED with a pin: no name is checked"
    , (fun () ->
        run ~fingerprint:(pin bare.cert) ~host:"localhost" Required bare.cert )
    , "pass verified" )
  ; ( "VERIFY_CA: the chain, and no name"
    , (fun () -> run ~ca:pki.ca ~host:"db.example.com" Verify_ca pki.cert)
    , "pass not verified" )
  ; ( "VERIFY_CA: another CA"
    , (fun () -> run ~ca:pki.ca ~host:"localhost" Verify_ca other.cert)
    , "fail not verified" )
  ; ( "VERIFY_CA with a CA and a pin"
    , (fun () ->
        run ~ca:pki.ca ~fingerprint:(pin pki.cert) ~host:"localhost" Verify_ca
          pki.cert )
    , "pass verified" )
  ; ( "VERIFY_CA with a CA and a pin of another certificate"
    , (fun () ->
        run ~ca:pki.ca ~fingerprint:(pin other.cert) ~host:"localhost" Verify_ca
          pki.cert )
    , "fail verified" )
  ; ( "VERIFY_CA with a pin and a chain of another CA"
    , (fun () ->
        run ~ca:pki.ca ~fingerprint:(pin other.cert) ~host:"localhost" Verify_ca
          other.cert )
    , "fail verified" )
  ; ( "VERIFY_CA with only a pin: no name"
    , (fun () ->
        run ~fingerprint:(pin bare.cert) ~host:"localhost" Verify_ca bare.cert )
    , "pass verified" )
  ; ( "VERIFY_IDENTITY: the host name"
    , (fun () -> run ~ca:pki.ca ~host:"localhost" Verify_identity pki.cert)
    , "pass verified" )
  ; ( "VERIFY_IDENTITY: the IP address"
    , (fun () -> run ~ca:pki.ca ~host:"127.0.0.1" Verify_identity pki.cert)
    , "pass verified" )
  ; ( "VERIFY_IDENTITY: another name"
    , (fun () -> run ~ca:pki.ca ~host:"db.example.com" Verify_identity pki.cert)
    , "fail verified" )
  ; ( "VERIFY_IDENTITY: another IPv4 address"
    , (fun () -> run ~ca:pki.ca ~host:"127.0.0.2" Verify_identity pki.cert)
    , "fail verified" )
  ; ( "VERIFY_IDENTITY: an IPv6 address"
    , (fun () -> run ~ca:pki.ca ~host:"::1" Verify_identity pki.cert)
    , "fail verified" )
  ; ( "VERIFY_IDENTITY: another CA"
    , (fun () -> run ~ca:pki.ca ~host:"localhost" Verify_identity other.cert)
    , "fail verified" )
  ; ( "VERIFY_IDENTITY with only a pin: the name too"
    , (fun () ->
        run ~fingerprint:(pin bare.cert) ~host:"localhost" Verify_identity
          bare.cert )
    , "fail verified" )
  ; ( "VERIFY_IDENTITY with only a pin: a certificate with the name"
    , (fun () ->
        run ~fingerprint:(pin pki.cert) ~host:"localhost" Verify_identity
          pki.cert )
    , "pass verified" )
  ; ( "VERIFY_IDENTITY with only an authenticator: not verified"
    , (fun () ->
        run ~authenticator:any ~host:"localhost" Verify_identity other.cert )
    , "pass not verified" )
  ; ( "VERIFY_IDENTITY with an authenticator and a pin"
    , (fun () ->
        run ~authenticator:any ~fingerprint:(pin pki.cert) ~host:"localhost"
          Verify_identity pki.cert )
    , "pass verified" )
  ; ( "VERIFY_IDENTITY with an authenticator and a pin: another certificate"
    , (fun () ->
        run ~authenticator:any ~fingerprint:(pin pki.cert) ~host:"localhost"
          Verify_identity other.cert )
    , "fail verified" )
  ; ( "VERIFY_IDENTITY with an authenticator that refuses"
    , (fun () ->
        run
          ~authenticator:
            (X509.Authenticator.chain_of_trust
               ~time:(fun () -> Some today)
               [pki.ca] )
          ~host:"localhost" Verify_identity other.cert )
    , "fail not verified" )
  ; ( "VERIFY_CA with an authenticator: not verified"
    , (fun () ->
        run
          ~authenticator:
            (X509.Authenticator.chain_of_trust
               ~time:(fun () -> Some today)
               [pki.ca] )
          ~host:"localhost" Verify_ca pki.cert )
    , "pass not verified" )
  ; ( "after the validity"
    , (fun () ->
        run
          ~time:(Some (at "2041-01-01T00:00:00Z"))
          ~ca:pki.ca ~host:"localhost" Verify_identity pki.cert )
    , "fail verified" )
  ; ( "before the validity"
    , (fun () ->
        run
          ~time:(Some (at "2019-01-01T00:00:00Z"))
          ~ca:pki.ca ~host:"localhost" Verify_identity pki.cert )
    , "fail verified" )
  ; ( "a pin after the validity"
    , (fun () ->
        run
          ~time:(Some (at "2041-01-01T00:00:00Z"))
          ~fingerprint:(pin pki.cert) ~host:"localhost" Required pki.cert )
    , "fail verified" )
  ; ( "no time: REQUIRED fails"
    , (fun () -> run ~time:None ~host:"localhost" Required pki.cert)
    , "fail not verified" )
  ; ( "no time: VERIFY_IDENTITY fails"
    , (fun () ->
        run ~time:None ~ca:pki.ca ~host:"localhost" Verify_identity pki.cert )
    , "fail verified" )
  ; ( "a host that is not a name of TLS"
    , (fun () ->
        run ~ca:pki.ca ~host:"db_1.example.com" Verify_identity pki.cert )
    , "error the host is not a valid name for TLS" )
  ; ( "an IPv4 literal that is not an address"
    , (fun () -> run ~ca:pki.ca ~host:"1.2.3.999" Verify_identity pki.cert)
    , "error the host is not a valid IP address for TLS" )
  ; ( "a host that is not a name, under VERIFY_CA: no name is used"
    , (fun () -> run ~ca:pki.ca ~host:"db_1.example.com" Verify_ca pki.cert)
    , "pass not verified" ) ]

(* The peer name and the IP address that ocaml-tls gets. *)
let names host mode =
  let tc =
    Tls.Config.of_client
      (Result.get_ok
         (S.make
            ~time:(fun () -> Some today)
            ~cas:[pki.ca]
            (config ~ca:pki.ca ~host mode) ) )
        .config
  in
  Printf.sprintf "%s %s"
    (Option.fold ~none:"-" ~some:Domain_name.to_string tc.peer_name)
    (Option.fold ~none:"-" ~some:Ipaddr.to_string tc.ip)

let pem certs = String.concat "" (List.map X509.Certificate.encode_pem certs)

let tests =
  group "Tls_setup"
    [ cases "the checks of table 3.1"
        ~id:(fun (n, _, _) -> n)
        rows
        (fun (_, f, expected) -> equal string expected (f ()))
    ; test "the peer name or the IP address of the configuration" (fun () ->
          equal string "localhost -" (names "localhost" Verify_identity) ;
          equal string "- 127.0.0.1" (names "127.0.0.1" Verify_identity) ;
          equal string "- ::1" (names "::1" Verify_identity) ;
          (* Four labels, as an IPv4 address, but a name. *)
          equal string "db.a.example.com -"
            (names "db.a.example.com" Verify_identity) ;
          equal string "- -" (names "localhost" Verify_ca) )
    ; test "ssl-ca with no certificate" (fun () ->
          equal (result unit string)
            (Error "the CA file has no valid certificate")
            (Result.map ignore
               (S.make
                  ~time:(fun () -> Some today)
                  ~cas:[]
                  (config ~ca:pki.ca ~host:"localhost" Verify_identity) ) ) )
    ; test "no TLS: Invalid_argument" (fun () ->
          raises (Invalid_argument "Tls_setup.make: a configuration with no TLS")
            (fun () ->
              S.make
                ~time:(fun () -> Some today)
                ~cas:[]
                (Result.get_ok
                   (C.make ~tls:C.No_tls
                      ~address:(C.Tcp {host= "h"; port= 1})
                      ~user:"u" ~password:"p" ~database:"db" () ) ) ) )
    ; cases "ca_certificates"
        ~id:(fun (n, _, _) -> n)
        [ ("two certificates", pem [pki.ca; other.ca], Ok 2)
        ; ("empty", "", Error "the CA file has no valid certificate")
        ; ( "a key and no certificate"
          , X509.Private_key.encode_pem pki.key
          , Error "the CA file has no valid certificate" )
        ; ( "two good certificates, then a bad block: the whole file is refused"
          , pem [pki.ca; other.ca]
            ^ "-----BEGIN CERTIFICATE-----\n\
               aHVudGVyMiBzZWNyZXQ=\n\
               -----END CERTIFICATE-----\n"
          , Error "the CA file has no valid certificate" )
        ; ( "a block with bad content"
          , "-----BEGIN CERTIFICATE-----\n\
             aHVudGVyMiBzZWNyZXQ=\n\
             -----END CERTIFICATE-----\n"
          , Error "the CA file has no valid certificate" ) ]
        (fun (_, text, expected) ->
          equal (result int string) expected
            (Result.map List.length (S.ca_certificates text)) ) ]
