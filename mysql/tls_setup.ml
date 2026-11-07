(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

let ( let* ) = Result.bind

let ca_certificates text =
  match X509.Certificate.decode_pem_multiple text with
  | Ok (_ :: _ as cas) ->
      Ok cas
  | Ok [] | Error _ ->
      (* The error of the decoder can show the content in hex. *)
      Error "the CA file has no valid certificate"

type t = {config: Tls.Config.client; verified: bool}

(* Each check fails if the time is not known: with None, x509 checks no time. *)
let timed time (a : X509.Authenticator.t) : X509.Authenticator.t =
 fun ?ip ~host certs ->
  match time () with None -> Error `InvalidChain | Some _ -> a ?ip ~host certs

(* [a], then [b] if [a] passed. *)
let both (a : X509.Authenticator.t) (b : X509.Authenticator.t) :
    X509.Authenticator.t =
 fun ?ip ~host certs ->
  match a ?ip ~host certs with Ok _ -> b ?ip ~host certs | Error _ as e -> e

(* Each certificate passes: REQUIRED with no pin. *)
let any : X509.Authenticator.t = fun ?ip:_ ~host:_ _ -> Ok None

(* The name for VERIFY_IDENTITY: the IP address or the host name of the
   configuration, never the resolved address. *)
let name host =
  if Config.ip_literal host then
    match Ipaddr.of_string host with
    | Ok ip ->
        Ok (`Ip ip)
    | Error _ ->
        Error "the host is not a valid IP address for TLS"
  else
    match Result.bind (Domain_name.of_string host) Domain_name.host with
    | Ok n ->
        Ok (`Host n)
    | Error _ ->
        Error "the host is not a valid name for TLS"

let make ~time ~cas config =
  let host, mode, ca, fingerprint =
    match (Config.address config, Config.tls config) with
    | Config.Tcp {host; _}, Config.Tls {mode; ca; fingerprint} ->
        (host, mode, ca, fingerprint)
    | _ ->
        invalid_arg "Tls_setup.make: a configuration with no TLS"
  in
  let* chain =
    match (Config.authenticator config, ca) with
    | Some a, _ ->
        Ok (Some a)
    | None, Some _ ->
        if cas = [] then Error "the CA file has no valid certificate"
        else Ok (Some (X509.Authenticator.chain_of_trust ~time cas))
    | None, None ->
        Ok None
  in
  let pin =
    Option.map
      (fun fingerprint ->
        X509.Authenticator.cert_fingerprint ~time ~hash:`SHA256 ~fingerprint )
      fingerprint
  in
  let authenticator =
    match (mode, chain, pin) with
    | Config.Required, _, None ->
        any
    | Required, _, Some p ->
        p
    | (Verify_ca | Verify_identity), Some c, Some p ->
        both c p
    | (Verify_ca | Verify_identity), Some c, None ->
        c
    | (Verify_ca | Verify_identity), None, Some p ->
        p
    | (Verify_ca | Verify_identity), None, None ->
        (* Config refuses this configuration. *)
        invalid_arg "Tls_setup.make: no check for the certificate"
  in
  (* An authenticator never makes the session verified (A-M1). [ca] here never
     comes with an authenticator, because Config refuses ssl-ca with an
     authenticator (the unit rows "with ssl-ca" of test_config). *)
  let verified =
    Option.is_some fingerprint || (mode = Verify_identity && Option.is_some ca)
  in
  let authenticator = timed time authenticator in
  let* config =
    match mode with
    | Required | Verify_ca ->
        Ok (Tls.Config.client ~authenticator ())
    | Verify_identity -> (
        let* n = name host in
        match n with
        | `Ip ip ->
            Ok (Tls.Config.client ~authenticator ~ip ())
        | `Host peer_name ->
            Ok (Tls.Config.client ~authenticator ~peer_name ()) )
  in
  match config with
  | Ok config ->
      Ok {config; verified}
  | Error (`Msg m) ->
      Error ("the TLS configuration: " ^ m)

let now () =
  Ptime.of_float_s (Weft.Time.posix_to_unix_s (Weft.Time.now Weft.Clock.Posix))
