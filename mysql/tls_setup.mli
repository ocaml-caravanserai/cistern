(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(** The TLS client configuration of a {!Config.t} (DESIGN.md, "Configuration",
    phase 1.4): the check of the certificate for each [ssl-mode], and the rule
    that tells if the session is verified for the clear-text rule. It does no
    I/O: the caller reads the CA file and gives the time. *)

val ca_certificates : string -> (X509.Certificate.t list, string) result
(** [ca_certificates text] is the certificates of the PEM [text] of a CA file. A
    parse error and a text with no certificate give the same message, with no
    part of [text]: ["the CA file has no valid certificate"]. *)

type t =
  { config: Tls.Config.client  (** For [Weft_tls.client]. *)
  ; verified: bool
        (** The session can get the password in clear text after the handshake
            passed with [config]: [Verify_identity] with [ssl-ca], or a pin
            ([ssl-fingerprint]). An authenticator of the program alone is never
            verified, because it can accept each certificate. *) }

val make :
     time:(unit -> Ptime.t option)
  -> cas:X509.Certificate.t list
  -> Config.t
  -> (t, string) result
(** [make ~time ~cas config] is the TLS configuration of [config]. [cas] is the
    content of the [ssl-ca] file of [config] (see {!ca_certificates}), and [[]]
    if [config] has no [ssl-ca]. The checks:
    - [Required]: each certificate passes. With a pin, only the pinned
      certificate passes, and no name is checked.
    - [Verify_ca]: the chain of [ssl-ca] or of the authenticator, with no name.
      With a pin, the pin too.
    - [Verify_identity]: as [Verify_ca], with the host of [config]: the IP
      address for an IP literal, else the host name (also [localhost]).
    - With only a pin, the pin alone. Under [Verify_identity], the pin also
      checks the name.
    - Each check uses [time]. The pin checks the validity of the certificate. If
      [time ()] gives [None], each check fails: x509 checks no time then.

    [Error] for a host that is not a valid name, for [ssl-ca] with no
    certificate in [cas], and for an error of [Tls.Config.client].

    @raise Invalid_argument if [config] has no TLS. *)

val now : unit -> Ptime.t option
(** [now ()] is the time of the wall clock of weft ([Weft.Time.now Posix]), for
    [make]. The simulator controls it. *)
