(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(** A test CA and a server certificate for the test servers (DESIGN.md,
    "Database servers for the tests"). The method of weft's
    [test/tls/tls_support.ml]. All keys are ECDSA P-256: MySQL 8.4.11 and
    MariaDB 11.4.13 and 11.8.8 accept them. *)

type t =
  { ca: X509.Certificate.t
        (** Self-signed. The subject has the common name "cistern test CA".
            Basic constraints: a CA (critical). Key usage: key cert sign
            (critical). *)
  ; cert: X509.Certificate.t
        (** Signed by [ca]. By default, the subject has the common name
            "localhost", and the subject alternative names are the DNS name
            [localhost] and the IP address [127.0.0.1] (see {!make}). Extended
            key usage: server auth. Basic constraints: not a CA (critical). *)
  ; key: X509.Private_key.t  (** The P-256 key of [cert]. *)
  ; other_ca: X509.Certificate.t
        (** Another self-signed CA, "cistern other CA", with the extensions of
            [ca]. No certificate of this module has it as issuer: a test uses it
            as a CA of another PKI. *) }

val valid_from : Ptime.t
(** 2020-01-01T00:00:00Z. *)

val valid_until : Ptime.t
(** 2040-01-01T00:00:00Z. The two certificates have the same validity. *)

val make : ?cn:string -> ?dns:string list -> ?ips:string list -> unit -> t
(** [make ()] makes three new keys and the three certificates. [cn] (default
    ["localhost"]) is the common name of [cert], [dns] (default [["localhost"]])
    its DNS names and [ips] (default [["127.0.0.1"]]) its IP addresses. With no
    DNS name and no IP address, [cert] has no subject alternative names (as the
    certificate that MySQL makes). It uses the default generator of
    mirage-crypto-rng: the caller initializes it first. *)

val fingerprint : X509.Certificate.t -> string
(** [fingerprint cert] is the SHA-256 of the DER of [cert], in lowercase hex:
    the value of [ssl-fingerprint=sha256:]. *)

val files : t -> (string * string) list
(** The names and the text of the files, in this order: [ca.pem],
    [server-cert.pem], [server-key.pem] (PKCS#8, [BEGIN PRIVATE KEY]),
    [server-cert.sha256] ({!fingerprint} of [cert], with no newline) and
    [other-ca.pem]. *)

val write : dir:string -> t -> (unit, string) result
(** [write ~dir t] makes the directory [dir] with [files t] in it. [dir] must
    not exist, and its parent must exist. [write] writes the files into a new
    temporary directory (mode 0o700) next to [dir], then it renames this
    directory to [dir]. Thus [dir] has all the files or does not exist, also
    after a failure or a second [write] at the same time. [server-key.pem] gets
    the mode 0o600. [Error msg] names the path. *)
