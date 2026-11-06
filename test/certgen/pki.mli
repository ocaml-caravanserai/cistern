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
        (** Signed by [ca]. The subject has the common name "localhost". Subject
            alternative names: the DNS name [localhost] and the IP address
            [127.0.0.1]. Extended key usage: server auth. Basic constraints: not
            a CA (critical). *)
  ; key: X509.Private_key.t  (** The P-256 key of [cert]. *) }

val valid_from : Ptime.t
(** 2020-01-01T00:00:00Z. *)

val valid_until : Ptime.t
(** 2040-01-01T00:00:00Z. The two certificates have the same validity. *)

val make : unit -> t
(** [make ()] makes two new keys and the two certificates. It uses the default
    generator of mirage-crypto-rng: the caller initializes it first. *)

val files : t -> (string * string) list
(** The names and the PEM text of the files, in this order: [ca.pem],
    [server-cert.pem] and [server-key.pem] (PKCS#8, [BEGIN PRIVATE KEY]). *)

val write : dir:string -> t -> (unit, string) result
(** [write ~dir t] makes the directory [dir] with [files t] in it. [dir] must
    not exist, and its parent must exist. [write] writes the files into a new
    temporary directory (mode 0o700) next to [dir], then it renames this
    directory to [dir]. Thus [dir] has the three files or does not exist, also
    after a failure or a second [write] at the same time. [server-key.pem] gets
    the mode 0o600. [Error msg] names the path. *)
