(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(** The configuration of a connection (DESIGN.md, "Configuration").

    Phase 1.1 parses the TLS settings with their final rules, but it has no TLS:
    the connect refuses each configuration that needs TLS ({!needs_tls}). Phase
    1.4 adds TLS. *)

type ssl_mode =
  | Required  (** TLS, with no check of the certificate. *)
  | Verify_ca  (** TLS, and a check of the chain. *)
  | Verify_identity  (** TLS, and a check of the chain and of the name. *)

type address =
  | Tcp of {host: string; port: int}
      (** An IP address, [localhost] (TCP to 127.0.0.1, not the socket) or a
          host name. *)
  | Socket of {path: string; allow_cleartext: bool}
      (** A Unix socket. [allow_cleartext] lets the full path of
          [caching_sha2_password] send the password in clear text on this socket
          (DESIGN.md, "Authentication"). *)

type tls =
  | No_tls
  | Tls of {mode: ssl_mode; ca: string option; fingerprint: string option}
      (** [ca] is the path of a CA file. [fingerprint] is the 32 bytes of the
          SHA-256 hash of the server certificate. When [fingerprint] is [Some],
          phase 1.4 checks the pin in each mode. [Required] with no fingerprint
          is the only case with no check of the certificate. [Required] with a
          [ca] is an error, because the CA would have no effect. *)

(** A configuration. The type is abstract, so that no generic printer shows the
    password. *)
type t

val address : t -> address

val user : t -> string

val password : t -> string

val database : t -> string

val tls : t -> tls

val kill : t -> bool
(** [false] turns [KILL] off (phase 1.5). The default is [true]. *)

val needs_tls : t -> bool
(** [needs_tls t] is true if [tls t] is not [No_tls]. *)

val of_url : string -> (t, string) result
(** [of_url url] parses
    [mysql://user:password@host:port/database?key=value&...]. [mariadb://] is an
    alias of [mysql://]. The scheme is in lowercase.
    - The URL must not have a space or a control character: use [%20] and the
      other escapes. The user, the password, the database and the values are
      percent-decoded, and [+] stays [+]. A decoded value must not have a
      control character (also NUL).
    - In the URL, the characters [@], [/], [?] and [#] of the user and the
      password must be percent-encoded. The password can have [:]. A raw [@] in
      the database part is an error, because it is probably a part of a password
      that is not encoded.
    - The host is an IPv4 address, a name, or an IPv6 address in brackets (an
      IPv6 address with no zone id: [fe80::1%25eth0] is an error. An IPv4 tail
      has no octet with a leading zero). The port is 1 to 65535, and the default
      is 3306.
    - The database is necessary.
    - The keys are [socket=<path>], [ssl-mode=<mode>], [ssl-ca=<path>],
      [ssl-fingerprint=sha256:<64 hex digits>], [allow-cleartext=socket] and
      [kill=true|false]. The keys, [sha256:], [socket], [true] and [false] are
      in lowercase. The modes are [DISABLED], [REQUIRED], [VERIFY_CA] and
      [VERIFY_IDENTITY], with no regard to case. An unknown key, a key two
      times, or an empty value is an error.

    The address and the TLS settings:
    - [socket=] gives a Unix socket. The URL then has no host, no port, and no
      TLS setting other than [ssl-mode=DISABLED]: version 1 has no TLS on a Unix
      socket.
    - [allow-cleartext=socket] is only for [socket=].
    - TCP with [ssl-mode=DISABLED]: no TLS. [ssl-ca] and [ssl-fingerprint] are
      then errors.
    - TCP with [ssl-mode=REQUIRED]: TLS. With [ssl-fingerprint], the pin is
      checked. With [ssl-ca], the URL is an error: use [VERIFY_CA] or
      [VERIFY_IDENTITY].
    - TCP with [ssl-mode=VERIFY_CA] or [VERIFY_IDENTITY]: [ssl-ca] or
      [ssl-fingerprint] is necessary.
    - TCP with no [ssl-mode]: [ssl-ca] gives [Verify_identity]. Only
      [ssl-fingerprint] gives [Required] with the pin. With neither, the URL is
      an error.

    A message of [Error] has no part of the URL, thus no part of the password.
*)

val make :
     ?tls:tls
  -> ?kill:bool
  -> address:address
  -> user:string
  -> password:string
  -> database:string
  -> unit
  -> (t, string) result
(** [make ~address ~user ~password ~database ()] is a configuration with the
    rules of {!of_url}, from one shared check: the user, the password and the
    database, the syntax of the host (an IPv6 address has no brackets here), the
    port, the paths (not empty, no control character) and the TLS rules. The
    default of [tls] is [No_tls] for a socket. A TCP address must have [tls].
    The default of [kill] is [true]. *)

val pp : Format.formatter -> t -> unit
(** [pp] prints the fields. The password shows as [***] if it is not empty. *)
