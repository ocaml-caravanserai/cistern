(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(** One connection to a server (DESIGN.md, "Handshake", "Authentication" and
    "Session").

    Phase 1.1 has no pool and no TLS. A connection has one user: a second call
    at the same time raises [Invalid_argument]. Thus a [close] from another
    fiber during a [ping] raises [Invalid_argument]. The functions need a weft
    fiber. *)

type t

(** The values of the session, from the greeting and from round trip 1. *)
type info =
  { thread_id: int  (** The thread id of the greeting (4 bytes). *)
  ; connection_id: int64  (** [CONNECTION_ID()]. *)
  ; version: string  (** [@@version], not the version of the greeting. *)
  ; version_comment: string
  ; dialect: Cistern_core.Dialect.t
  ; hostname: string
  ; port: int
  ; server_id: int64
  ; wait_timeout: int
  ; innodb_rollback_on_timeout: bool
  ; max_allowed_packet: int }

val connect : Config.t -> (t, Cistern_core.Error.t) result
(** [connect config] opens a connection, authenticates, and runs the first two
    round trips of the session:
    + [SELECT CONNECTION_ID(), @@version, @@version_comment, @@hostname, @@port,
       @@server_id, @@wait_timeout, @@innodb_rollback_on_timeout,
       @@max_allowed_packet]. The dialect comes from [@@version] and
      [@@version_comment].
    + [SET NAMES utf8mb4 COLLATE <collation of the dialect>, time_zone =
       '+00:00', sql_mode = '<the complete mode>', autocommit = 1].

    The rules:
    - A configuration that needs TLS gives
      [Connect "TLS is not supported before phase 1.4"] at once: before the DNS
      resolve, and before a socket opens.
    - A host name goes to [Weft.Dns.resolve], and the client tries the addresses
      in order. An IP address and [localhost] (127.0.0.1) go straight to the
      connect.
    - One timeout of 10 s covers the resolve, the connect, the greeting, the
      authentication and the two round trips.
    - The server must have [CLIENT_PROTOCOL_41], [CLIENT_SECURE_CONNECTION],
      [CLIENT_PLUGIN_AUTH], [CLIENT_DEPRECATE_EOF] and [CLIENT_CONNECT_WITH_DB].
    - The authentication: [mysql_native_password], or the fast path of
      [caching_sha2_password]. The full path sends the password in clear text
      only on a Unix socket with [allow-cleartext=socket]. Else the connection
      closes, no password byte goes, and the result is
      [Connect "the server asks for full authentication; use VERIFY_IDENTITY,
       ssl-fingerprint, or allow-cleartext=socket"]. At most one
      [AuthSwitchRequest], to one of the two plugins, with 20 bytes and a NUL.
    - Each failure gives [Connect msg]. When a server error caused it, [msg] has
      the error number, but not the server message, which can hold a user name.
      A plugin name of the server is escaped if it is not 1 to 64 printable
      ASCII bytes.
    - The flow closes on each failure. A cancel of the fiber goes up as
      [Weft.Fiber.Cancelled]. *)

val info : t -> info

val ping : t -> (unit, Cistern_core.Error.t) result
(** [ping t] sends [COM_PING]. A server error gives [Error.of_server]. The end
    of the stream, a reset, a failed write, a bad packet, or an error that
    [Error.of_server] maps to [Connection_lost] give [Connection_lost], and the
    connection is then broken. Each later [ping] on a broken connection gives
    [Connection_lost] before any I/O.

    [ping] has no timeout: if the server does not answer, only a cancel of the
    fiber of [ping] stops it. The cancel breaks the connection. *)

val close : t -> unit
(** [close t] sends [COM_QUIT], with a write timeout of 1 s and no wait for a
    reply, then closes the flow. A broken connection sends nothing. A second
    [close] does nothing. *)
