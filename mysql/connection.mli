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

(** {1 Commands}

    The outcome of each command follows DESIGN.md, "Order of the error rules"
    ({!Outcome.decide}). [query] and [exec] are writes. [ping], [init_db] and
    [reset] are not.

    - A command on a broken or closed connection gives [Connection_lost] before
      any I/O.
    - If the flush of the command packet raises, the result is
      [Connection_lost]: the server did not get the command.
    - The end of the stream after the flush gives [Outcome_unknown `Statement]
      for a write, else [Connection_lost].
    - A server error gives [Error.of_server], and the connection stays usable,
      except for 1153 and for an error that ends the session.
    - A cancel of the fiber breaks the connection, and [Weft.Fiber.Cancelled]
      goes up.

    No command has a timeout: if the server does not answer, only a cancel of
    the fiber stops it. *)

val query :
     t
  -> Cistern_core.Raw.t
  -> (Cistern_core.Raw.result, Cistern_core.Error.t) result
(** [query t r] runs the raw statement [r] with [COM_QUERY], and gives its first
    result set. A statement with no result set gives no columns and no rows. The
    client reads the other results (a [CALL] has more than one) to the end, and
    an error in any of them is the result.

    - A value with no literal, or SQL text whose packet does not fit in
      [@@max_allowed_packet] (see {!Packet.query_fits}), gives [Invalid_value]
      before any byte goes. The connection stays usable.
    - A value of the result that does not decode ({!Column.row}) gives [Decode],
      after the client read the whole reply. The connection stays usable. *)

val exec :
     t
  -> Cistern_core.Raw.t
  -> (Cistern_core.Exec_result.t, Cistern_core.Error.t) result
(** [exec t r] runs [r] as {!query} does, ignores the rows, and gives the counts
    of the last OK packet. *)

val ping : t -> (unit, Cistern_core.Error.t) result
(** [ping t] sends [COM_PING]. *)

val init_db : t -> string -> (unit, Cistern_core.Error.t) result
(** [init_db t db] sends [COM_INIT_DB]: [db] becomes the database of the
    session. *)

val reset : t -> (unit, Cistern_core.Error.t) result
(** [reset t] is the session reset of DESIGN.md, "Session":
    [COM_RESET_CONNECTION] (the session variables get their global values, and
    an open transaction rolls back), the [SET] of round trip 2, and
    [COM_INIT_DB] with the database of the configuration. If a step fails, the
    connection breaks. *)

val status : t -> int
(** [status t] is the status flags of the last OK packet
    ({!Message.has_status}), for example [In_trans]. *)

val close : t -> unit
(** [close t] sends [COM_QUIT], with a write timeout of 1 s and no wait for a
    reply, then closes the flow. A broken connection sends nothing. A second
    [close] does nothing. *)

(** For the tests only. It can change in each version. *)
module Private : sig
  val reset_connection : t -> (unit, Cistern_core.Error.t) result
  (** [COM_RESET_CONNECTION] alone, with no [SET] and no [COM_INIT_DB]. *)

  val last_error : t -> Message.err option
  (** The error packet of the last call, if its result came from one. Each call
      clears it first, also a call on a broken connection. The claims tests read
      the error number and the SQLSTATE of the server with it. *)

  val query_text :
       t
    -> Cistern_core.Raw.t
    -> (string option array list, Cistern_core.Error.t) result
  (** [query_text t r] is {!query} with the text of the values as the server
      sent them, with no map to [Value.t]. The claims tests read the text of
      [DOUBLE] with it. *)
end
