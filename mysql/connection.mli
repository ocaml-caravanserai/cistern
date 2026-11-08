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

(** The identity of a server (DESIGN.md, "Session"): with the exact start time,
    (identity, [CONNECTION_ID()]) names one session. The start time is in
    seconds. *)
type identity =
  | Mysql_server of {uuid: string; start: int64}  (** [@@server_uuid]. *)
  | Mariadb_server of
      { server_uid: string
      ; hostname: string
      ; port: int
      ; server_id: int64
      ; start: int64 }

(** The result of round trip 3. *)
type identity_check =
  | Unchecked  (** [kill=false]: round trip 3 did not run. *)
  | Checked of identity  (** All the checks passed: [KILL] can use it. *)
  | Refused of Cistern_core.Event.kill_off
      (** A check failed: [KILL] goes off for the pool, with this reason. *)

(** The values of the session, from the greeting and from round trips 1 and 3.
*)
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
  ; max_allowed_packet: int
  ; identity: identity_check }

val connect : Config.t -> (t, Cistern_core.Error.t) result
(** [connect config] opens a connection, authenticates, and runs the round trips
    of the session:
    + [SELECT CONNECTION_ID(), @@version, @@version_comment, @@hostname, @@port,
       @@server_id, @@wait_timeout, @@innodb_rollback_on_timeout,
       @@max_allowed_packet]. The dialect comes from [@@version] and
      [@@version_comment].
    + [SET NAMES utf8mb4 COLLATE <collation of the dialect>, time_zone =
       '+00:00', sql_mode = '<the complete mode>', autocommit = 1,
       completion_type = 'NO_CHAIN'], so that a [COMMIT] never chains a new
      transaction and never ends the session.
    + Only with [kill=true] (DESIGN.md, "Session"): one [SELECT] of
      [CONNECTION_ID()], the identity of the server ([@@server_uuid] on MySQL;
      [@@server_uid], [@@hostname], [@@port] and [@@server_id] on MariaDB), its
      exact start time ([UNIX_TIMESTAMP()] minus the uptime), and three counts:
      the rows of [information_schema.USER_PRIVILEGES] of the grantee of
      [CURRENT_USER()], the rows of [SUPER] and of the admin privilege
      ([CONNECTION_ADMIN], or [CONNECTION ADMIN] on MariaDB) among them, and the
      enabled roles ([COUNT(ROLE_NAME)] of [information_schema.ENABLED_ROLES]).
      The result is [info.identity]. A server error or a row that does not
      decode gives [Refused `Identity_failed]; then, in this order: a
      [CONNECTION_ID()] that is not the one of round trip 1, or whose low 32
      bits are not the thread id of the greeting, gives [`Proxy]; no row of the
      grantee gives [`Grantee_unknown]; a direct grant of [SUPER] or of the
      admin privilege gives [`Privileged]; an enabled role gives [`Role]. None
      of these fails the connect.

    The rules:
    - A configuration that needs TLS raises [Invalid_argument] if no default
      generator of mirage-crypto-rng is set ({!check_rng}), before the DNS
      resolve and the socket.
    - TLS (DESIGN.md, "Handshake"): the CA file of [ssl-ca] is read first (a
      regular file of at most 1 MiB; a missing file and a path through a file
      give short messages, and a file with no valid certificate gives a fixed
      message with no part of the file). Then the greeting must have
      [CLIENT_SSL], the SSL request goes with the sequence id 1, the reader must
      hold no byte, and [Weft_tls.client] checks the certificate ({!Tls_setup}).
      A failure of TLS gives [Connect "TLS: <cause>"], with the cause escaped
      with its first and its last 100 bytes ({!Wire.peer_text_ends}: x509 puts
      the cause at the end). The handshake response then has the sequence id 2.
    - A host name goes to [Weft.Dns.resolve], and the client tries the addresses
      in order. An IP address and [localhost] (127.0.0.1) go straight to the
      connect.
    - One timeout of 10 s covers the read of the CA file, the resolve, the
      connect, the greeting, the TLS handshake, the authentication and the round
      trips.
    - The server must have [CLIENT_PROTOCOL_41], [CLIENT_SECURE_CONNECTION],
      [CLIENT_PLUGIN_AUTH], [CLIENT_DEPRECATE_EOF] and [CLIENT_CONNECT_WITH_DB].
    - The authentication: [mysql_native_password], or the fast path of
      [caching_sha2_password]. The full path sends the password in clear text
      only on a Unix socket with [allow-cleartext=socket], or in a TLS session
      that is verified: [Verify_identity] with [ssl-ca], or a pin. An
      authenticator alone is not verified. Else the connection closes, no
      password byte goes, and the result is
      [Connect "the server asks for full authentication; use VERIFY_IDENTITY
       with ssl-ca, a pinned certificate (ssl-fingerprint), or
       allow-cleartext=socket"]. At most one [AuthSwitchRequest], to one of the
      two plugins, with 20 bytes and a NUL.
    - Each failure gives [Connect msg]. When a server error caused it, [msg] has
      the error number, but not the server message, which can hold a user name.
      A plugin name of the server is escaped if it is not 1 to 64 printable
      ASCII bytes.
    - The flow closes on each failure. A cancel of the fiber goes up as
      [Weft.Fiber.Cancelled]. *)

val info : t -> info

val check_rng : string -> unit
(** [check_rng name] raises [Invalid_argument] for the function [name] if no
    default generator of mirage-crypto-rng is set: the same test as [Weft_tls].
*)

(** {1 Commands}

    The outcome of each command follows DESIGN.md, "Order of the error rules"
    ({!Outcome.decide}). [query] and [exec] are writes by default (their
    [?kind]). [ping], [init_db] and [reset] are not.

    - A command on a broken or closed connection gives [Connection_lost] before
      any I/O.
    - If the flush of the command packet raises, the result is
      [Connection_lost]: the server did not get the command.
    - The end of the stream after the flush gives [Outcome_unknown `Statement]
      for a write, else [Connection_lost].
    - A deadline ({!set_deadline}) that passes in the flush gives [Timeout]: the
      server did not get the whole command. A deadline that passes in a read
      gives [Outcome_unknown `Statement] for a write, else [Timeout]. The two
      break the connection.
    - A server error gives [Error.of_server], and the connection stays usable,
      except for 1153 and for an error that ends the session.
    - A cancel of the fiber breaks the connection, and [Weft.Fiber.Cancelled]
      goes up.

    With no deadline, if the server does not answer, only a cancel of the fiber
    stops a command. *)

val set_deadline : t -> Weft.Clock.mono option -> unit
(** [set_deadline t d] sets (or with [None] removes) the read and the write
    deadline of the flow. It applies also to a command that waits at the time of
    the call. The pool sets it before a statement and removes it at the checkin.
*)

val query :
     ?kind:Outcome.command
  -> t
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
     ?kind:Outcome.command
  -> t
  -> Cistern_core.Raw.t
  -> (Cistern_core.Exec_result.t, Cistern_core.Error.t) result
(** [exec t r] runs [r] as {!query} does, ignores the rows, and gives the counts
    of the last OK packet. *)

val query_rows :
     ?kind:Outcome.command
  -> t
  -> Cistern_core.Raw.t
  -> (string option array -> ('a, string) result)
  -> ('a list, Cistern_core.Error.t) result
(** [query_rows t r decode] runs [r] as {!query} does, and gives [decode] of
    each row of the first result set, with the text of each column as the server
    sent it ([None] for SQL [NULL]). The typed statements of phase 2 use it. The
    client reads the whole reply before the first [decode]. A row that [decode]
    refuses gives [Decode msg], and the connection stays usable. [msg] must not
    hold the text of a value. *)

type kill_result =
  | Killed  (** An OK. *)
  | Gone  (** 1094: no such session. The query is already gone. *)
  | Not_owner  (** 1095: the session is of another user. *)

val kill : t -> int64 -> (kill_result, Cistern_core.Error.t) result
(** [kill t id] sends [KILL CONNECTION id], with the whole 64-bit id. It is not
    a write: the rules of the commands apply, and another server error gives
    [Error]. *)

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

val sent : t -> bool
(** [sent t] is true if the last {!query} or {!exec} flushed its command packet.
    [query] and [exec] clear it at their start, and no other call changes it.
    After a cancel in a statement, the pool reads it for the event of the
    statement. *)

val in_flight : t -> bool
(** [in_flight t] is true if the last command flushed its packet and no whole
    reply came: no server error, no ERR before the command (R-SEQ0, the server
    did not read it), and no end of its reply. Thus the server can still run the
    command, for example after a cancel, a read deadline or the end of the
    stream. It is true only on a broken connection. A command on a closed or
    broken connection gives [Connection_lost] with no I/O and keeps it, so that
    the [KILL] of an earlier command is not lost. The pool asks for a [KILL]
    only then (the plan of phase 1.5, D1 and N2). *)

val usable : t -> bool
(** [usable t] is true if [t] is not closed and not broken. The pool checks it
    in at the end of a statement as reusable only then. *)

val stale : t -> bool
(** [stale t] is the check of an idle connection at the checkout of the pool:
    true if the connection is closed or broken, if the reader holds bytes, or if
    the flow has bytes or the end of the stream ([Flow.read_now]: for example
    error 4031 at the idle timeout of MySQL), or if the read raises
    [Flow.Closed], [Flow.Timeout] or [Weft.Net.Error] (on TLS, the end with no
    [close_notify]). A TLS record with no data (a session ticket) is not stale.
    A stale connection is broken. It waits for nothing.

    It raises [Weft.Fiber.Cancelled] after the connection breaks, for a cancel
    in [read_now] (the lock of a TLS session). Each other exception also breaks
    the connection first. *)

val abort : t -> bool
(** [abort t] breaks the connection and closes the flow. It sends nothing, never
    waits and never raises, also in a cancelled fiber. It uses no busy flag: a
    command that waits in another fiber then raises [Flow.Closed]. It gives
    [true] only for the first [abort] or {!close} of [t], also when two domains
    call them at the same time. *)

val close : t -> bool
(** [close t] sends [COM_QUIT], with a write timeout of 1 s and no wait for a
    reply, then closes the flow. A broken connection sends nothing. In a
    cancelled fiber the write raises [Weft.Fiber.Cancelled] after the flow
    closes: use {!abort} there. It gives [true] only for the first {!abort} or
    [close] of [t], also across domains. A later one does nothing. *)

(** For the tests only. It can change in each version. *)
module Private : sig
  val claim : bool Atomic.t -> bool
  (** The flag of {!abort} and {!close}: [claim a] sets [a], and gives [true]
      only if [a] was not set. *)

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
