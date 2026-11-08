(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(** The events of [?on_query] (DESIGN.md, "Shared state and the query hook"). An
    event holds no message of a server error packet and no value of a statement:
    - The error of a [Failed] outcome and of a [Connect] event is redacted
      ({!Error.redact}): it has no message of an error packet, which can hold a
      value of a row. The kind, the error number and the SQLSTATE stay.
    - The SQL has [?] in place of each value of {!Raw.v} and of each literal of
      a typed statement. A literal that the program writes into the text of
      {!Raw.sql} stays in the event: [Raw.sql] takes only the text of the
      program ({!Raw.redacted}).
    - The text of a [Connect] error comes from the client. Two such texts can
      hold a short text that the server (or a peer in the middle) controls,
      escaped and cut, which is never a value of a row: the name of an
      authentication plugin ("unsupported authentication plugin <name>"), and
      the cause of a TLS failure ("TLS: <cause>"), which can name the subject of
      the certificate.

    The result of the statement keeps the full error. {!pp} prints no message
    either. The events of [KILL] (phase 1.5) hold no free text: their reasons
    are variants. *)

type outcome =
  | Done of {rows: int}
      (** The rows of a result set, or the affected rows of a write. *)
  | Failed of Error.t
  | Cancelled of {sent: bool}
      (** The fiber was cancelled. [sent]: the client sent the statement, thus
          the outcome of a write is not known. *)

type kill_outcome =
  [ `Killed  (** [KILL] gave an OK. *)
  | `Gone  (** 1094: the session was already gone. *) ]

type kill_dropped =
  [ `Queue_full  (** The kill queue was full. *)
  | `Duplicate  (** The queue already held the same session. *)
  | `Other_server
    (** The control connection is on another server, or the id is its own. *)
  | `Failed  (** Two tries failed. *)
  | `Not_owner  (** 1095: the session is of another user. *)
  | `Off  (** Kill went off for the pool. *)
  | `Drain_timeout  (** The drain at the end of [with_db] passed its limit. *)
  | `Closed  (** The pool closed and its drain ended. *) ]

(** The reasons why [KILL] goes off for a pool (DESIGN.md, "Session"). *)
type kill_off =
  [ `Identity_failed
    (** The identity [SELECT] of round trip 3 failed, for example with
        [performance_schema = OFF] on MySQL. *)
  | `Proxy
    (** Its [CONNECTION_ID()] is not the one of round trip 1, or its low 32 bits
        are not the thread id of the greeting. *)
  | `Privileged  (** The user has a direct grant of [SUPER] or the admin one. *)
  | `Role  (** The session has an enabled role. *)
  | `Grantee_unknown  (** The grantee of the user has no row. *)
  | `Not_owner  (** A [KILL] gave 1095. *) ]

type t =
  | Query of {sql: string; duration: Weft.Span.t; outcome: outcome}
      (** [sql] is the redacted SQL. The producer of the event (the client) puts
          each value of [Raw.v] and each typed literal as [?]. The text of
          [Raw.sql] stays as the program wrote it. *)
  | Connect of {address: string; duration: Weft.Span.t; error: Error.t option}
      (** [address] is [host:port] or the path of the socket, never the
          password. *)
  | Kill of {connection_id: int64; duration: Weft.Span.t; outcome: kill_outcome}
      (** The killer sent [KILL CONNECTION connection_id]. *)
  | Kill_dropped of {connection_id: int64; reason: kill_dropped}
      (** No [KILL] went for the session, and its slot is free. *)
  | Kill_off of {reason: kill_off}
      (** [KILL] went off for the pool. A pool gives at most one. *)

val pp : Format.formatter -> t -> unit
(** [pp] prints the kind of the event, the duration, the number of rows, the
    address, the redacted SQL, and for an error only {!Error.pp_summary}. For
    example [query 1500ns rows 3 "SELECT ... WHERE id = ?"],
    [connect 10000000ns db:3306 failed Connect], [kill 2000000ns 42 gone],
    [kill dropped 42 queue_full] and [kill off proxy]. It never prints a message
    of a server. *)
