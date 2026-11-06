(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(** The events of [?on_query] (DESIGN.md, "Shared state and the query hook"). An
    event holds no value of a statement, and {!pp} prints no message of a
    server. The events of [KILL] come in phase 1.5. *)

type outcome =
  | Done of {rows: int}
      (** The rows of a result set, or the affected rows of a write. *)
  | Failed of Error.t
  | Cancelled of {sent: bool}
      (** The fiber was cancelled. [sent]: the client sent the statement, thus
          the outcome of a write is not known. *)

type t =
  | Query of {sql: string; duration: Weft.Span.t; outcome: outcome}
      (** [sql] is the redacted SQL. The producer of the event (the client) puts
          each literal as [?], and puts no other text from a user in [sql]. *)
  | Connect of {address: string; duration: Weft.Span.t; error: Error.t option}
      (** [address] is [host:port] or the path of the socket, never the
          password. *)

val pp : Format.formatter -> t -> unit
(** [pp] prints the kind of the event, the duration, the number of rows, the
    address, the redacted SQL, and for an error only {!Error.pp_summary}. For
    example [query 1500ns rows 3 "SELECT ... WHERE id = ?"] and
    [connect 10000000ns db:3306 failed Connect]. It never prints a message of a
    server. *)
