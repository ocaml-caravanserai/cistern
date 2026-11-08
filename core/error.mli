(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(** The errors of Cistern (DESIGN.md, "Core user types"). *)

type kind = Unique | Foreign_key | Not_null | Check | Other_constraint

type t =
  | Not_found
  | Constraint_violation of {kind: kind; message: string}
  | Invalid_value of {code: int option; message: string}
  | Retryable of [`Deadlock | `Lock_timeout | `Rolled_back]
  | Statement_timeout
      (** The server stopped the statement. The connection is good. *)
  | Timeout  (** A read or an unsent statement passed its deadline. *)
  | Pool_exhausted  (** No connection before [checkout_timeout]. *)
  | Outcome_unknown of [`Statement | `Commit]
      (** A write was sent, and no reply came. *)
  | Implicit_commit
      (** In a [Tx], the server committed the transaction before its end. *)
  | Connection_lost  (** Before a write was sent, or during a read. *)
  | Read_only  (** The server or the transaction is in a read-only mode. *)
  | Connect of string  (** The last connect error of the pool. *)
  | Decode of string
  | Driver of {code: int; sqlstate: string; message: string}

val interrupts : Dialect.t -> int -> bool
(** [interrupts d code] is true for an error number with which a server of
    dialect [d] stops a session or a statement from outside: 1053
    ([ER_SERVER_SHUTDOWN]) and 1317 ([ER_QUERY_INTERRUPTED]) on the two servers,
    4031 ([ER_CLIENT_INTERACTION_TIMEOUT]) on MySQL, and 1927
    ([ER_CONNECTION_KILLED]) on MariaDB. On the other server, 4031 and 1927 are
    other errors or no error. *)

val of_server : Dialect.t -> code:int -> sqlstate:string -> message:string -> t
(** [of_server d ~code ~sqlstate ~message] is the meaning of an error packet of
    a server of dialect [d] (DESIGN.md, "Error mapping"). The key is the dialect
    and the number, because a number can have another meaning on the other
    server (for example 4031). A number that the table does not have uses the
    SQLSTATE, if the SQLSTATE has exactly 5 characters: class [23] gives
    [Constraint_violation Other_constraint], and [40001] gives
    [Retryable `Deadlock]. Else [Driver].

    [of_server] gives the meaning of the packet alone. It does not know if the
    client sent a write, if the stream ended after the packet, if the statement
    is a [COMMIT], or if a [Tx] is open. The client adds these, with the rules
    of DESIGN.md: "Order of the error rules", the replies to [COMMIT] and
    "Transactions". *)

val pp_summary : Format.formatter -> t -> unit
(** [pp_summary] prints the constructor, the kind of a constraint and the error
    number, never a message. For example [Constraint_violation Unique],
    [Invalid_value 1406] and [Driver 1064 42000]. It prints the SQLSTATE of
    [Driver] only if it has exactly 5 characters of [0-9A-Z]. This is the
    printer for logs: a message of the server can hold a value of a row. *)

val pp : Format.formatter -> t -> unit
(** [pp] prints the text of {!pp_summary}, then [": "] and the message for the
    constructors with a message that is not empty. For example
    [Constraint_violation Unique: Duplicate entry 'x' for key 'email']. The
    message can hold a value of a row: do not send this text to a log if the
    values are confidential. *)

val redact : t -> t
(** [redact e] is [e] with each message of a server error packet removed (an
    empty message): the messages of [Constraint_violation], [Invalid_value] and
    [Driver], which can hold a value of a row. The kind, the error number and
    the SQLSTATE stay. [redact] also empties a message of [Invalid_value] that
    the client made (for example of {!Literal.add} or of the size check): it
    holds no value, but one rule for each constructor is safer. The messages of
    [Connect] and [Decode] come from the client and hold no value of a row, thus
    they stay. A [Connect] text can hold the name of an authentication plugin or
    the cause of a TLS failure, escaped and cut ({!Event}). The events of
    [on_query] hold redacted errors (DESIGN.md, "Shared state and the query
    hook"). *)

val to_string : t -> string
(** [to_string e] is the text of {!pp}. It contains the message of the server,
    thus it is not for logs. *)

(** The exception of the functions [..._exn]. The module registers a printer
    with [Printexc.register_printer]. It prints ["Cistern_error: "] and the text
    of {!pp_summary}, never the message of the server, because logging libraries
    print exceptions with [Printexc.to_string]. To see the full message, match
    the exception and call {!pp} or {!to_string}. *)
exception Cistern_error of t
