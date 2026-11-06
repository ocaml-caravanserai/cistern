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

val pp_summary : Format.formatter -> t -> unit
(** [pp_summary] prints the constructor, the kind of a constraint and the error
    number, never a message. For example [Constraint_violation Unique],
    [Invalid_value 1406] and [Driver 1064 42000]. It prints the SQLSTATE of
    [Driver] only if it has exactly 5 characters of [0-9A-Z]. This is the
    printer for logs: a message of the server can hold a value of a row. *)

val pp : Format.formatter -> t -> unit
(** [pp] prints the text of {!pp_summary}, then [": "] and the message for the
    constructors with a message. For example
    [Constraint_violation Unique: Duplicate entry 'x' for key 'email']. The
    message can hold a value of a row: do not send this text to a log if the
    values are confidential. *)

val to_string : t -> string
(** [to_string e] is the text of {!pp}. It contains the message of the server,
    thus it is not for logs. *)

(** The exception of the functions [..._exn]. The module registers a printer
    with [Printexc.register_printer]. It prints ["Cistern_error: "] and the text
    of {!pp_summary}, never the message of the server, because logging libraries
    print exceptions with [Printexc.to_string]. To see the full message, match
    the exception and call {!pp} or {!to_string}. *)
exception Cistern_error of t
