(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(** The reply to one command: an OK, an ERR, a [LOCAL INFILE] request, or one
    text result set with its raw rows (the plan of phase 1.1, section 3.7).

    A result set is the column count, the column definitions, the rows, and the
    terminator: an OK packet that starts with [0xFE]. A row cannot start with
    [0xFE], because its first value would need a packet of 16 MiB. The map of
    the values to [Value.t] and the loop over more result sets
    ([SERVER_MORE_RESULTS_EXISTS]) are in the connection.

    An ERR among the column definitions is a malformed column ([Malformed]), not
    an [Err_packet]: no known server sends it there. The reader keeps all the
    rows of a result in memory, with no limit of the number of rows. The
    deadline of phase 1.2 bounds the time of a read. *)

type t =
  { columns: Message.column array
  ; rows: string option array list  (** In the order of the server. *)
  ; ok: Message.ok  (** The terminator. *) }

type reply =
  | Ok_packet of Message.ok
  | Err_packet of Message.err
      (** An ERR in place of the reply, or after the columns or some rows. The
          rows that came before it are dropped. *)
  | Local_infile of string
      (** A [LOCAL INFILE] request ([0xFB]) and its file name. The client did
          not set [CLIENT_LOCAL_FILES], thus a server does not send it. *)
  | Rows of t

type cause = Packet of Packet.error | Malformed of string

type error =
  { in_result: bool
        (** True if the error came after the column count: the server then ran
            the statement. *)
  ; cause: cause }

val read : Weft_flow.Reader.t -> seq:int -> (reply * int, error) result
(** [read r ~seq] reads one reply. Its first packet has the sequence id [seq].
    It gives the reply and the sequence id of the packet after it. It raises the
    exceptions of [Weft_flow.Reader.ensure]. After an [error], the connection is
    broken. *)
