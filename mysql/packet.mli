(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(** The packets of the MySQL protocol: a length of 3 bytes, a sequence id of 1
    byte, and the payload (DESIGN.md, "Commands and packets").

    The sequence ids: each command starts at 0 (the client), and each packet
    adds 1, modulo 256. The authentication continues from the greeting (0) to
    the final OK, also across an [AuthSwitchRequest] and [AuthMoreData].

    Packets of 16 MiB and more are not supported: a payload has at most
    [0xFFFFFE] bytes. *)

val reader : Weft_flow.Flow.t -> Weft_flow.Reader.t
(** A reader of the flow. *)

val writer : Weft_flow.Flow.t -> Weft_flow.Writer.t
(** A writer of the flow, with a maximum frame of [0xFFFFFE] bytes and the
    header. *)

val next : int -> int
(** [next seq] is [(seq + 1) mod 256]. *)

type error =
  | Sequence of {expected: int; got: int}
      (** A packet with another sequence id. The connection is then broken. *)
  | Unsolicited of string
      (** An ERR packet (its payload) with the sequence id 0, where the expected
          id is 1: the first packet of the reply to a command. The server sent
          it before it read the command, for example error 4031 at the idle
          timeout (rule R-SEQ0 of the plan of phase 1.1). Later in a reply
          (after the column count, or during the authentication), an ERR with
          the id 0 is a [Sequence] error. *)
  | Too_long
      (** A packet with the length [0xFFFFFF]: a payload of 16 MiB or more,
          which version 1 does not support. *)

val read : Weft_flow.Reader.t -> seq:int -> (string, error) result
(** [read r ~seq] reads one packet, and gives its payload if its sequence id is
    [seq]. It consumes the packet also on an error, except [Too_long] (the
    connection is then broken). It raises the exceptions of
    {!Weft_flow.Reader.ensure}, for example [End_of_file] at the end of the
    stream. *)

val write :
  Weft_flow.Writer.t -> seq:int -> (Weft_flow.Writer.t -> unit) -> unit
(** [write w ~seq body] writes one packet, with the payload that [body] writes.
    It does not flush. If [body] raises, the packet is dropped and the exception
    goes up.

    @raise Weft_flow.Writer.Too_large
      if the payload has more than [0xFFFFFE] bytes. *)

val query_fits : max_allowed_packet:int -> sql_length:int -> bool
(** [query_fits ~max_allowed_packet ~sql_length] is true if a [COM_QUERY] with
    an SQL text of [sql_length] bytes can go: its payload ([1 + sql_length]) is
    less than the smaller of [0xFFFFFF] and [max_allowed_packet]. The server
    refuses a payload equal to [max_allowed_packet]. *)
