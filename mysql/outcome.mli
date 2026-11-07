(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(** The outcome of a command that did not give its value (DESIGN.md, "Order of
    the error rules"): the error, and if the connection stays usable. It is a
    pure function: [Connection] finds the event and acts on the result. *)

(** A raw statement ([query], [exec]) is a write. [ping], [init_db] and the
    steps of [reset] are not. *)
type command = Write | Not_write

type event =
  | Write_failed
      (** The flush of the command packet raised. The server did not get a
          complete command, thus it cannot run it. *)
  | Write_deadline
      (** The flush of the command packet passed its deadline. The writer raises
          then only while it waits to write the rest of the packet, thus the
          server did not get the whole command. A server runs a command only
          when it has the whole packet. *)
  | Read_deadline
      (** A read of the reply passed its deadline, after the flush. *)
  | Before_command
      (** Rule R-SEQ0: an ERR with the sequence id 0 where the first packet of
          the reply (id 1) was expected. The server sent it before it read the
          command, for example 4031 at the idle timeout. *)
  | Stream_ended
      (** The end of the stream, a reset or another failure of the transport
          after the flush, before the end of the reply. *)
  | Bad_packet of string
      (** A malformed packet or a wrong sequence id, with what was wrong. Also
          after the column count: the statement then ran. [decide] does not use
          the text. It is for the event of phase 1.2. *)
  | Local_infile  (** A [LOCAL INFILE] request ([0xFB]). *)
  | Server_error of Message.err  (** An ERR with the expected sequence id. *)
  | Bad_value of string
      (** A value that does not decode. The client read the whole reply. *)

type state = Usable | Broken

val decide :
  Cistern_core.Dialect.t -> command -> event -> Cistern_core.Error.t * state
(** [decide d c e]:

    - [Write_failed] and [Before_command] give [Connection_lost], for each
      command: the statement did not run.
    - [Write_deadline] gives [Timeout], for each command: the statement did not
      go.
    - [Read_deadline] gives [Outcome_unknown `Statement] for a write, else
      [Timeout].
    - [Stream_ended] and [Bad_packet] give [Outcome_unknown `Statement] for a
      write, else [Connection_lost]. A [Bad_packet] after the column count is
      also [Outcome_unknown]: the statement ran, but a [CALL] can run more
      statements after its first result set, and the client cannot read their
      outcome.
    - [Local_infile] gives [Decode "LOCAL INFILE request"]. The server waits for
      a file, thus it did not run the statement.
    - A [Server_error] gives [Error.of_server]. If that is [Connection_lost]
      (1053; 4031 on MySQL; 1927 on MariaDB), a write gives
      [Outcome_unknown `Statement]. 1153 gives [Invalid_value], and the server
      closes the connection after it.
    - [Bad_value msg] gives [Decode msg].

    Each event breaks the connection, except a [Server_error] that is not
    [Connection_lost] and not 1153 (for example 1317), and [Bad_value]. *)
