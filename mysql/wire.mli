(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(** The basic values of the MySQL protocol: little-endian integers,
    length-encoded integers and strings, and strings that end with a NUL
    (DESIGN.md, "Commands and packets").

    A {!cursor} reads the payload of one packet from its start. A read past the
    end raises {!Truncated}, and a value that the protocol does not permit
    raises {!Malformed}. The decoders of {!Message} catch the two and give
    [Error]. *)

(** A read past the end of the payload. *)
exception Truncated

(** A value that the protocol does not permit, for example a length-encoded
    integer that starts with [0xFF]. *)
exception Malformed of string

val peer_text : limit:int -> string -> string
(** [peer_text ~limit s] is a text of the peer for a message (DESIGN.md,
    "Authentication"): [s] as it is if it has 1 to [limit] printable ASCII
    bytes, and no double quote and no backslash. Else the first [limit] bytes of
    [s] between double quotes, with [\xNN] for each other byte, a double quote
    and a backslash, and ["..."] if [s] is longer. Thus a hostile peer cannot
    put control bytes or a long text into a log. The plugin name uses a limit of
    64, and the text of a TLS failure a limit of 200 (phase 1.4). *)

(** A payload and a position in it. *)
type cursor

val cursor : string -> cursor
(** [cursor payload] is a cursor at the start of [payload]. *)

val remaining : cursor -> int
(** The number of bytes after the position. *)

val uint8 : cursor -> int

val uint16 : cursor -> int

val uint24 : cursor -> int

val uint32 : cursor -> int

val int64 : cursor -> int64
(** [int64 c] reads 8 bytes. A value above [Int64.max_int] gives a negative
    number with the same 64 bits. *)

val bytes : cursor -> int -> string
(** [bytes c n] reads [n] bytes. *)

val skip : cursor -> int -> unit

val rest : cursor -> string
(** [rest c] reads all the bytes after the position. *)

val string_nul : cursor -> string
(** [string_nul c] reads the bytes up to the next NUL, and the NUL. No NUL
    raises {!Truncated}. *)

type lenenc = Value of int64 | Null

val lenenc : cursor -> lenenc
(** [lenenc c] reads a length-encoded integer: one byte below [0xFB], or [0xFC]
    and 2 bytes, [0xFD] and 3 bytes, [0xFE] and 8 bytes. [0xFB] is [Null] (a
    NULL value in a row). [0xFF] raises {!Malformed}. *)

val lenenc_string : cursor -> string option
(** [lenenc_string c] reads a length-encoded integer, then that number of bytes.
    [Null] gives [None]. *)

val write_lenenc : Weft_flow.Writer.t -> int64 -> unit
(** [write_lenenc w n] writes [n] in the shortest form. A negative [n] is the
    unsigned value with the same 64 bits, thus 9 bytes. *)

val write_lenenc_string : Weft_flow.Writer.t -> string -> unit

val write_string_nul : Weft_flow.Writer.t -> string -> unit
(** [write_string_nul w s] writes [s] and a NUL.

    @raise Invalid_argument if [s] has a NUL. *)
