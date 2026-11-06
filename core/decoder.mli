(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(** Decoders of the columns of the text protocol. The server sends each column
    as text, or as SQL [NULL]. The generated decoders of phase 3 read the
    columns by position, in the order of the fields.

    Each decoder checks a grammar before it converts the text, because
    [float_of_string] and [Int64.of_string_opt] also accept text that a server
    never sends, for example ["1_0"], ["0x10"] or ["nan"]. *)

(** The decoder of one column. *)
type 'a t

val int64 : int64 t
(** [-?[0-9]+], in the range of [int64]. *)

val int : int t
(** As {!int64}, in the range of [int]. *)

val float : float t
(** [-?[0-9]+(\.[0-9]+)?([eE][-+]?[0-9]+)?], with a finite result. *)

val bool : bool t
(** An integer, as {!int64}: [0] is [false], another value is [true]. *)

val string : string t
(** Valid UTF-8 ([String.is_valid_utf_8]). It refuses an encoded surrogate, for
    example [ED A0 80]. Phase 1.1 examines if a server can store one in a
    [utf8mb4] column. *)

val bytes : bytes t
(** Any bytes. *)

val ptime : Ptime.t t
(** [YYYY-MM-DD HH:MM:SS], with the seconds from 00 to 59, and with 0 to 6
    digits after a point, in UTC. *)

val decimal : Decimal.t t
(** The grammar of {!Decimal.of_string}. *)

val option : 'a t -> 'a option t
(** [option d] gives [None] for SQL [NULL]. *)

type cursor

val next : cursor -> 'a t -> 'a
(** [next c d] decodes the next column of the row. Call it only in the function
    that {!run} calls: after [run] returns, it raises [Invalid_argument]. *)

val run : (cursor -> 'a) -> string option array -> ('a, string) result
(** [run f row] calls [f] with a cursor at the first column of [row]. It is
    [Error msg] if a column does not decode, if [f] reads more columns than
    [row] has, or if [f] reads fewer columns. [msg] names the column (from 0)
    and the expected type, and never the text of the column. Another exception
    of [f] goes up with its backtrace.

    {!next} reports an error with an exception that [run] catches. Thus [f] must
    not catch all exceptions (for example with [with _ ->]): it would catch this
    exception, and [run] would not see the error. *)
