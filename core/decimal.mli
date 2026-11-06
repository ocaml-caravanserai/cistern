(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(** Decimal numbers as text, for raw results such as [SUM] (DESIGN.md, "Core
    user types"). A [t] holds only text that {!of_string} checked. Thus
    [Raw.v (Decimal d)] cannot carry SQL text. *)

type t

val of_string : string -> t option
(** [of_string s] is [Some d] if and only if [s] matches [-?[0-9]+(\.[0-9]+)?],
    with at most 65 digits in total and at most 38 digits after the point. These
    are the limits of the [DECIMAL] values of the servers: 65 digits, and a
    scale of at most 38 (MariaDB) or 30 (MySQL). {!Literal.add} refuses more
    than 30 digits after the point. *)

val to_string : t -> string
(** [to_string d] is the text that {!of_string} checked, with no change. *)

val equal : t -> t -> bool
(** [equal a b] compares the text, not the number. Thus [1.0] and [1] are not
    equal. *)

val pp : Format.formatter -> t -> unit
(** [pp] prints the text. *)
