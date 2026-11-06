(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(** The values of the text protocol: the parameters of [Raw.v], and the columns
    of a raw result (DESIGN.md, "Core user types"). *)

type t =
  | Null
  | Int of int64
  | Float of float
  | Text of string  (** Valid UTF-8. {!Literal.add} checks it. *)
  | Bytes of string
  | Datetime of Ptime.t  (** UTC. The literal truncates it to 1 µs. *)
  | Decimal of Decimal.t

val equal : t -> t -> bool
(** [equal a b] is true if [a] and [b] have the same constructor and the same
    value. [Float] compares the bits: [-0.0] and [0.0] are not equal, and a NaN
    equals the same NaN. [Datetime] uses [Ptime.equal], and [Decimal] uses
    {!Decimal.equal}. *)
