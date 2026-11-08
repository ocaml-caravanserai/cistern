(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(** The helpers of the generated code (DESIGN.md, "Generated code"; the plan of
    phase 3, D8). The code of [[@@deriving cistern]] names each of them through
    [Cistern.Private.Encode], so that it names no module of the stdlib and no
    user name can hide them. *)

(** The draft of a type with no column field for the draft (only an [[@auto]]
    key). *)
type empty = unit

val int64 : int64 -> Value.t

val int : int -> Value.t

val text : string -> Value.t

val bool : bool -> Value.t
(** [true] is 1, and [false] is 0. *)

val float : float -> Value.t

val bytes : bytes -> Value.t

val ptime : Ptime.t -> Value.t

val opt : ('a -> Value.t) -> 'a option -> Value.t
(** [opt f None] is [Null], and [opt f (Some x)] is [f x]. *)

val cut : Ptime.t -> Ptime.t
(** [cut t] is [t] cut (not rounded) to 1 µs, as the literal and the server cut
    it. *)

val cut_opt : Ptime.t option -> Ptime.t option
(** {!cut} of the value, if one. *)

val auto : 'k option -> 'k
(** [auto k] is the key from the server in [of_draft] of a model with an
    [[@auto]] key. [Model.create] always gives [Some]. [auto None] raises
    [Invalid_argument]. *)
