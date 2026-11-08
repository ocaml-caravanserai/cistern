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

val some : 'a -> 'a option
(** [some x] is [Some x]: the value of [get_by] of a nullable [[@unique]] field.
*)

val set : ('m, 'a) Field.t -> 'a option -> 'm Update.t option
(** [set f v] is [Update.set f x] for [v = Some x], and [None] for [None]: a
    label of [update]. *)

val updates : 'm Update.t option list -> 'm Update.t list
(** [updates l] is the list of the values of [l], in order. *)

val reset : ('k -> Value.t) -> 'k -> 'k -> 'r Deferred.t -> 'r Deferred.t
(** [reset encode old k rel] is the relation [rel] of a belongs_to after its key
    changes from [old] to [k] (phase 4c1, the plan of phase 4, E9): [Unloaded]
    if the encoded keys are not equal ({!Value.equal}), else [rel]. The setter
    of the key field calls it, thus [Field.set] and [update] of the key do the
    same. *)

val group : ('a -> 'b) -> 'a option -> 'b option
(** [group f l] is [f] of the list of a [?<has_many>] label of [create], if the
    caller gave one (phase 4g). *)

val parent : ('c -> 'p -> 'c) -> ('c -> 'c) -> 'c -> 'p option -> 'c
(** [parent set unload c p] is the setter of a belongs_to with a key that is not
    nullable, for [Relation.belongs_to] (phase 4g; the notes of the review of
    4a): [set c x] for [Some x], and [unload c] (the relation [Unloaded]) for
    [None]. *)

val somes : 'a option list -> 'a list
(** [somes l] is the values of [l], in order: the groups of children of [create]
    for the [?<has_many>] labels that the caller gave (phase 4g). *)

val draft_key : 'k option -> 'k
(** [draft_key k] is the key of a belongs_to in [of_draft] of a child model
    whose key is not nullable (phase 4g, E13): the draft holds it as an option.
    [Model.create] calls [of_draft] after the [INSERT], and an [INSERT] with no
    key gives [Not_null] first. For a parent with children and a key that is not
    [[@auto]], [Model.create] calls [of_draft] before, to take the key of the
    parent from the draft: then a draft with no key gives [Invalid_value] before
    the checkout. [draft_key None] raises [Invalid_argument]. *)

val auto : 'k option -> 'k
(** [auto k] is the key from the server in [of_draft] of a model with an
    [[@auto]] key. [Model.create] always gives [Some]. [auto None] raises
    [Invalid_argument]. *)
