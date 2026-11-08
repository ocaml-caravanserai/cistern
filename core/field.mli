(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(** The typed fields of a model (DESIGN.md, "Core user types"). A field joins a
    column of a {!Schema.model} with the OCaml record type ['m] and the OCaml
    type ['a] of its value. The type is abstract, thus a field of one model
    cannot type as a field of another model. *)

type ('m, 'a) t

val make :
     model:Schema.model
  -> field:Schema.field
  -> encode:('a -> Value.t)
  -> decode:'a Decoder.t
  -> get:('m -> 'a)
  -> set:('m -> 'a -> 'm)
  -> ('m, 'a) t
(** [make ~model ~field ~encode ~decode ~get ~set] is the field of the column
    [field] in the table of [model]. The column, the scalar, [nullable] and the
    key flag come from [field], so that one source holds them. [encode] gives
    [Null] only for [None] of a nullable field. The hand-written models of phase
    2 and the ppx of phase 3 use this function, through [Cistern.Private].

    @raise Invalid_argument if [field] is not in [model.fields]. *)

val table : (_, _) t -> string
(** The table of the model. *)

val column : (_, _) t -> string
(** The name of the column in the database. *)

val scalar : (_, _) t -> Schema.scalar

val nullable : (_, _) t -> bool
(** The type ['a] is an [option]. *)

val is_key : (_, _) t -> bool
(** The field is the key of its model. *)

val encode : (_, 'a) t -> 'a -> Value.t

val decode : (_, 'a) t -> 'a Decoder.t

val get : ('m, 'a) t -> 'm -> 'a
(** [get f r] is the value of [f] in the record [r]. *)

val set : ('m, 'a) t -> 'm -> 'a -> 'm
(** [set f r v] is [r] with the value [v] for [f]. *)

(** A field with its value type hidden: the list of the fields of a model. *)
type 'm any = Any : ('m, 'a) t -> 'm any
