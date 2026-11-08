(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(** A relation of a model (DESIGN.md, "Generated code", [Rel]; the plan of phase
    4, E2): the relation from the model ['m] to the record type ['r] of the
    other model, for [Query.include_]. The generated [Rel] of each model makes
    one with {!has_many} or {!belongs_to}, through [Cistern.Private]. *)

(** The GADT is visible inside the library ([Query.include_] of phase 4d reads
    the two models, the key and the setter). The public [Cistern.Relation] gives
    only the type, with no constructor. ['k] is the key of the parent. *)
type ('m, 'r) t =
  | Has_many :
      { name: string
      ; parent:
          (module Cistern_core.Model_intf.MODEL
             with type t = 'm
              and type key = 'k )
      ; child: (module Cistern_core.Model_intf.MODEL with type t = 'r)
      ; key: 'r -> 'k option  (** The key of the parent of a child. *)
      ; set: 'm -> 'r list -> 'm  (** The parent with its children. *) }
      -> ('m, 'r) t
  | Belongs_to :
      { name: string
      ; child: (module Cistern_core.Model_intf.MODEL with type t = 'm)
      ; parent:
          (module Cistern_core.Model_intf.MODEL
             with type t = 'r
              and type key = 'k )
      ; key: 'm -> 'k option  (** The key of the parent of the child. *)
      ; set: 'm -> 'r option -> 'm  (** The child with its parent. *) }
      -> ('m, 'r) t

val name : (_, _) t -> string
(** The name of the relation: the name of its field. *)

val kind : (_, _) t -> [`Has_many | `Belongs_to]

val has_many :
     name:string
  -> (module P : Cistern_core.Model_intf.MODEL)
     -> (module C : Cistern_core.Model_intf.MODEL)
        ->    key:(C.t -> P.key option)
           -> set:(P.t -> C.t list -> P.t)
           -> (P.t, C.t) t
(** [has_many ~name (module P) (module C) ~key ~set] is the [Has_many] [name] of
    [P] to [C]. [key c] is the key of the parent of the child [c] ([None] for a
    nullable key with no parent), and [set p cs] gives [p] with its children
    [cs] loaded.

    @raise Invalid_argument
      if the schema of [P] has no relation [name], or has it as a [Belongs_to],
      or to another model than the model of [C]. The message names the relation
      that the schema has. *)

val belongs_to :
     name:string
  -> (module C : Cistern_core.Model_intf.MODEL)
     -> (module P : Cistern_core.Model_intf.MODEL)
        ->    key:(C.t -> P.key option)
           -> set:(C.t -> P.t option -> C.t)
           -> (C.t, P.t) t
(** [belongs_to ~name (module C) (module P) ~key ~set] is the [Belongs_to]
    [name] of [C] to [P]. [key c] is the key of the parent of [c], and [set c p]
    gives [c] with its parent [p] loaded ([None] for a nullable key with no
    parent).

    @raise Invalid_argument
      if the schema of [C] has no relation [name], or has it as a [Has_many], or
      to another model than the model of [P]. *)
