(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(** The parser of a group of [[@@deriving cistern]] and its checks (DESIGN.md,
    "Model definition"; the plan of phase 3, D3 to D6, D10, D12). Each check
    raises a located error ([Location.Error]) at the node of its cause: the
    compiler shows one error at a time. *)

open Ppxlib

(** A field of a record type, with its attributes. *)
type field =
  { name: string  (** The OCaml name of the field. *)
  ; loc: Location.t  (** The location of the name of the field. *)
  ; ty: core_type
        (** The type of the field as the source has it. The generated code
            copies it (D8). *)
  ; scalar: Cistern_schema.Schema.scalar
  ; nullable: bool  (** The type is an [option]. *)
  ; key: bool
  ; auto: bool
  ; unique: bool
  ; index: bool
  ; column: (string * Location.t) option
        (** [[@column "x"]], with the location of the attribute. *)
  ; default: expression option  (** [[@default e]] *)
  ; number: int
        (** The index of the field in the group, from 0, over all the fields of
            all the types: the number of its default thunk (D10). *) }

(** The kind of a relation field (phase 4f, the plan of phase 4, E12). *)
type kind =
  | Has_many of {target: string}  (** [<target> list Deferred.t [@has_many]] *)
  | Belongs_to of
      { target: string
      ; key: string * Location.t
            (** The field of the key, with the location of its name in the
                payload. *)
      ; references: string * Location.t
      ; nullable: bool  (** [<target> option Deferred.t]: a nullable key. *) }
      (** [<target> Deferred.t [@belongs_to {key; references}]] *)

(** A relation field. It is not a column (E3). *)
type relation =
  { name: string
  ; loc: Location.t  (** The location of the name of the field. *)
  ; ty: core_type  (** The type of the field as the source has it. *)
  ; attr: Location.t  (** The location of the relation attribute. *)
  ; kind: kind }

(** A type of the group. *)
type typ =
  { name: string  (** The OCaml name of the type. *)
  ; loc: Location.t  (** The location of the name of the type. *)
  ; table: (string * Location.t) option
        (** [[@@table "x"]], with the location of the attribute. *)
  ; fields: field list  (** The column fields, in order. *)
  ; relations: relation list  (** The relation fields, in order. *) }

val group : loc:Location.t -> type_declaration list -> typ list
(** [group ~loc tds] is the description of the group [tds] at [loc], after the
    checks of the plan of phase 3, section 3.2, in this order:
    + The shape: each type is a record type with no parameter, not private and
      with no manifest.
    + The attributes of each type: only [[@@table "x"]]. This check comes before
      the type names, because a type name with an uppercase letter needs
      [[@@table]].
    + Each type name: it starts with an ASCII lowercase letter, has only ASCII
      characters, has no uppercase letter (or has [[@@table]]), has no [__],
      does not start with [cistern], and is not [stdlib], [ptime], [unit] or a
      scalar name.
    + Each attribute of the fields and of the types of the fields: a near miss
      or a spelling of another ORM, an unknown [[@cistern.x]], the context, an
      attribute after the [";"] of a field on the line of the next field, a
      duplicate, a relation, the payload.
    + Each field: its name (not a name of [Expr.OPS], no [cistern__]), not
      [mutable], a type of version 1, and no [[@default]] on a key. A field with
      [[@has_many]] or [[@belongs_to]] is a relation field (phase 4f, E12): its
      name is not a value of the model module ([all], [where], [update],
      [delete], [draft], [create], [model], [get_by_*], [find_by_*],
      [filter_by_*]), it has no other Cistern attribute, and its type is
      [<t> list Deferred.t] or [<t> Deferred.t] (or [<t> option Deferred.t] for
      a nullable key) with [<t>] a type of the group. A field with a type of the
      group or a [Deferred.t] and no relation attribute is an error.
    + The key of each belongs_to is nullable exactly for
      [<t> option Deferred.t].
    + {!Cistern_schema.Schema.faults} of {!schema}: see {!place}. *)

val schema : typ list -> Cistern_schema.Schema.model list
(** [schema g] is the model of each type of [g], with its relations (phase 4f).
    The ppx checks this value, and the generator of phase 3b lifts it into the
    generated code (D2). *)

val place : loc:Location.t -> typ list -> Cistern_schema.Schema.fault -> 'a
(** [place ~loc g f] raises the located error of the fault [f] of the models of
    [g] (D2). A fault of one field goes to the field, and a fault of two or more
    fields to the second of them, the first that makes the conflict. A fault of
    a field goes to its [[@column]] attribute if the column name is the cause. A
    fault of a table name goes to [[@@table]], else to the type. Another fault
    of a model goes to the type. A fault of the list goes to the second type of
    its models, or to [loc] if the group has no such type. A name that is too
    long and comes from the OCaml name gets the hint [[@column "..."]] or
    [[@@table "..."]]. A fault of a relation (rules 7 to 9, E15) goes to the
    relation field, or for a belongs_to to its attribute. A fault of rule 11
    with one type two times in its models (two foreign key names of one table
    that are equal: the hash of a long name can collide) goes to the attribute
    of the second belongs_to of the two. Exported for the tests. *)
