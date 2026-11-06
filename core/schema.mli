(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(** The description of the models (DESIGN.md, "Schema, AST and SQL"). The ppx of
    phase 3 makes one [model] for each type of a group. Phase 2 uses
    hand-written models. [push_schema] and [check_schema] call [check] first.

    The record types follow the attributes of DESIGN.md ("Model definition") one
    for one. *)

(** The OCaml type of a column, without [option] (DESIGN.md, "Field types"). *)
type scalar = Int64 | Int | String | Bool | Float | Bytes | Ptime

type field =
  { name: string  (** The OCaml field. *)
  ; column: string  (** The name in the database: [[@column]], or [name]. *)
  ; scalar: scalar
  ; nullable: bool  (** An [option] type. *)
  ; key: bool  (** [[@key]] *)
  ; auto: bool  (** [[@auto]] *)
  ; unique: bool  (** [[@unique]] *)
  ; index: bool  (** [[@index]] *) }

type relation =
  | Has_many of {name: string; target: string}
  | Belongs_to of {name: string; target: string; key: string; references: string}
      (** [target] is the [name] of a model. [key] is the [name] of a field of
          this model, and [references] is the [name] of a field of the target.
      *)

type model =
  { name: string  (** The OCaml type. *)
  ; table: string  (** [[@@table]], or [name]. *)
  ; fields: field list  (** The column fields, in the order of the record. *)
  ; relations: relation list }

type column_type =
  | Bigint
  | Varchar_255
  | Mediumtext
  | Tinyint_1
  | Double
  | Varbinary_255
  | Mediumblob
  | Datetime_6

val column_type : model -> field -> column_type
(** The type of the column (DESIGN.md, "Field types"). A [String] or a [Bytes]
    field with an index is [VARCHAR(255)] or [VARBINARY(255)], else [MEDIUMTEXT]
    or [MEDIUMBLOB]. A field has an index if it is the key, if it has [unique]
    or [index], or if it is the key of a [Belongs_to]. *)

type index_kind = Primary | Unique | Plain

type index = {name: string; column: string; kind: index_kind}

val indexes : model -> index list
(** The indexes of the table. First [PRIMARY] on the key. Then, for each other
    field in order:
    - A [Unique] index [<table>_<column>_key] if the field has [unique].
    - Else a [Plain] index [<table>_<column>_idx] if the field has [index] or is
      the key of a [Belongs_to]. There is only one index if the two apply.

    The key and a [unique] field get no plain index, because their index serves
    the foreign key. The names come from {!Cistern_names.index_name}. *)

type foreign_key =
  {name: string; column: string; target_table: string; target_column: string}

val foreign_keys : model list -> model -> foreign_key list
(** [foreign_keys models model] is one foreign key for each [Belongs_to] of
    [model], in order, with the name [<table>_<column>_fk]
    ({!Cistern_names.index_name}). A [Belongs_to] with a [key], a [target] or a
    [references] that does not exist gives no foreign key: [check] refuses it.
    [ON DELETE RESTRICT] is a rule of the DDL, not of this record. *)
