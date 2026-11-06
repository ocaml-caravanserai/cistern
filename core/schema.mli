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

val check : model list -> (unit, string list) result
(** The checks of [push_schema] and [check_schema] (DESIGN.md, "Schema, AST and
    SQL"). [Error] has one message for each fault. The rules:
    + The model names are unique. The table names are unique after the ASCII
      fold.
    + Each table name passes {!Cistern_names.valid_table_name}, and each column
      name passes {!Cistern_names.valid_identifier}.
    + Each model has exactly one [key] field. The key is not [nullable] and not
      [Float].
    + [auto] is only on the key, and only on an [Int64] field.
    + The column names of a model are unique after the ASCII fold.
    + The key has no [unique] and no [index]. A field does not have [unique] and
      [index] together.
    + The relation names of a model are unique, and differ from the field names.
    + [Belongs_to]: [key] is a field of the model, [target] is a model of the
      list, [references] is the key of the target, and the two fields have the
      same [scalar]. Two [Belongs_to] of a model do not use one key. A
      [Belongs_to] to its own model does not use the key of the model as its
      [key]: MySQL refuses [FOREIGN KEY (id) REFERENCES t (id)] (error 6125),
      and on MariaDB each [INSERT] fails (error 1452).
    + [Has_many]: [target] is a model of the list, with exactly one [Belongs_to]
      to this model (version 1 has no [via]).
    + The names of the unique and plain indexes of a table are unique after the
      ASCII fold. Rule 3 checks the primary key.
    + The foreign key names of all the models are unique after the ASCII fold,
      because a constraint name is unique in the database.

    The messages come in this order: rule 1, then rules 2 to 10 for each model
    in order, then rule 11. In rules 8 and 9, the message of two [Belongs_to] on
    one key comes first, then the messages of each relation in order. A group of
    equal names gives one message that names all the members, and a name used by
    more than one relation or field gives one message. The names in the messages
    come from {!Cistern_names.quote}. A rule does not run if it needs a part
    that another rule refuses. For example, rule 8 does not compare the type of
    a key that does not exist, rule 10 does not run for a model that breaks rule
    2 or 5, and rule 11 does not run if rule 1 found equal tables. Two
    [Belongs_to] on one key give only the message of rule 8.

    The fold is ASCII only ({!Cistern_names.fold}). Thus two names that differ
    only in a non-ASCII letter, for example [é] and [É], pass, and the server
    refuses the DDL with error 1060 (a duplicate column). An index name with a
    non-ASCII character always has the hash, thus it does not give error 1061.
    The fold of the servers is not the same everywhere: MariaDB 11.8 accepts the
    Kelvin sign (U+212A) and [k] as two columns. [check] cannot see the foreign
    keys of tables outside the list. *)
