(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(** The DDL of [push_schema] and the comparison of [check_schema] (DESIGN.md,
    "DDL"). The two are pure: [Db.push_schema] and [Db.check_schema] of phase 2e
    run them. *)

open Cistern_core

val create_table : Dialect.t -> Schema.model list -> Schema.model -> string
(** [create_table d models m] is the [CREATE TABLE IF NOT EXISTS] of [m]:

    - One column for each field, in order:
      [`c` <type> NULL|NOT NULL [AUTO_INCREMENT]], with the type of
      {!Schema.column_type}. No [DEFAULT] clause.
    - [PRIMARY KEY], then the [UNIQUE KEY] and [KEY] of {!Schema.indexes}.
    - The foreign keys of {!Schema.foreign_keys} [models m]:
      [CONSTRAINT `n` FOREIGN KEY (`c`) REFERENCES `t` (`k`) ON DELETE RESTRICT].
    - [ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=<collation>
       ROW_FORMAT=DYNAMIC], with {!Dialect.collation}.

    The text has no value, thus it is also the SQL of the event. Call
    {!Schema.check} first: [create_table] does not check the models. *)

(** A row of [information_schema.COLUMNS] (DESIGN.md, "DDL"). *)
type column =
  { table: string  (** [TABLE_NAME] *)
  ; column: string  (** [COLUMN_NAME] *)
  ; data_type: string  (** [DATA_TYPE] *)
  ; nullable: bool  (** [IS_NULLABLE = 'YES'] *)
  ; max_length: int option  (** [CHARACTER_MAXIMUM_LENGTH] *)
  ; precision: int option  (** [DATETIME_PRECISION] *)
  ; collation: string option  (** [COLLATION_NAME] *)
  ; unsigned: bool  (** [COLUMN_TYPE LIKE '%unsigned%'] *)
  ; auto_increment: bool  (** [EXTRA LIKE '%auto_increment%'] *) }

val compare : Dialect.t -> Schema.model list -> column list -> string list
(** [compare d models rows] is one message for each difference between the
    models and the columns of the database, in the order of the models and of
    their fields, then the extra columns of each table in the order of [rows]:

    - A table of a model with no row.
    - A field with no column, and a column with no field. The column names match
      after the ASCII fold ({!Cistern_names.fold}).
    - Another [DATA_TYPE] (with no regard to ASCII case). Then the other parts
      of this column are not compared.
    - Another nullability, length, precision or collation, and an unsigned
      column.
    - An [[@auto]] key with no [AUTO_INCREMENT] (then each insert with no key
      fails with 1364), and an [AUTO_INCREMENT] column with no [[@auto]].

    The expected values are the values of the DDL of {!create_table} on the
    three servers. [compare] never compares the whole [COLUMN_TYPE], because
    MariaDB writes [bigint(20)] and MySQL writes [bigint]. A row of a table that
    is not a table of the models has no message. [[]] means that the database
    matches the models. *)
