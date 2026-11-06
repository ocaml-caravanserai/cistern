(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(** The rules of the servers for names of tables, columns, indexes and
    constraints (DESIGN.md, "Model definition" and "DDL").

    This library uses only the stdlib, so that the ppx of phase 3 can link it.
    Its dune file has an empty [libraries] field, thus dune refuses a
    dependency. *)

val quote : string -> string
(** [quote name] is [name] between double quotes, for a message. Valid UTF-8
    stays as it is, for example ["café"]. A double quote and a backslash get a
    backslash before them. The characters U+0000 to U+001F and U+007F to U+009F
    become [\u{X}], with uppercase hex digits. Each byte of invalid UTF-8
    becomes [\xNN], with lowercase hex digits. *)

val valid_identifier : string -> (unit, string) result
(** [valid_identifier name] is [Ok ()] if [name] obeys the rules of the two
    servers for a column, an index or a constraint name:
    - It is valid UTF-8, with 1 to 64 characters (code points, not bytes).
    - Each character is in U+0001 to U+FFFF, because the servers keep names in
      utf8mb3.
    - It has no backquote.
    - It does not end with ASCII white space (space, tab, LF, VT, FF or CR).
      Other white space, for example U+3000, is permitted at the end.

    The message of [Error] tells the rule, and contains [quote name]. A name is
    program text, not a value of a row. *)

val valid_table_name : string -> (unit, string) result
(** [valid_table_name name] is [valid_identifier name], and also a limit of the
    file name. InnoDB keeps a table in the file [<encoded name>.ibd]. The
    encoded name has 1 byte for each character in [[0-9A-Za-z_]], and 3 or 5
    bytes for each other character. It has a limit of 251 bytes. This function
    counts 5 bytes for each other character, thus it can refuse a name that the
    servers accept, but it never accepts a name that they refuse. For example,
    64 × [€] is a valid column name, but it is not a valid table name.

    A table name must also not start with [#mysql50#] (with this case). The
    servers keep this prefix for the file names of old tables: MariaDB 11.4
    refuses it with error 1103, and MariaDB 11.8 uses another table. *)

val fold : string -> string
(** [fold name] changes the ASCII letters of [name] to lowercase. Cistern
    compares names after this fold. The servers also fold other letters (for
    example [É] to [é]), and MySQL removes accents from constraint names. Thus
    two names that differ only in a non-ASCII letter can pass the checks of
    Cistern, and the server then refuses the DDL. *)

val fnv1a32 : string -> int
(** [fnv1a32 s] is the FNV-1a 32 hash of the bytes of [s], in 0 to 0xffffffff.
    It does not change with the version of OCaml. *)

type kind =
  | Key  (** A unique index. *)
  | Idx  (** A plain index. *)
  | Fk  (** A foreign key constraint. *)

val index_name : table:string -> column:string -> kind -> string
(** [index_name ~table ~column kind] is the name of an index or a constraint on
    one column. The full name is [<table>_<column>_<kind>], with the kind [key],
    [idx] or [fk].
    - If the full name has only ASCII characters and at most 64 characters, the
      result is the full name.
    - Else the result is [<prefix>_<kind>_<hash>]. [<hash>] is the 8 lowercase
      hex digits of {!fnv1a32} of the full name. [<prefix>] is
      [<table>_<column>], cut on a character boundary so that the result has at
      most 64 characters.

    A name with a non-ASCII character always gets the hash. Thus two names that
    MySQL finds equal with no accents (for example [t_é_x_fk] and [t_e_x_fk])
    differ, except for a collision of the hash.

    If [table] and [column] pass {!valid_identifier}, the result also passes it.
*)
