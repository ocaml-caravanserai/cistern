(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

open Cistern_core

let ident = Print.ident

let type_text = function
  | Schema.Bigint ->
      "BIGINT"
  | Varchar_255 ->
      "VARCHAR(255)"
  | Mediumtext ->
      "MEDIUMTEXT"
  | Tinyint_1 ->
      "TINYINT(1)"
  | Double ->
      "DOUBLE"
  | Varbinary_255 ->
      "VARBINARY(255)"
  | Mediumblob ->
      "MEDIUMBLOB"
  | Datetime_6 ->
      "DATETIME(6)"

let create_table dialect models (m : Schema.model) =
  let column (f : Schema.field) =
    String.concat ""
      [ ident f.column
      ; " "
      ; type_text (Schema.column_type m f)
      ; (if f.nullable then " NULL" else " NOT NULL")
      ; (if f.auto then " AUTO_INCREMENT" else "") ]
  in
  let index (i : Schema.index) =
    match i.kind with
    | Primary ->
        "PRIMARY KEY (" ^ ident i.column ^ ")"
    | Unique ->
        "UNIQUE KEY " ^ ident i.name ^ " (" ^ ident i.column ^ ")"
    | Plain ->
        "KEY " ^ ident i.name ^ " (" ^ ident i.column ^ ")"
  in
  let foreign_key (k : Schema.foreign_key) =
    Printf.sprintf
      "CONSTRAINT %s FOREIGN KEY (%s) REFERENCES %s (%s) ON DELETE RESTRICT"
      (ident k.name) (ident k.column) (ident k.target_table)
      (ident k.target_column)
  in
  let parts =
    List.map column m.fields
    @ List.map index (Schema.indexes m)
    @ List.map foreign_key (Schema.foreign_keys models m)
  in
  Printf.sprintf
    "CREATE TABLE IF NOT EXISTS %s (%s) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 \
     COLLATE=%s ROW_FORMAT=DYNAMIC"
    (ident m.table) (String.concat ", " parts)
    (Dialect.collation dialect)

type column =
  { table: string
  ; column: string
  ; data_type: string
  ; nullable: bool
  ; max_length: int option
  ; precision: int option
  ; collation: string option
  ; unsigned: bool
  ; auto_increment: bool }

(* The values of information_schema for the DDL of each column type (the probe
   A7 of the plan of phase 2, the same on the three servers). *)
type expected =
  { data_type: string
  ; max_length: int option
  ; precision: int option
  ; text: bool  (** The column has the collation of the table. *) }

let medium = Some 16777215

let expected = function
  | Schema.Bigint ->
      {data_type= "bigint"; max_length= None; precision= None; text= false}
  | Varchar_255 ->
      {data_type= "varchar"; max_length= Some 255; precision= None; text= true}
  | Mediumtext ->
      {data_type= "mediumtext"; max_length= medium; precision= None; text= true}
  | Tinyint_1 ->
      {data_type= "tinyint"; max_length= None; precision= None; text= false}
  | Double ->
      {data_type= "double"; max_length= None; precision= None; text= false}
  | Varbinary_255 ->
      { data_type= "varbinary"
      ; max_length= Some 255
      ; precision= None
      ; text= false }
  | Mediumblob ->
      {data_type= "mediumblob"; max_length= medium; precision= None; text= false}
  | Datetime_6 ->
      {data_type= "datetime"; max_length= None; precision= Some 6; text= false}

let q = Cistern_names.quote

let opt f = function None -> "none" | Some v -> f v

let compare dialect models rows =
  let model_faults (m : Schema.model) =
    let mine =
      List.filter (fun (r : column) -> String.equal r.table m.table) rows
    in
    if mine = [] then [Printf.sprintf "the table %s does not exist" (q m.table)]
    else
      let same c (r : column) =
        String.equal (Cistern_names.fold c) (Cistern_names.fold r.column)
      in
      let field (f : Schema.field) =
        let at = Printf.sprintf "%s.%s" (q m.table) (q f.column) in
        match List.find_opt (same f.column) mine with
        | None ->
            [ Printf.sprintf "the table %s has no column %s" (q m.table)
                (q f.column) ]
        | Some r ->
            let e = expected (Schema.column_type m f) in
            if
              not
                (String.equal (String.lowercase_ascii r.data_type) e.data_type)
            then
              [ Printf.sprintf "%s: the type is %s, the model needs %s" at
                  (q r.data_type) e.data_type ]
            else
              let collation =
                if e.text then Some (Dialect.collation dialect) else None
              in
              List.concat
                [ ( if r.nullable <> f.nullable then
                      [ Printf.sprintf
                          "%s: the column is %s, the model needs %s" at
                          (if r.nullable then "NULL" else "NOT NULL")
                          (if f.nullable then "NULL" else "NOT NULL") ]
                    else [] )
                ; ( if r.max_length <> e.max_length then
                      [ Printf.sprintf
                          "%s: the length is %s, the model needs %s" at
                          (opt string_of_int r.max_length)
                          (opt string_of_int e.max_length) ]
                    else [] )
                ; ( if r.precision <> e.precision then
                      [ Printf.sprintf
                          "%s: the precision is %s, the model needs %s" at
                          (opt string_of_int r.precision)
                          (opt string_of_int e.precision) ]
                    else [] )
                ; ( if r.collation <> collation then
                      [ Printf.sprintf
                          "%s: the collation is %s, the model needs %s" at
                          (opt q r.collation) (opt Fun.id collation) ]
                    else [] )
                ; ( if r.unsigned then
                      [Printf.sprintf "%s: the column is unsigned" at]
                    else [] )
                ; ( if f.auto && not r.auto_increment then
                      [ Printf.sprintf
                          "%s: the column has no AUTO_INCREMENT, the model \
                           needs it ([@auto])"
                          at ]
                    else if r.auto_increment && not f.auto then
                      [ Printf.sprintf
                          "%s: the column has AUTO_INCREMENT, the model has no \
                           [@auto]"
                          at ]
                    else [] ) ]
      in
      let extra =
        List.filter_map
          (fun (r : column) ->
            if List.exists (fun (f : Schema.field) -> same f.column r) m.fields
            then None
            else
              Some
                (Printf.sprintf
                   "the table %s has the column %s, which the model %s does \
                    not have"
                   (q m.table) (q r.column) (q m.name) ) )
          mine
      in
      List.concat_map field m.fields @ extra
  in
  List.concat_map model_faults models
