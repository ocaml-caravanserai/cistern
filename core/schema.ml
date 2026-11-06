(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

type scalar = Int64 | Int | String | Bool | Float | Bytes | Ptime

type field =
  { name: string
  ; column: string
  ; scalar: scalar
  ; nullable: bool
  ; key: bool
  ; auto: bool
  ; unique: bool
  ; index: bool }

type relation =
  | Has_many of {name: string; target: string}
  | Belongs_to of {name: string; target: string; key: string; references: string}

type model =
  {name: string; table: string; fields: field list; relations: relation list}

type column_type =
  | Bigint
  | Varchar_255
  | Mediumtext
  | Tinyint_1
  | Double
  | Varbinary_255
  | Mediumblob
  | Datetime_6

(* True if [f] is the key of a Belongs_to of [m]. *)
let belongs_to_key (m : model) (f : field) =
  List.exists
    (function Belongs_to {key; _} -> key = f.name | Has_many _ -> false)
    m.relations

(* True if [f] has an index: the key, [unique], [index], or the key of a
   Belongs_to. *)
let indexed m f = f.key || f.unique || f.index || belongs_to_key m f

let column_type m f =
  match f.scalar with
  | Int64 | Int ->
      Bigint
  | String ->
      if indexed m f then Varchar_255 else Mediumtext
  | Bool ->
      Tinyint_1
  | Float ->
      Double
  | Bytes ->
      if indexed m f then Varbinary_255 else Mediumblob
  | Ptime ->
      Datetime_6

type index_kind = Primary | Unique | Plain

type index = {name: string; column: string; kind: index_kind}

let indexes m =
  let name (f : field) kind =
    Cistern_names.index_name ~table:m.table ~column:f.column kind
  in
  let primary =
    List.filter_map
      (fun (f : field) ->
        if f.key then Some {name= "PRIMARY"; column= f.column; kind= Primary}
        else None )
      m.fields
  in
  let others =
    List.filter_map
      (fun (f : field) ->
        if f.key then None
        else if f.unique then
          Some {name= name f Key; column= f.column; kind= Unique}
        else if indexed m f then
          Some {name= name f Idx; column= f.column; kind= Plain}
        else None )
      m.fields
  in
  primary @ others

type foreign_key =
  {name: string; column: string; target_table: string; target_column: string}

let field_named (m : model) name =
  List.find_opt (fun (f : field) -> f.name = name) m.fields

let model_named models name =
  List.find_opt (fun (t : model) -> t.name = name) models

let foreign_keys models m =
  List.filter_map
    (function
      | Has_many _ ->
          None
      | Belongs_to {target; key; references; _} ->
          let ( let* ) = Option.bind in
          let* k = field_named m key in
          let* t = model_named models target in
          let* r = field_named t references in
          Some
            { name= Cistern_names.index_name ~table:m.table ~column:k.column Fk
            ; column= k.column
            ; target_table= t.table
            ; target_column= r.column } )
    m.relations
