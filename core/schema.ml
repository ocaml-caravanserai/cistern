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

let q = Cistern_names.quote

let quoted names = String.concat ", " (List.map q names)

(* The groups of two or more items with the same [key], in the order of the
   first item of each group. *)
let groups key items =
  let rec go acc = function
    | [] ->
        List.rev acc
    | x :: rest ->
        let same, other = List.partition (fun y -> key y = key x) rest in
        go (if same = [] then acc else (x :: same) :: acc) other
  in
  go [] items

(* The items of [l] with no repeated item, in the order of the first
   occurrence. *)
let distinct l =
  List.rev
    (List.fold_left
       (fun acc x -> if List.mem x acc then acc else x :: acc)
       [] l )

let keys (m : model) = List.filter (fun (f : field) -> f.key) m.fields

let relation_name = function Has_many {name; _} | Belongs_to {name; _} -> name

(* Rule 8 and rule 9 for the relation [r] of [m]. *)
let relation_faults models (m : model) r =
  let here =
    Printf.sprintf "model %s, relation %s" (q m.name) (q (relation_name r))
  in
  let absent target =
    Printf.sprintf "%s: the target %s is not a model of the list" here
      (q target)
  in
  match r with
  | Belongs_to {target; key; references; _} -> (
      let key_field = field_named m key in
      let no_key =
        if Option.is_none key_field then
          [ Printf.sprintf "%s: the key %s is not a field of the model" here
              (q key) ]
        else []
      in
      match model_named models target with
      | None ->
          no_key @ [absent target]
      | Some t -> (
        match keys t with
        | [tk] when tk.name <> references ->
            no_key
            @ [ Printf.sprintf
                  "%s: references %s is not the key %s of %s (version 1 refers \
                   only to the key)"
                  here (q references) (q tk.name) (q target) ]
        | [tk] when target = m.name && key = tk.name ->
            [ Printf.sprintf
                "%s: the Belongs_to refers to its own key (MySQL refuses this \
                 foreign key)"
                here ]
        | [tk] -> (
          match key_field with
          | Some k when k.scalar <> tk.scalar ->
              [ Printf.sprintf
                  "%s: the key %s and the field %s of %s have different types"
                  here (q key) (q references) (q target) ]
          | _ ->
              no_key )
        | _ ->
            (* Rule 3 refuses the target. *)
            no_key ) )
  | Has_many {target; _} -> (
    match model_named models target with
    | None ->
        [absent target]
    | Some t -> (
        let back =
          List.filter
            (function
              | Belongs_to {target; _} -> target = m.name | Has_many _ -> false )
            t.relations
        in
        match back with
        | [_] ->
            []
        | [] ->
            [ Printf.sprintf "%s: the model %s has no Belongs_to to %s" here
                (q target) (q m.name) ]
        | _ ->
            [ Printf.sprintf
                "%s: the model %s has %d Belongs_to to %s (version 1 has no \
                 via)"
                here (q target) (List.length back) (q m.name) ] ) )

(* The keys of the Belongs_to of [m], in order. *)
let belongs_to_keys (m : model) =
  List.filter_map
    (function Belongs_to {key; _} -> Some key | Has_many _ -> None)
    m.relations

(* Rules 2 to 10 for [m], and true if rules 2 and 5 pass. *)
let model_faults models (m : model) =
  let at (f : field) =
    Printf.sprintf "model %s, field %s" (q m.name) (q f.name)
  in
  let names =
    ( match Cistern_names.valid_table_name m.table with
      | Error e ->
          [Printf.sprintf "model %s: %s" (q m.name) e]
      | Ok () ->
          if String.exists Char.Ascii.is_upper m.table then
            [ Printf.sprintf
                "model %s: the table %s has an uppercase ASCII letter (version \
                 1 permits only lowercase table names)"
                (q m.name) (q m.table) ]
          else [] )
    @ List.filter_map
        (fun (f : field) ->
          match Cistern_names.valid_identifier f.column with
          | Error e ->
              Some (Printf.sprintf "%s: %s" (at f) e)
          | Ok () ->
              None )
        m.fields
  in
  let key =
    match keys m with
    | [] ->
        [Printf.sprintf "model %s: no field has [@key]" (q m.name)]
    | [k] ->
        ( if k.nullable then [Printf.sprintf "%s: the key is an option" (at k)]
          else [] )
        @
        if k.scalar = Float then [Printf.sprintf "%s: the key is a float" (at k)]
        else []
    | ks ->
        [ Printf.sprintf
            "model %s: %d fields have [@key] (version 1 has one key)" (q m.name)
            (List.length ks) ]
  in
  let auto (f : field) =
    ( if f.auto && not f.key then
        [Printf.sprintf "%s: [@auto] is only on the key" (at f)]
      else [] )
    @
    if f.auto && f.scalar <> Int64 then
      [Printf.sprintf "%s: [@auto] is only on an int64 field" (at f)]
    else []
  in
  let columns =
    List.map
      (fun g ->
        Printf.sprintf
          "model %s: the columns %s are equal with no regard to ASCII case"
          (q m.name)
          (quoted (List.map (fun (f : field) -> f.column) g)) )
      (groups (fun (f : field) -> Cistern_names.fold f.column) m.fields)
  in
  let index (f : field) =
    if f.key && (f.unique || f.index) then
      [ Printf.sprintf
          "%s: the key has no [@unique] or [@index] (it has an index)" (at f) ]
    else if f.unique && f.index then
      [ Printf.sprintf
          "%s: [@unique] and [@index] together (the unique index is an index)"
          (at f) ]
    else []
  in
  let relation_names =
    List.filter_map
      (fun name ->
        let count =
          List.length
            (List.filter (fun r -> relation_name r = name) m.relations)
        in
        let field = Option.is_some (field_named m name) in
        if count > 1 || field then
          Some
            (Printf.sprintf "model %s: the name %s is used by %s" (q m.name)
               (q name)
               ( ( if count > 1 then Printf.sprintf "%d relations" count
                   else "a relation" )
               ^ if field then " and a field" else "" ) )
        else None )
      (distinct (List.map relation_name m.relations))
  in
  let shared_keys =
    List.map
      (fun g ->
        Printf.sprintf "model %s: %d Belongs_to use the key %s" (q m.name)
          (List.length g)
          (q (List.hd g)) )
      (groups Fun.id (belongs_to_keys m))
  in
  let named = names = [] && columns = [] in
  let index_names =
    if not named then []
    else
      List.map
        (fun (g : index list) ->
          let names = List.map (fun (i : index) -> i.name) g in
          Printf.sprintf
            "model %s: the index names %s of the columns %s are equal with no \
             regard to ASCII case%s"
            (q m.name) (quoted names)
            (quoted (List.map (fun (i : index) -> i.column) g))
            ( if List.for_all (( = ) (List.hd names)) names then
                " (a collision of the hash: rename one column)"
              else "" ) )
        (groups
           (fun (i : index) -> Cistern_names.fold i.name)
           (List.filter (fun (i : index) -> i.kind <> Primary) (indexes m)) )
  in
  ( List.concat
      [ names
      ; key
      ; List.concat_map auto m.fields
      ; columns
      ; List.concat_map index m.fields
      ; relation_names
      ; shared_keys
      ; List.concat_map (relation_faults models m) m.relations
      ; index_names ]
  , named )

(* The foreign keys of [m], with one foreign key for each key column: rule 8
   refuses two Belongs_to on one key. *)
let distinct_foreign_keys models m =
  List.fold_left
    (fun acc (k : foreign_key) ->
      if List.exists (fun (j : foreign_key) -> j.column = k.column) acc then acc
      else acc @ [k] )
    [] (foreign_keys models m)

let check models =
  let model_names =
    List.map
      (fun g ->
        Printf.sprintf "%d models have the name %s" (List.length g)
          (q (List.hd g : model).name) )
      (groups (fun (m : model) -> m.name) models)
  in
  let tables = groups (fun (m : model) -> Cistern_names.fold m.table) models in
  let table_names =
    List.map
      (fun g ->
        Printf.sprintf
          "the tables %s of the models %s are equal with no regard to ASCII \
           case"
          (quoted (List.map (fun (m : model) -> m.table) g))
          (quoted (List.map (fun (m : model) -> m.name) g)) )
      tables
  in
  let each = List.map (fun m -> (m, model_faults models m)) models in
  let foreign =
    if tables <> [] then []
    else
      let named =
        List.filter_map (fun (m, (_, ok)) -> if ok then Some m else None) each
      in
      let fks =
        List.concat_map
          (fun (m : model) ->
            List.map (fun k -> (m, k)) (distinct_foreign_keys models m) )
          named
      in
      List.map
        (fun g ->
          Printf.sprintf
            "the foreign key names %s of the columns %s are equal with no \
             regard to ASCII case (a constraint name is unique in the \
             database)"
            (quoted (List.map (fun (_, (k : foreign_key)) -> k.name) g))
            (quoted
               (List.map
                  (fun ((m : model), (k : foreign_key)) ->
                    m.table ^ "." ^ k.column )
                  g ) ) )
        (groups (fun (_, (k : foreign_key)) -> Cistern_names.fold k.name) fks)
  in
  match
    List.concat
      [ model_names
      ; table_names
      ; List.concat_map (fun (_, (f, _)) -> f) each
      ; foreign ]
  with
  | [] ->
      Ok ()
  | faults ->
      Error faults
