(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

open Ppxlib
module Schema = Cistern_schema.Schema

type field =
  { name: string
  ; loc: Location.t
  ; ty: core_type
  ; scalar: Schema.scalar
  ; nullable: bool
  ; key: bool
  ; auto: bool
  ; unique: bool
  ; index: bool
  ; column: (string * Location.t) option
  ; default: expression option
  ; number: int }

type kind =
  | Has_many of {target: string}
  | Belongs_to of
      { target: string
      ; key: string * Location.t
      ; references: string * Location.t
      ; nullable: bool }

type relation =
  {name: string; loc: Location.t; ty: core_type; attr: Location.t; kind: kind}

type typ =
  { name: string
  ; loc: Location.t
  ; table: (string * Location.t) option
  ; fields: field list
  ; relations: relation list }

let q = Cistern_names.quote

(* A located error. The text goes through Printf, not Format, because "@" is a
   directive of Format. *)
let fail ~loc fmt =
  Printf.ksprintf
    (fun text ->
      Location.raise_errorf ~loc "%s" ("[@@deriving cistern] " ^ text) )
    fmt

(* The attributes (D3). The declarations put the names in the namespace cistern
   for the driver. The parser reads the attributes itself, so that each error
   has the text of Cistern, and marks each one as handled. *)

let field_names = ["key"; "auto"; "unique"; "index"; "column"; "default"]

let relation_names = ["has_many"; "belongs_to"]

let known = field_names @ ["table"] @ relation_names

let () =
  List.iter
    (fun x ->
      ignore
        ( Attribute.declare ("cistern." ^ x) Attribute.Context.label_declaration
            Ast_pattern.__ Fun.id
          : (label_declaration, payload) Attribute.t ) )
    (field_names @ relation_names) ;
  ignore
    ( Attribute.declare "cistern.table" Attribute.Context.type_declaration
        Ast_pattern.__ Fun.id
      : (type_declaration, payload) Attribute.t )

(* The spelling of an attribute in a message. *)
let spell x = if x = "table" then "[@@table]" else "[@" ^ x ^ "]"

(* The optimal string alignment distance of two short names: an insertion, a
   deletion, a substitution or a transposition of two adjacent letters counts as
   1. *)
let distance a b =
  let m = String.length a and n = String.length b in
  (* Row 0 and column 0 are the distances to an empty name. The other cells get
     their values below. *)
  let d = Array.init (m + 1) (fun i -> Array.init (n + 1) (fun j -> i + j)) in
  for i = 1 to m do
    for j = 1 to n do
      let cost = if a.[i - 1] = b.[j - 1] then 0 else 1 in
      d.(i).(j) <-
        min
          (min (d.(i - 1).(j) + 1) (d.(i).(j - 1) + 1))
          (d.(i - 1).(j - 1) + cost) ;
      if i > 1 && j > 1 && a.[i - 1] = b.[j - 2] && a.[i - 2] = b.[j - 1] then
        d.(i).(j) <- min d.(i).(j) (d.(i - 2).(j - 2) + 1)
    done
  done ;
  d.(m).(n)

(* A bare name is a near miss of a Cistern name [k] at a distance of 1 for a
   name of 4 letters or fewer ([key], [auto]), else of 2. Thus short names of
   other derivers, as [@gen] or [@at], pass. *)
let near n k = distance n k <= if String.length k <= 4 then 1 else 2

(* The spellings of other ORMs, with the Cistern name. *)
let foreign =
  [ ("col", "column")
  ; ("unique_key", "unique")
  ; ("unique_index", "unique")
  ; ("primary_key", "key")
  ; ("primary", "key")
  ; ("pk", "key")
  ; ("auto_increment", "auto")
  ; ("autoincrement", "auto") ]

(* The Cistern name of an attribute ([key] for [@key] and [@cistern.key]), or
   None for an attribute of another program. An unknown [@cistern.x], a bare
   name near a Cistern name, and a bare spelling of another ORM are errors. [at]
   is "@" or "@@". *)
let cistern_name ~at (a : attribute) =
  let n = a.attr_name.txt and loc = a.attr_name.loc in
  let prefix = "cistern." in
  if String.starts_with ~prefix n then (
    let x =
      String.sub n (String.length prefix)
        (String.length n - String.length prefix)
    in
    if not (List.mem x known) then fail ~loc "unknown attribute [%s%s]" at n ;
    Some x )
  else if String.contains n '.' then None
  else if List.mem n known then Some n
  else
    let guess =
      match List.assoc_opt n foreign with
      | Some k ->
          Some k
      | None ->
          List.find_opt (near n) known
    in
    match guess with
    | Some k ->
        fail ~loc "unknown attribute [%s%s]: did you mean %s?" at n (spell k)
    | None ->
        None

(* The Cistern attributes of a node, each marked as handled for the driver, with
   no duplicate. [where] names the node in a message. *)
let cistern_attributes ~at ~where attrs =
  List.fold_left
    (fun acc (a : attribute) ->
      match cistern_name ~at a with
      | None ->
          acc
      | Some x ->
          if List.mem_assoc x acc then
            fail ~loc:a.attr_name.loc "the attribute %s comes two times on %s"
              (spell x) where ;
          Attribute.mark_as_handled_manually a ;
          acc @ [(x, a)] )
    [] attrs

let string_payload ~where x (a : attribute) =
  match a.attr_payload with
  | PStr
      [ { pstr_desc=
            Pstr_eval
              ({pexp_desc= Pexp_constant (Pconst_string (s, _, _)); _}, [])
        ; _ } ] ->
      s
  | _ ->
      fail ~loc:a.attr_loc "%s on %s takes one string" (spell x) where

let expression_payload ~where x (a : attribute) =
  match a.attr_payload with
  | PStr [{pstr_desc= Pstr_eval (e, []); _}] ->
      e
  | PStr [] ->
      fail ~loc:a.attr_loc "%s on %s has no expression" (spell x) where
  | _ ->
      fail ~loc:a.attr_loc "%s on %s takes one expression" (spell x) where

let flag_payload ~where x (a : attribute) =
  match a.attr_payload with
  | PStr [] ->
      ()
  | _ ->
      fail ~loc:a.attr_loc "%s on %s takes no payload" (spell x) where

(* The attributes in the type of a field, at each depth. *)
let rec core_type_attributes (t : core_type) =
  t.ptyp_attributes
  @
  match t.ptyp_desc with
  | Ptyp_constr (_, ts) | Ptyp_tuple ts | Ptyp_class (_, ts) ->
      List.concat_map core_type_attributes ts
  | Ptyp_arrow (_, a, b) ->
      core_type_attributes a @ core_type_attributes b
  | Ptyp_alias (t, _) | Ptyp_poly (_, t) ->
      core_type_attributes t
  | _ ->
      []

(* The shape and the name of a type. *)

let shape (td : type_declaration) =
  let loc = td.ptype_loc and name = q td.ptype_name.txt in
  if td.ptype_params <> [] then fail ~loc "the type %s has parameters" name ;
  if td.ptype_private = Private then fail ~loc "the type %s is private" name ;
  if td.ptype_manifest <> None then
    fail ~loc "the type %s is equal to another type" name ;
  match td.ptype_kind with
  | Ptype_record lds ->
      lds
  | _ ->
      fail ~loc "the type %s is not a record type" name

let scalars = ["int64"; "int"; "string"; "bool"; "float"; "bytes"; "option"]

let type_name ~has_table (td : type_declaration) =
  let n = td.ptype_name.txt and loc = td.ptype_name.loc in
  if not (Char.Ascii.is_lower n.[0] && String.for_all Char.Ascii.is_valid n)
  then
    fail ~loc
      "the type name %s must start with an ASCII lowercase letter and have \
       only ASCII characters"
      (q n) ;
  if String.exists Char.Ascii.is_upper n && not has_table then
    fail ~loc
      "the type name %s has an uppercase letter, which a table name cannot \
       have: give [@@table \"%s\"]"
      (q n) (String.lowercase_ascii n) ;
  if String.starts_with ~prefix:"cistern__" n then
    fail ~loc
      "the type name %s starts with \"cistern__\", the prefix of the generated \
       names"
      (q n) ;
  if n = "cistern" then
    fail ~loc "the type name %s: its module Cistern would hide the library"
      (q n) ;
  if String.starts_with ~prefix:"cistern" n then
    fail ~loc
      "the type name %s starts with \"cistern\", the prefix of the generated \
       modules"
      (q n) ;
  (* The name starts with a letter, thus an empty piece is between two
     underscores. *)
  if List.mem "" (String.split_on_char '_' (n ^ "x")) then
    fail ~loc
      "the type name %s has \"__\", the separator of the generated names" (q n) ;
  if n = "stdlib" then
    fail ~loc "the type name %s: its module Stdlib would hide the stdlib" (q n) ;
  if n = "ptime" then
    fail ~loc "the type name %s: its module Ptime would change Ptime.t" (q n) ;
  if n = "unit" || List.mem n scalars then
    fail ~loc
      "the type name %s is the name of a type of OCaml: it would change the \
       types of the fields of the program"
      (q n)

(* The attributes of a type and of its fields. *)

let type_attributes (td : type_declaration) =
  let where = "the type " ^ q td.ptype_name.txt in
  let attrs = cistern_attributes ~at:"@@" ~where td.ptype_attributes in
  List.iter
    (fun (x, (a : attribute)) ->
      if x <> "table" then
        fail ~loc:a.attr_name.loc "the attribute %s is for a field, not for %s"
          (spell x) where )
    attrs ;
  Option.map
    (fun (a : attribute) -> (string_payload ~where "table" a, a.attr_loc))
    (List.assoc_opt "table" attrs)

let where_field ~typ (ld : label_declaration) =
  Printf.sprintf "the field %s of the type %s" (q ld.pld_name.txt) (q typ)

(* The payload of a Cistern attribute of a field, parsed one time. *)
type payload =
  | Flag
  | Column of string
  | Default of expression
  | Refs of {key: string * Location.t; references: string * Location.t}

(* The payload of [@belongs_to] (E12): the record { key = <field>; references =
   <field> }, each label one time, with no other label. *)
let belongs_to_payload ~where (a : attribute) =
  let form = "takes the record { key = <field>; references = <field> }" in
  match a.attr_payload with
  | PStr
      [ { pstr_desc= Pstr_eval ({pexp_desc= Pexp_record (labels, base); _}, [])
        ; _ } ] ->
      ( match base with
      | Some (e : expression) ->
          fail ~loc:e.pexp_loc "[@belongs_to] on %s %s, with no \"with\"" where
            form
      | None ->
          () ) ;
      let value label (e : expression) =
        match e.pexp_desc with
        | Pexp_ident {txt= Lident x; _} ->
            (x, e.pexp_loc)
        | _ ->
            fail ~loc:e.pexp_loc
              "[@belongs_to] on %s: the value of %s must be the name of a field"
              where label
      in
      let found =
        List.fold_left
          (fun acc (({txt; loc} : longident loc), e) ->
            match txt with
            | Lident (("key" | "references") as l) ->
                if List.mem_assoc l acc then
                  fail ~loc "[@belongs_to] on %s has the label %s two times"
                    where l ;
                acc @ [(l, value l e)]
            | _ ->
                fail ~loc "[@belongs_to] on %s has an unknown label: it %s"
                  where form )
          [] labels
      in
      let get l =
        match List.assoc_opt l found with
        | Some v ->
            v
        | None ->
            fail ~loc:a.attr_loc "[@belongs_to] on %s has no label %s: it %s"
              where l form
      in
      Refs {key= get "key"; references= get "references"}
  | _ ->
      fail ~loc:a.attr_loc "[@belongs_to] on %s %s" where form

(* OCaml gives an attribute after the ";" of a field to that field. An attribute
   on a later line than its field and on the line of the name of the next field
   is for the next field. *)
let misplaced ~where ~(next : label_declaration option) (ld : label_declaration)
    x (a : attribute) =
  let line (l : Location.t) = l.loc_start.pos_lnum in
  match next with
  | Some n
    when line a.attr_loc > line ld.pld_name.loc
         && line a.attr_loc = line n.pld_name.loc ->
      let next = q n.pld_name.txt in
      fail ~loc:a.attr_name.loc
        "the attribute %s is after the \";\" of %s, thus OCaml gives it to %s, \
         not to %s on the same line: for %s, put it after its type, as in \"%s \
         : %s %s;\""
        (spell x) where (q ld.pld_name.txt) next next n.pld_name.txt
        (Format.asprintf "%a" Pprintast.core_type n.pld_type)
        (spell x)
  | _ ->
      ()

let label_attributes ~typ ~next (ld : label_declaration) =
  let where = where_field ~typ ld in
  List.iter
    (fun (a : attribute) ->
      match cistern_name ~at:"@" a with
      | Some x ->
          fail ~loc:a.attr_name.loc
            "the attribute %s is in the type of %s: put it after the type"
            (spell x) where
      | None ->
          () )
    (core_type_attributes ld.pld_type) ;
  List.iter
    (fun (a : attribute) ->
      Option.iter
        (fun x -> misplaced ~where ~next ld x a)
        (cistern_name ~at:"@" a) )
    ld.pld_attributes ;
  List.map
    (fun (x, (a : attribute)) ->
      if x = "table" then
        fail ~loc:a.attr_name.loc
          "the attribute [@@table] is for a type, not for %s" where ;
      let payload =
        if x = "column" then Column (string_payload ~where x a)
        else if x = "default" then Default (expression_payload ~where x a)
        else if x = "belongs_to" then belongs_to_payload ~where a
        else (flag_payload ~where x a ; Flag)
      in
      (x, (a, payload)) )
    (cistern_attributes ~at:"@" ~where ld.pld_attributes)

(* The type of a field (D4, D12). *)

let scalar_of = function
  | Lident "int64" ->
      Some Schema.Int64
  | Lident "int" ->
      Some Int
  | Lident "string" ->
      Some String
  | Lident "bool" ->
      Some Bool
  | Lident "float" ->
      Some Float
  | Lident "bytes" ->
      Some Bytes
  | Ldot (Lident "Ptime", "t") ->
      Some Ptime
  | _ ->
      None

(* [Deferred.t] or [Cistern.Deferred.t], by syntax only (E12). *)
let deferred = function
  | Ldot (Lident "Deferred", "t")
  | Ldot (Ldot (Lident "Cistern", "Deferred"), "t") ->
      true
  | _ ->
      false

let field_type ~group ~where (ld : label_declaration) =
  let loc = ld.pld_type.ptyp_loc in
  let relation (t : core_type) =
    match t.ptyp_desc with
    | Ptyp_constr ({txt= Lident n; _}, _) ->
        List.mem n group
    | Ptyp_constr ({txt= Ldot (Lident "Deferred", "t"); _}, _)
    | Ptyp_constr ({txt= Ldot (Ldot (_, "Deferred"), "t"); _}, _) ->
        true
    | _ ->
        false
  in
  let base (t : core_type) =
    if relation t then
      fail ~loc
        "%s has a type of the group or a Deferred.t, with no relation \
         attribute: give [@has_many] or [@belongs_to]"
        where ;
    match t.ptyp_desc with
    | Ptyp_constr ({txt; _}, []) ->
        scalar_of txt
    | Ptyp_poly _ ->
        fail ~loc "%s has a polymorphic type" where
    | _ ->
        None
  in
  let found =
    match ld.pld_type.ptyp_desc with
    | Ptyp_constr ({txt= Lident "option"; _}, [t]) ->
        Option.map (fun s -> (s, true)) (base t)
    | _ ->
        Option.map (fun s -> (s, false)) (base ld.pld_type)
  in
  match found with
  | Some r ->
      r
  | None ->
      fail ~loc
        "the type of %s is not a type of version 1: int64, int, string, bool, \
         float, bytes, Ptime.t, or an option of one of them (a type alias of \
         the program is not recognized)"
        where

let ops =
  [ "is_null"
  ; "in_"
  ; "starts_with"
  ; "contains"
  ; "starts_with_opt"
  ; "contains_opt"
  ; "not" ]

(* The checks of the name of each field, a column or a relation. *)
let field_name ~where (ld : label_declaration) =
  let name = ld.pld_name.txt and loc = ld.pld_name.loc in
  if List.mem name ops then
    fail ~loc "the name of %s is a name of the filters of Q (Expr.OPS)" where ;
  if String.starts_with ~prefix:"cistern__" name then
    fail ~loc
      "the name of %s starts with \"cistern__\", the prefix of the generated \
       names"
      where ;
  if ld.pld_mutable = Mutable then
    fail ~loc
      "%s is mutable: a mutable field is not supported (Field.set gives a copy)"
      where

(* A relation field is also a value of the model module (the scope): these names
   are values of stage 2 and 3 (DESIGN.md, "Model definition"). *)
let refused_relation =
  ["all"; "where"; "update"; "delete"; "draft"; "create"; "model"]

let refused_prefixes = ["get_by_"; "find_by_"; "filter_by_"]

(* A relation field (E12): [<t> list Deferred.t] with [@has_many], [<t>
   Deferred.t] or [<t> option Deferred.t] with [@belongs_to], [<t>] a type of
   the group. No other Cistern attribute. *)
let relation ~group ~typ (ld : label_declaration) attrs =
  let name = ld.pld_name.txt and loc = ld.pld_name.loc in
  let where = where_field ~typ ld in
  field_name ~where ld ;
  if
    List.mem name refused_relation
    || List.exists
         (fun prefix -> String.starts_with ~prefix name)
         refused_prefixes
  then
    fail ~loc
      "the name of %s, a relation field, is a value of the model module (all, \
       where, update, delete, draft, create, model, get_by_*, find_by_*, \
       filter_by_*)"
      where ;
  List.iter
    (fun (x, ((a : attribute), _)) ->
      if not (List.mem x relation_names) then
        fail ~loc:a.attr_name.loc
          "%s on %s, a relation field: a relation field is not a column"
          (spell x) where )
    attrs ;
  let attr, payload =
    match List.filter (fun (x, _) -> List.mem x relation_names) attrs with
    | [(_, ((a : attribute), p))] ->
        (a, p)
    | _ :: (_, ((a : attribute), _)) :: _ ->
        fail ~loc:a.attr_name.loc "%s has [@has_many] and [@belongs_to]" where
    | [] ->
        assert false
  in
  let tloc = ld.pld_type.ptyp_loc in
  let many = "<t> list Deferred.t, with <t> a type of the group"
  and one =
    "<t> Deferred.t or <t> option Deferred.t, with <t> a type of the group"
  in
  let bad x form =
    fail ~loc:tloc "%s on %s: the type of the field must be %s" (spell x) where
      form
  in
  (* The type of the group in [t], or the error of the form. *)
  let target x form (t : core_type) =
    match t.ptyp_desc with
    | Ptyp_constr ({txt= Lident n; _}, []) when List.mem n group ->
        n
    | Ptyp_constr ({txt= Lident n; _}, []) ->
        fail ~loc:t.ptyp_loc "%s on %s: the type %s is not a type of the group"
          (spell x) where (q n)
    | _ ->
        bad x form
  in
  let kind =
    match (payload, ld.pld_type.ptyp_desc) with
    | ( Flag
      , Ptyp_constr
          ( {txt; _}
          , [{ptyp_desc= Ptyp_constr ({txt= Lident "list"; _}, [t]); _}] ) )
      when deferred txt ->
        Has_many {target= target "has_many" many t}
    | Flag, _ ->
        bad "has_many" many
    | Refs {key; references}, Ptyp_constr ({txt; _}, [t]) when deferred txt -> (
      match t.ptyp_desc with
      | Ptyp_constr ({txt= Lident "option"; _}, [t]) ->
          Belongs_to
            {target= target "belongs_to" one t; key; references; nullable= true}
      | _ ->
          Belongs_to
            {target= target "belongs_to" one t; key; references; nullable= false}
      )
    | Refs _, _ ->
        bad "belongs_to" one
    | (Column _ | Default _), _ ->
        assert false
  in
  {name; loc; ty= ld.pld_type; attr= attr.attr_loc; kind}

let field ~group ~typ ~number (ld : label_declaration) attrs =
  let name = ld.pld_name.txt and loc = ld.pld_name.loc in
  let where = where_field ~typ ld in
  field_name ~where ld ;
  let scalar, nullable = field_type ~group ~where ld in
  let has x = List.mem_assoc x attrs in
  ( match List.assoc_opt "default" attrs with
  | Some ((a : attribute), _) when has "auto" ->
      fail ~loc:a.attr_loc
        "%s has [@auto]: the server gives its value, so it has no default" where
  | Some ((a : attribute), _) when has "key" ->
      fail ~loc:a.attr_loc
        "%s is the key: each row needs its own key, so it has no default" where
  | _ ->
      () ) ;
  { name
  ; loc
  ; ty= ld.pld_type
  ; scalar
  ; nullable
  ; key= has "key"
  ; auto= has "auto"
  ; unique= has "unique"
  ; index= has "index"
  ; column=
      ( match List.assoc_opt "column" attrs with
      | Some ((a : attribute), Column c) ->
          Some (c, a.attr_loc)
      | _ ->
          None )
  ; default=
      ( match List.assoc_opt "default" attrs with
      | Some (_, Default e) ->
          Some e
      | _ ->
          None )
  ; number }

let column (f : field) = match f.column with Some (c, _) -> c | None -> f.name

let table_name (t : typ) =
  match t.table with Some (x, _) -> x | None -> t.name

let schema g =
  List.map
    (fun (t : typ) ->
      { Schema.name= t.name
      ; table= table_name t
      ; fields=
          List.map
            (fun (f : field) ->
              { Schema.name= f.name
              ; column= column f
              ; scalar= f.scalar
              ; nullable= f.nullable
              ; key= f.key
              ; auto= f.auto
              ; unique= f.unique
              ; index= f.index } )
            t.fields
      ; relations=
          List.map
            (fun (r : relation) ->
              match r.kind with
              | Has_many {target} ->
                  Schema.Has_many {name= r.name; target}
              | Belongs_to {target; key; references; _} ->
                  Schema.Belongs_to
                    { name= r.name
                    ; target
                    ; key= fst key
                    ; references= fst references } )
            t.relations } )
    g

(* The second model of the fault of the list [f], as a type of [g]: the type
   with the name of the second item of [f.models], at its second occurrence if
   the first item has the same name. Rule 1 always names two models of the
   group. A rule of phase 4 (two equal foreign key names) can name fewer, thus
   the result is an option. *)
let second g (f : Schema.fault) =
  match f.models with
  | first :: name :: _ ->
      List.nth_opt
        (List.filter (fun (t : typ) -> t.name = name) g)
        (if first = name then 1 else 0)
  | _ ->
      None

(* The [@column] name [c] of [x] is the cause of a fault of [x]: it is not
   valid, or it is equal to another column of [t] after the fold. *)
let column_cause (t : typ) (x : field) c =
  Result.is_error (Cistern_names.valid_identifier c)
  || List.exists
       (fun (y : field) ->
         y.name <> x.name
         && Cistern_names.fold (column y) = Cistern_names.fold c )
       t.fields

(* A fault of the list with one type two times in its models: two foreign key
   names of the type [t] that are equal after the fold (rule 11; the hash of a
   long name can collide). The place is the attribute of the second belongs_to
   of the two (the review of 4f, L1). *)
let collision g (f : Schema.fault) =
  match f.models with
  | n :: (_ :: _ as rest) when List.for_all (String.equal n) rest -> (
      let t = List.find (fun (t : typ) -> t.name = n) g in
      let m = List.find (fun (m : Schema.model) -> m.name = n) (schema g) in
      let fks = Schema.foreign_keys (schema g) m in
      let rec second seen = function
        | (k : Schema.foreign_key) :: rest ->
            let name = Cistern_names.fold k.name in
            if List.mem name seen then Some k.column
            else second (name :: seen) rest
        | [] ->
            None
      in
      match second [] fks with
      | Some c ->
          List.find_map
            (fun (r : relation) ->
              match r.kind with
              | Belongs_to {key= k, _; _}
                when List.exists
                       (fun (x : field) -> x.name = k && column x = c)
                       t.fields ->
                  Some r.attr
              | _ ->
                  None )
            t.relations
      | None ->
          None )
  | _ ->
      None

let place ~loc g (f : Schema.fault) =
  let table_loc (t : typ) =
    match t.table with Some (_, loc) -> loc | None -> t.loc
  in
  let fields =
    if List.length f.fields > 1 then
      Printf.sprintf " (the fields %s)"
        (String.concat ", " (List.map q f.fields))
    else ""
  in
  let relation_of (t : typ) =
    List.find_opt (fun (r : relation) -> Some r.name = f.relation) t.relations
  in
  match List.find_opt (fun (t : typ) -> Some t.name = f.model) g with
  | Some t when Option.is_some (relation_of t) -> (
    (* E15: a fault of a relation goes to its field; for a belongs_to, to its
       attribute (the payload names the key and the referenced field). Rule 7
       cannot come from a record, which has unique labels. *)
    match relation_of t with
    | Some {kind= Belongs_to _; attr; _} ->
        fail ~loc:attr "%s" f.text
    | Some r ->
        fail ~loc:r.loc "%s" f.text
    | None ->
        assert false )
  | None -> (
    match second g f with
    | Some t ->
        fail ~loc:(if f.table then table_loc t else t.loc) "%s" f.text
    | None ->
        fail ~loc:(Option.value (collision g f) ~default:loc) "%s" f.text )
  | Some t when f.table ->
      let long =
        t.table = None
        && Result.is_error (Cistern_names.valid_table_name t.name)
      in
      fail ~loc:(table_loc t) "%s%s" f.text
        ( if long then " (give a shorter table name with [@@table \"...\"])"
          else "" )
  | Some t -> (
      (* A fault of two or more fields goes to the second one, the first that
         makes the conflict. *)
      let name =
        match f.fields with
        | _ :: second :: _ ->
            Some second
        | [x] ->
            Some x
        | [] ->
            None
      in
      match List.find_opt (fun (x : field) -> Some x.name = name) t.fields with
      | Some x ->
          let long =
            x.column = None
            && Result.is_error (Cistern_names.valid_identifier x.name)
          in
          let loc =
            match x.column with
            | Some (c, loc) when column_cause t x c ->
                loc
            | _ ->
                x.loc
          in
          fail ~loc "%s%s%s" f.text fields
            ( if long then " (give a shorter column name with [@column \"...\"])"
              else "" )
      | None ->
          fail ~loc:t.loc "%s" f.text )

let group ~loc tds =
  let lds = List.map shape tds in
  let tables = List.map type_attributes tds in
  List.iter2
    (fun td table -> type_name ~has_table:(table <> None) td)
    tds tables ;
  let names = List.map (fun (td : type_declaration) -> td.ptype_name.txt) tds in
  let attrs =
    List.map2
      (fun typ lds ->
        let nexts = List.map Option.some (List.tl lds) @ [None] in
        List.map2 (fun ld next -> label_attributes ~typ ~next ld) lds nexts )
      names lds
  in
  let number = ref 0 in
  let is_relation a = List.exists (fun (x, _) -> List.mem x relation_names) a in
  let g =
    List.map2
      (fun (td : type_declaration) (lds, (table, attrs)) ->
        let typ = td.ptype_name.txt in
        let pairs = List.combine lds attrs in
        { name= typ
        ; loc= td.ptype_name.loc
        ; table
        ; fields=
            List.filter_map
              (fun (ld, a) ->
                if is_relation a then None
                else
                  let f = field ~group:names ~typ ~number:!number ld a in
                  incr number ; Some f )
              pairs
        ; relations=
            List.filter_map
              (fun (ld, a) ->
                if is_relation a then Some (relation ~group:names ~typ ld a)
                else None )
              pairs } )
      tds
      (List.combine lds (List.combine tables attrs))
  in
  (* The key of a belongs_to is nullable exactly for <t> option Deferred.t (E12,
     E13). *)
  List.iter
    (fun (t : typ) ->
      List.iter
        (fun (r : relation) ->
          match r.kind with
          | Belongs_to {key= k, _; nullable; _} -> (
            match List.find_opt (fun (f : field) -> f.name = k) t.fields with
            | Some f when f.nullable <> nullable ->
                fail ~loc:r.ty.ptyp_loc
                  "the relation field %s of the type %s: the key %s is %s, \
                   thus the type is <t> %sDeferred.t"
                  (q r.name) (q t.name) (q k)
                  (if f.nullable then "an option" else "not an option")
                  (if f.nullable then "option " else "")
            | _ ->
                () )
          | Has_many _ ->
              () )
        t.relations )
    g ;
  (match Schema.faults (schema g) with [] -> () | f :: _ -> place ~loc g f) ;
  g
