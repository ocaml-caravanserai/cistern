(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

open Ppxlib
open Ast_builder.Default
module Schema = Cistern_schema.Schema

(* The names of a group (D7). *)

let alias (g : Desc.typ list) = "Cistern__private__" ^ (List.hd g).name

let fields_module (g : Desc.typ list) = "Cistern_fields__" ^ (List.hd g).name

let drafts_module (g : Desc.typ list) = "Cistern_drafts__" ^ (List.hd g).name

let model_module (t : Desc.typ) = "Cistern_model__" ^ t.name

let sub_module (t : Desc.typ) = String.capitalize_ascii t.name

let thunk g (f : Desc.field) =
  Printf.sprintf "cistern__default__%s__%d" (List.hd g : Desc.typ).name f.number

(* A path in the library, through the alias of the group. *)
let lib g ~loc path = evar ~loc (alias g ^ "." ^ path)

let lid ~loc s = {txt= Longident.parse s; loc}

(* The warning attribute of D13. *)
let warning ~loc =
  attribute ~loc
    ~name:{txt= "ocaml.warning"; loc}
    ~payload:
      (PStr
         [pstr_eval ~loc (estring ~loc "-23-27-30-32-34-37-40-41-42-60-69") []]
      )

let module_item ~loc name expr =
  pstr_module ~loc
    { (module_binding ~loc ~name:{txt= Some name; loc} ~expr) with
      pmb_attributes= [warning ~loc] }

(* Item 0: the alias and the default thunks (D7, D10). *)

let item0 ~loc g =
  let alias_item =
    module_item ~loc (alias g) (pmod_ident ~loc (lid ~loc "Cistern.Private"))
  in
  let thunks =
    List.concat_map
      (fun (t : Desc.typ) ->
        List.filter_map
          (fun (f : Desc.field) ->
            Option.map
              (fun e ->
                let loc = f.loc in
                let vb =
                  value_binding ~loc
                    ~pat:(pvar ~loc (thunk g f))
                    ~expr:[%expr fun () : [%t f.ty] -> [%e e]]
                in
                pstr_value ~loc Nonrecursive
                  [{vb with pvb_attributes= [warning ~loc]}] )
              f.default )
          t.fields )
      g
  in
  alias_item :: thunks

(* Item 1: the lifted models and the typed fields (D2, D8). *)

let scalar_name : Schema.scalar -> string = function
  | Int64 ->
      "Int64"
  | Int ->
      "Int"
  | String ->
      "String"
  | Bool ->
      "Bool"
  | Float ->
      "Float"
  | Bytes ->
      "Bytes"
  | Ptime ->
      "Ptime"

(* A record of the module Schema of the library: the first label has the path of
   the module, and the type gives the other labels. *)
let schema_record g ~loc fields =
  pexp_record ~loc
    (List.mapi
       (fun i (label, e) ->
         (lid ~loc (if i = 0 then alias g ^ ".Schema." ^ label else label), e) )
       fields )
    None

let lift_field g ~loc (f : Schema.field) =
  schema_record g ~loc
    [ ("name", estring ~loc f.name)
    ; ("column", estring ~loc f.column)
    ; ( "scalar"
      , pexp_construct ~loc
          (lid ~loc (alias g ^ ".Schema." ^ scalar_name f.scalar))
          None )
    ; ("nullable", ebool ~loc f.nullable)
    ; ("key", ebool ~loc f.key)
    ; ("auto", ebool ~loc f.auto)
    ; ("unique", ebool ~loc f.unique)
    ; ("index", ebool ~loc f.index) ]

let lift_model g ~loc (m : Schema.model) =
  schema_record g ~loc
    [ ("name", estring ~loc m.name)
    ; ("table", estring ~loc m.table)
    ; ( "fields"
      , elist ~loc
          (List.map (fun (f : Schema.field) -> evar ~loc f.name) m.fields) )
    ; ("relations", elist ~loc []) ]

let encoder : Schema.scalar -> string = function
  | Int64 ->
      "int64"
  | Int ->
      "int"
  | String ->
      "text"
  | Bool ->
      "bool"
  | Float ->
      "float"
  | Bytes ->
      "bytes"
  | Ptime ->
      "ptime"

let decoder : Schema.scalar -> string = function
  | Int64 ->
      "int64"
  | Int ->
      "int"
  | String ->
      "string"
  | Bool ->
      "bool"
  | Float ->
      "float"
  | Bytes ->
      "bytes"
  | Ptime ->
      "ptime"

let typed_field g (t : Desc.typ) (f : Desc.field) =
  let loc = f.loc in
  let record = ptyp_constr ~loc (lid ~loc t.name) [] in
  let encode = lib g ~loc ("Encode." ^ encoder f.scalar) in
  let decode = lib g ~loc ("Decoder." ^ decoder f.scalar) in
  let encode, decode =
    if f.nullable then
      ( [%expr [%e lib g ~loc "Encode.opt"] [%e encode]]
      , [%expr [%e lib g ~loc "Decoder.option"] [%e decode]] )
    else (encode, decode)
  in
  let value =
    match (f.scalar, f.nullable) with
    | Ptime, false ->
        [%expr [%e lib g ~loc "Encode.cut"] cistern__v]
    | Ptime, true ->
        [%expr [%e lib g ~loc "Encode.cut_opt"] cistern__v]
    | _ ->
        [%expr cistern__v]
  in
  let get = pexp_field ~loc [%expr cistern__r] (lid ~loc f.name) in
  let set =
    pexp_record ~loc [(lid ~loc f.name, value)] (Some [%expr cistern__r])
  in
  [%stri
    let [%p pvar ~loc f.name] =
      [%e lib g ~loc "Field.make"] ~model:Schema.cistern__model
        ~field:[%e evar ~loc ("Schema." ^ f.name)]
        ~encode:[%e encode] ~decode:[%e decode]
        ~get:(fun (cistern__r : [%t record]) -> [%e get])
        ~set:(fun (cistern__r : [%t record]) cistern__v -> [%e set])]

let item1 ~loc g =
  let models = Desc.schema g in
  let types =
    List.map2
      (fun (t : Desc.typ) (m : Schema.model) ->
        let schema =
          List.map2
            (fun (d : Desc.field) (f : Schema.field) ->
              let loc = d.loc in
              [%stri let [%p pvar ~loc f.name] = [%e lift_field g ~loc f]] )
            t.fields m.fields
          @ [ (let loc = t.loc in
               [%stri let cistern__model = [%e lift_model g ~loc m]] ) ]
        in
        let loc = t.loc in
        pstr_module ~loc
          (module_binding ~loc
             ~name:{txt= Some (sub_module t); loc}
             ~expr:
               (pmod_structure ~loc
                  ( pstr_module ~loc
                      (module_binding ~loc ~name:{txt= Some "Schema"; loc}
                         ~expr:(pmod_structure ~loc schema) )
                  :: List.map (typed_field g t) t.fields ) ) ) )
      g models
  in
  module_item ~loc (fields_module g) (pmod_structure ~loc types)

(* Item 2: the drafts and their encoders (D9). *)

let draft_fields (t : Desc.typ) =
  List.filter (fun (f : Desc.field) -> not f.auto) t.fields

let item2 ~loc g =
  let decl (t : Desc.typ) =
    let loc = t.loc in
    match draft_fields t with
    | [] ->
        type_declaration ~loc ~name:{txt= t.name; loc} ~params:[] ~cstrs:[]
          ~kind:Ptype_abstract ~private_:Public
          ~manifest:
            (Some (ptyp_constr ~loc (lid ~loc (alias g ^ ".Encode.empty")) []))
    | fs ->
        type_declaration ~loc ~name:{txt= t.name; loc} ~params:[] ~cstrs:[]
          ~kind:
            (Ptype_record
               (List.map
                  (fun (f : Desc.field) ->
                    label_declaration ~loc:f.loc ~name:{txt= f.name; loc= f.loc}
                      ~mutable_:Immutable ~type_:f.ty )
                  fs ) )
          ~private_:Public ~manifest:None
  in
  let encoder (t : Desc.typ) =
    let loc = t.loc in
    let name = pvar ~loc ("encode_" ^ t.name) in
    match draft_fields t with
    | [] ->
        [%stri let [%p name] = fun () -> []]
    | fs ->
        let draft = ptyp_constr ~loc (lid ~loc t.name) [] in
        let column (f : Desc.field) =
          let loc = f.loc in
          let field =
            evar ~loc (fields_module g ^ "." ^ sub_module t ^ "." ^ f.name)
          in
          [%expr
            [%e lib g ~loc "Field.column"] [%e field]
          , [%e lib g ~loc "Field.encode"] [%e field]
              [%e pexp_field ~loc [%expr cistern__d] (lid ~loc f.name)]]
        in
        [%stri
          let [%p name] =
           fun (cistern__d : [%t draft]) -> [%e elist ~loc (List.map column fs)]]
  in
  module_item ~loc (drafts_module g)
    (pmod_structure ~loc
       (pstr_type ~loc Recursive (List.map decl g) :: List.map encoder g) )

(* Item 3: one model module for each type (D7). *)

let key_field (t : Desc.typ) =
  List.find (fun (f : Desc.field) -> f.key) t.fields

let item3 g (t : Desc.typ) =
  let loc = t.loc in
  let typed (f : Desc.field) =
    evar ~loc:f.loc (fields_module g ^ "." ^ sub_module t ^ "." ^ f.name)
  in
  let key = key_field t in
  let user = ptyp_constr ~loc (lid ~loc t.name) [] in
  let draft = ptyp_constr ~loc (lid ~loc (drafts_module g ^ "." ^ t.name)) [] in
  let local i = Printf.sprintf "cistern__%d" i in
  let decode =
    List.fold_right
      (fun (i, (f : Desc.field)) body ->
        let loc = f.loc in
        [%expr
          let [%p pvar ~loc (local i)] =
            [%e lib g ~loc "Decoder.next"] cistern__c
              ([%e lib g ~loc "Field.decode"] [%e typed f])
          in
          [%e body]] )
      (List.mapi (fun i f -> (i, f)) t.fields)
      (pexp_record ~loc
         (List.mapi
            (fun i (f : Desc.field) -> (lid ~loc f.name, evar ~loc (local i)))
            t.fields )
         None )
  in
  let of_draft =
    pexp_record ~loc
      (List.map
         (fun (f : Desc.field) ->
           let loc = f.loc in
           let v = pexp_field ~loc [%expr cistern__d] (lid ~loc f.name) in
           ( lid ~loc f.name
           , match (f.auto, f.scalar, f.nullable) with
             | true, _, _ ->
                 [%expr [%e lib g ~loc "Encode.auto"] cistern__k]
             | false, Ptime, false ->
                 [%expr [%e lib g ~loc "Encode.cut"] [%e v]]
             | false, Ptime, true ->
                 [%expr [%e lib g ~loc "Encode.cut_opt"] [%e v]]
             | false, _, _ ->
                 v ) )
         t.fields )
      None
  in
  let body =
    [%str
      type nonrec t = [%t user]

      type key = [%t key.ty]

      type draft = [%t draft]

      let schema =
        [%e
          evar ~loc
            (fields_module g ^ "." ^ sub_module t ^ ".Schema.cistern__model")]

      let key = [%e typed key]

      let fields =
        [%e
          elist ~loc
            (List.map
               (fun f ->
                 pexp_construct ~loc
                   (lid ~loc (alias g ^ ".Field.Any"))
                   (Some (typed f)) )
               t.fields )]

      let decode cistern__c : t = [%e decode]

      let encode_draft = [%e evar ~loc (drafts_module g ^ ".encode_" ^ t.name)]

      let of_draft (cistern__d : draft) cistern__k : t = [%e of_draft]]
  in
  let constraint_ name ty =
    Pwith_type
      ( lid ~loc name
      , type_declaration ~loc ~name:{txt= name; loc} ~params:[] ~cstrs:[]
          ~kind:Ptype_abstract ~private_:Public ~manifest:(Some ty) )
  in
  let sig_ =
    pmty_with ~loc
      (pmty_ident ~loc (lid ~loc "Cistern.MODEL"))
      [constraint_ "t" user; constraint_ "key" key.ty; constraint_ "draft" draft]
  in
  module_item ~loc (model_module t)
    (pmod_constraint ~loc (pmod_structure ~loc body) sig_)

let stage1 g =
  let loc = (List.hd g : Desc.typ).loc in
  item0 ~loc g @ [item1 ~loc g; item2 ~loc g] @ List.map (item3 g) g
