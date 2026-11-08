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

(* Item 0: the default thunks and the alias (D10, D7). The thunks come first, so
   that a type error of a payload comes before any use of the library. *)

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
  thunks @ [alias_item]

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

(* Items 4 and 5: stages 2 and 3 (D9). *)

let gen_module (g : Desc.typ list) = "Cistern_gen__" ^ (List.hd g).name

(* A function with the parameters [params] and the body [body], with the return
   type [ret] if any, as the parser makes it from [let f p1 p2 : ret = body]. *)
let func ~loc ?ret params body =
  pexp_function ~loc params
    (Option.map (fun t -> Pconstraint t) ret)
    (Pfunction_body body)

let param ~loc ?default label name =
  pparam_val ~loc label default (pvar ~loc name)

let value ~loc name expr =
  pstr_value ~loc Nonrecursive [value_binding ~loc ~pat:(pvar ~loc name) ~expr]

(* The labels of [draft] and [create]: [~f] for a field with no default and no
   option, [?f] for an option field or a field with a default (D9). *)
let draft_label (f : Desc.field) =
  if f.nullable || f.default <> None then Optional f.name else Labelled f.name

let item4 ~loc g =
  let typ (t : Desc.typ) =
    let loc = t.loc in
    let model = pexp_pack ~loc (pmod_ident ~loc (lid ~loc (model_module t))) in
    let model_fn name = lib g ~loc ("Model." ^ name) in
    let q (f : Desc.field) = evar ~loc:f.loc ("Q." ^ f.name) in
    let user = ptyp_constr ~loc (lid ~loc t.name) [] in
    let q_module =
      pstr_module ~loc
        (module_binding ~loc ~name:{txt= Some "Q"; loc}
           ~expr:
             (pmod_structure ~loc
                ( List.map
                    (fun (f : Desc.field) ->
                      let loc = f.loc in
                      value ~loc f.name
                        (evar ~loc
                           (fields_module g ^ "." ^ sub_module t ^ "." ^ f.name) ) )
                    t.fields
                @ [ pstr_include ~loc
                      (include_infos ~loc
                         (pmod_ident ~loc (lid ~loc (alias g ^ ".Expr.Ops"))) )
                  ] ) ) )
    in
    let key = key_field t in
    let by_key =
      let loc = key.loc in
      [ value ~loc ("get_by_" ^ key.name)
          (func ~loc
             [param ~loc Nolabel "cistern__k"]
             [%expr [%e model_fn "get"] [%e model] cistern__k] )
      ; value ~loc ("find_by_" ^ key.name)
          (func ~loc
             [param ~loc Nolabel "cistern__k"]
             [%expr [%e model_fn "find"] [%e model] cistern__k] ) ]
    in
    let by_unique =
      List.concat_map
        (fun (f : Desc.field) ->
          let loc = f.loc in
          let v =
            if f.nullable then [%expr [%e lib g ~loc "Encode.some"] cistern__v]
            else [%expr cistern__v]
          in
          [ value ~loc ("get_by_" ^ f.name)
              (func ~loc
                 [param ~loc Nolabel "cistern__v"]
                 [%expr [%e model_fn "get_by"] [%e model] [%e q f] [%e v]] )
          ; value ~loc ("find_by_" ^ f.name)
              (func ~loc
                 [param ~loc Nolabel "cistern__v"]
                 [%expr [%e model_fn "find_by"] [%e model] [%e q f] [%e v]] ) ] )
        (List.filter (fun (f : Desc.field) -> f.unique) t.fields)
    in
    let by_index =
      List.map
        (fun (f : Desc.field) ->
          let loc = f.loc in
          let eq =
            pexp_ident ~loc
              { txt= Ldot (Ldot (Ldot (Lident (alias g), "Expr"), "Ops"), "==.")
              ; loc }
          in
          value ~loc ("filter_by_" ^ f.name)
            (func ~loc
               [param ~loc Nolabel "cistern__v"]
               [%expr
                 [%e model_fn "where"] [%e model] ([%e eq] [%e q f] cistern__v)] ) )
        (List.filter (fun (f : Desc.field) -> f.index) t.fields)
    in
    let others = List.filter (fun (f : Desc.field) -> not f.key) t.fields in
    let update =
      value ~loc "update"
        (func ~loc
           ( List.map
               (fun (f : Desc.field) ->
                 param ~loc:f.loc (Optional f.name) f.name )
               others
           @ [param ~loc Nolabel "cistern__r"] )
           [%expr
             [%e model_fn "update"] [%e model]
               ([%e lib g ~loc "Encode.updates"]
                  [%e
                    elist ~loc
                      (List.map
                         (fun (f : Desc.field) ->
                           let loc = f.loc in
                           [%expr
                             [%e lib g ~loc "Encode.set"] [%e q f]
                               [%e evar ~loc f.name]] )
                         others )] )
               cistern__r] )
    in
    let draft_type =
      ptyp_constr ~loc (lid ~loc (drafts_module g ^ "." ^ t.name)) []
    in
    let draft =
      match draft_fields t with
      | [] ->
          value ~loc "draft"
            (func ~loc ~ret:draft_type
               [pparam_val ~loc Nolabel None (punit ~loc)]
               [%expr ()] )
      | fs ->
          let draft_param (f : Desc.field) =
            let loc = f.loc in
            match f.default with
            | Some _ ->
                param ~loc (Optional f.name) f.name
                  ~default:(eapply ~loc (evar ~loc (thunk g f)) [eunit ~loc])
            | None ->
                param ~loc (draft_label f) f.name
          in
          value ~loc "draft"
            (func ~loc ~ret:draft_type
               ( List.map draft_param fs
               @ [pparam_val ~loc Nolabel None (punit ~loc)] )
               (pexp_record ~loc
                  (List.map
                     (fun (f : Desc.field) ->
                       (lid ~loc:f.loc f.name, evar ~loc:f.loc f.name) )
                     fs )
                  None ) )
    in
    pstr_module ~loc
      (module_binding ~loc
         ~name:{txt= Some (sub_module t); loc}
         ~expr:
           (pmod_structure ~loc
              ( ([%stri type nonrec t = [%t user]] :: q_module :: by_key)
              @ by_unique @ by_index
              @ [ value ~loc "all" [%expr [%e model_fn "all"] [%e model]]
                ; value ~loc "where"
                    (func ~loc
                       [param ~loc Nolabel "cistern__e"]
                       [%expr [%e model_fn "where"] [%e model] cistern__e] )
                ; update
                ; value ~loc "delete"
                    (func ~loc
                       [param ~loc Nolabel "cistern__r"]
                       [%expr [%e model_fn "delete"] [%e model] cistern__r] )
                ; draft ] ) ) )
  in
  module_item ~loc (gen_module g) (pmod_structure ~loc (List.map typ g))

let item5 g (t : Desc.typ) =
  let loc = t.loc in
  let model = pexp_pack ~loc (pmod_ident ~loc (lid ~loc (model_module t))) in
  let draft_fn = evar ~loc (gen_module g ^ "." ^ sub_module t ^ ".draft") in
  let create =
    match draft_fields t with
    | [] ->
        func ~loc
          [pparam_val ~loc Nolabel None (punit ~loc)]
          [%expr [%e lib g ~loc "Model.create"] [%e model] ([%e draft_fn] ())]
    | fs ->
        func ~loc
          ( List.map
              (fun (f : Desc.field) -> param ~loc:f.loc (draft_label f) f.name)
              fs
          @ [pparam_val ~loc Nolabel None (punit ~loc)] )
          [%expr
            [%e lib g ~loc "Model.create"] [%e model]
              [%e
                pexp_apply ~loc draft_fn
                  ( List.map
                      (fun (f : Desc.field) ->
                        (draft_label f, evar ~loc:f.loc f.name) )
                      fs
                  @ [(Nolabel, eunit ~loc)] )]]
  in
  module_item ~loc (sub_module t)
    (pmod_structure ~loc
       [ pstr_include ~loc
           (include_infos ~loc
              (pmod_ident ~loc (lid ~loc (gen_module g ^ "." ^ sub_module t))) )
       ; pstr_module ~loc
           (module_binding ~loc ~name:{txt= Some "Drafts"; loc}
              ~expr:(pmod_ident ~loc (lid ~loc (drafts_module g))) )
       ; [%stri
           type draft = [%t ptyp_constr ~loc (lid ~loc ("Drafts." ^ t.name)) []]]
       ; [%stri let model : (module Cistern.MODEL) = [%e model]]
       ; value ~loc "create" create ] )

let all g =
  let loc = (List.hd g : Desc.typ).loc in
  stage1 g @ [item4 ~loc g] @ List.map (item5 g) g
