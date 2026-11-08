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

(* The relations of a group (phase 4g, the plan of phase 4, section 3). *)

(* The belongs_to of [t] on the key field [f], if any (rule 8 permits one). *)
let belongs_on (t : Desc.typ) (f : Desc.field) =
  List.find_map
    (fun (r : Desc.relation) ->
      match r.kind with
      | Belongs_to {key= k, _; _} when k = f.name ->
          Some r
      | _ ->
          None )
    t.relations

let typ_named (g : Desc.typ list) name =
  List.find (fun (t : Desc.typ) -> t.name = name) g

(* The child of the has_many [target] of [t], its one belongs_to back to [t]
   (rule 9), and the key field of that belongs_to. *)
let back g (t : Desc.typ) target =
  let c = typ_named g target in
  let r, k =
    List.find_map
      (fun (r : Desc.relation) ->
        match r.kind with
        | Belongs_to {target; key= k, _; _} when target = t.name ->
            Some (r, k)
        | _ ->
            None )
      c.relations
    |> Option.get
  in
  (c, r, List.find (fun (f : Desc.field) -> f.name = k) c.fields)

(* The key fields of the belongs_to of [t] that have no [@index], [@unique] or
   [@key]: they act as [@index] (filter_by_<key>). *)
let belongs_keys (t : Desc.typ) =
  List.filter
    (fun (f : Desc.field) ->
      Option.is_some (belongs_on t f) && not (f.index || f.unique || f.key) )
    t.fields

let has_many (t : Desc.typ) =
  List.filter
    (fun (r : Desc.relation) ->
      match r.kind with Has_many _ -> true | Belongs_to _ -> false )
    t.relations

let set_module (t : Desc.typ) = "Set_" ^ t.name

(* A path in the library, through the alias of the group. *)
let lib g ~loc path = evar ~loc (alias g ^ "." ^ path)

let lid ~loc s = {txt= Longident.parse s; loc}

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
    ; ( "relations"
      , elist ~loc
          (List.map
             (function
               | Schema.Has_many {name; target} ->
                   pexp_construct ~loc
                     (lid ~loc (alias g ^ ".Schema.Has_many"))
                     (Some
                        (pexp_record ~loc
                           [ (lid ~loc "name", estring ~loc name)
                           ; (lid ~loc "target", estring ~loc target) ]
                           None ) )
               | Schema.Belongs_to {name; target; key; references} ->
                   pexp_construct ~loc
                     (lid ~loc (alias g ^ ".Schema.Belongs_to"))
                     (Some
                        (pexp_record ~loc
                           [ (lid ~loc "name", estring ~loc name)
                           ; (lid ~loc "target", estring ~loc target)
                           ; (lid ~loc "key", estring ~loc key)
                           ; (lid ~loc "references", estring ~loc references) ]
                           None ) ) )
             m.relations ) ) ]

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
    match belongs_on t f with
    | None ->
        pexp_record ~loc [(lid ~loc f.name, value)] (Some [%expr cistern__r])
    | Some r ->
        (* E9: a new key resets the relation. *)
        pexp_record ~loc
          [ (lid ~loc f.name, value)
          ; ( lid ~loc r.name
            , [%expr
                [%e lib g ~loc "Encode.reset"] [%e encode] [%e get] cistern__v
                  [%e pexp_field ~loc [%expr cistern__r] (lid ~loc r.name)]] )
          ]
          (Some [%expr cistern__r])
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
  (* E3: the setters of the relation fields, which set Loaded. *)
  let setters =
    List.filter_map
      (fun (t : Desc.typ) ->
        match t.relations with
        | [] ->
            None
        | rs ->
            let loc = t.loc in
            let record = ptyp_constr ~loc (lid ~loc t.name) [] in
            Some
              (pstr_module ~loc
                 (module_binding ~loc
                    ~name:{txt= Some (set_module t); loc}
                    ~expr:
                      (pmod_structure ~loc
                         (List.map
                            (fun (r : Desc.relation) ->
                              let loc = r.loc in
                              value ~loc r.name
                                (func ~loc
                                   [ pparam_val ~loc Nolabel None
                                       (ppat_constraint ~loc
                                          (pvar ~loc "cistern__r") record )
                                   ; param ~loc Nolabel "cistern__v" ]
                                   (pexp_record ~loc
                                      [ ( lid ~loc r.name
                                        , pexp_construct ~loc
                                            (lid ~loc
                                               (alias g ^ ".Deferred.Loaded") )
                                            (Some [%expr cistern__v]) ) ]
                                      (Some [%expr cistern__r]) ) ) )
                            rs ) ) ) ) )
      g
  in
  module_item ~loc (fields_module g) (pmod_structure ~loc (types @ setters))

(* Item 2: the drafts and their encoders (D9). *)

let draft_fields (t : Desc.typ) =
  List.filter (fun (f : Desc.field) -> not f.auto) t.fields

(* The key of a belongs_to that is not nullable: an option in the draft (E13),
   which a nested create fills. *)
let optional_key (t : Desc.typ) (f : Desc.field) =
  Option.is_some (belongs_on t f) && not f.nullable

(* The type of the field [f] of the draft of [t]. *)
let draft_type (t : Desc.typ) (f : Desc.field) =
  if optional_key t f then
    let loc = f.loc in
    [%type: [%t f.ty] option]
  else f.ty

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
                      ~mutable_:Immutable ~type_:(draft_type t f) )
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
          let v = pexp_field ~loc [%expr cistern__d] (lid ~loc f.name) in
          if optional_key t f then
            (* None gives NULL. *)
            [%expr
              [%e lib g ~loc "Field.column"] [%e field]
            , [%e lib g ~loc "Encode.opt"]
                ([%e lib g ~loc "Field.encode"] [%e field])
                [%e v]]
          else
            [%expr
              [%e lib g ~loc "Field.column"] [%e field]
            , [%e lib g ~loc "Field.encode"] [%e field] [%e v]]
        in
        [%stri
          let [%p name] =
           fun (cistern__d : [%t draft]) -> [%e elist ~loc (List.map column fs)]]
  in
  (* The setters of the draft keys of the belongs_to, for the fk of a nested
     create (E10). *)
  let setters =
    List.filter_map
      (fun (t : Desc.typ) ->
        let keys =
          List.filter (fun f -> Option.is_some (belongs_on t f)) t.fields
        in
        match keys with
        | [] ->
            None
        | keys ->
            let loc = t.loc in
            let draft = ptyp_constr ~loc (lid ~loc t.name) [] in
            Some
              (pstr_module ~loc
                 (module_binding ~loc
                    ~name:{txt= Some (set_module t); loc}
                    ~expr:
                      (pmod_structure ~loc
                         (List.map
                            (fun (f : Desc.field) ->
                              let loc = f.loc in
                              value ~loc f.name
                                (func ~loc
                                   [ pparam_val ~loc Nolabel None
                                       (ppat_constraint ~loc
                                          (pvar ~loc "cistern__d") draft )
                                   ; param ~loc Nolabel "cistern__k" ]
                                   (pexp_record ~loc
                                      [ ( lid ~loc f.name
                                        , [%expr
                                            [%e lib g ~loc "Encode.some"]
                                              cistern__k] ) ]
                                      (Some [%expr cistern__d]) ) ) )
                            keys ) ) ) ) )
      g
  in
  module_item ~loc (drafts_module g)
    (pmod_structure ~loc
       ( (pstr_type ~loc Recursive (List.map decl g) :: List.map encoder g)
       @ setters ) )

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
  (* E3: each relation field is Unloaded in decode and of_draft. *)
  let unloaded =
    List.map
      (fun (r : Desc.relation) ->
        let loc = r.loc in
        ( lid ~loc r.name
        , pexp_construct ~loc (lid ~loc (alias g ^ ".Deferred.Unloaded")) None
        ) )
      t.relations
  in
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
         ( List.mapi
             (fun i (f : Desc.field) -> (lid ~loc f.name, evar ~loc (local i)))
             t.fields
         @ unloaded )
         None )
  in
  let of_draft =
    pexp_record ~loc
      ( List.map
          (fun (f : Desc.field) ->
            let loc = f.loc in
            let v = pexp_field ~loc [%expr cistern__d] (lid ~loc f.name) in
            ( lid ~loc f.name
            , match (f.auto, f.scalar, f.nullable) with
              | true, _, _ ->
                  [%expr [%e lib g ~loc "Encode.auto"] cistern__k]
              | false, _, _ when optional_key t f ->
                  [%expr [%e lib g ~loc "Encode.draft_key"] [%e v]]
              | false, Ptime, false ->
                  [%expr [%e lib g ~loc "Encode.cut"] [%e v]]
              | false, Ptime, true ->
                  [%expr [%e lib g ~loc "Encode.cut_opt"] [%e v]]
              | false, _, _ ->
                  v ) )
          t.fields
      @ unloaded )
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

(* The labels of [draft] and [create]: [~f] for a field with no default and no
   option, [?f] for an option field or a field with a default (D9). *)
let draft_label (t : Desc.typ) (f : Desc.field) =
  if f.nullable || f.default <> None || optional_key t f then Optional f.name
  else Labelled f.name

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
        (List.filter (fun (f : Desc.field) -> f.index) t.fields @ belongs_keys t)
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
                param ~loc (draft_label t f) f.name
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

(* The scope of each relation of [t] (E11). *)
let scopes g (t : Desc.typ) =
  let pack (u : Desc.typ) =
    let loc = u.loc in
    pexp_pack ~loc (pmod_ident ~loc (lid ~loc (model_module u)))
  in
  let stage2 (u : Desc.typ) =
    let loc = u.loc in
    ptyp_constr ~loc (lid ~loc (gen_module g ^ "." ^ sub_module u ^ ".t")) []
  in
  let record ~loc =
    pparam_val ~loc Nolabel None
      (ppat_constraint ~loc (pvar ~loc "cistern__r") (stage2 t))
  in
  let get ~loc name = pexp_field ~loc [%expr cistern__r] (lid ~loc name) in
  List.map
    (fun (r : Desc.relation) ->
      let loc = r.loc in
      match r.kind with
      | Has_many {target} ->
          let c, _, kf = back g t target in
          let pk = key_field t in
          let v = get ~loc pk.name in
          let v =
            if kf.nullable then [%expr [%e lib g ~loc "Encode.some"] [%e v]]
            else v
          in
          let eq =
            pexp_ident ~loc
              { txt= Ldot (Ldot (Ldot (Lident (alias g), "Expr"), "Ops"), "==.")
              ; loc }
          in
          let field =
            evar ~loc (fields_module g ^ "." ^ sub_module c ^ "." ^ kf.name)
          in
          value ~loc r.name
            (func ~loc
               [record ~loc]
               [%expr
                 [%e lib g ~loc "Model.where"] [%e pack c]
                   ([%e eq] [%e field] [%e v])] )
      | Belongs_to {target; key= k, _; nullable; _} ->
          let p = typ_named g target in
          let fn = if nullable then "Model.get_parent" else "Model.get" in
          value ~loc r.name
            (func ~loc
               [record ~loc]
               [%expr [%e lib g ~loc fn] [%e pack p] [%e get ~loc k]] ) )
    t.relations

(* The module Rel of [t] (E2): one value for each relation, which the library
   checks against the schema at module initialization. *)
let rel g (t : Desc.typ) =
  let pack (u : Desc.typ) =
    let loc = u.loc in
    pexp_pack ~loc (pmod_ident ~loc (lid ~loc (model_module u)))
  in
  let stage2 (u : Desc.typ) =
    let loc = u.loc in
    ptyp_constr ~loc (lid ~loc (gen_module g ^ "." ^ sub_module u ^ ".t")) []
  in
  let setter ~loc (u : Desc.typ) name =
    evar ~loc (fields_module g ^ "." ^ set_module u ^ "." ^ name)
  in
  (* The key of the parent in a record [u]: Encode.some of a key that is not
     nullable. *)
  let key_fn ~loc (u : Desc.typ) (kf : Desc.field) =
    let v = pexp_field ~loc [%expr cistern__r] (lid ~loc kf.name) in
    func ~loc
      [ pparam_val ~loc Nolabel None
          (ppat_constraint ~loc (pvar ~loc "cistern__r") (stage2 u)) ]
      (if kf.nullable then v else [%expr [%e lib g ~loc "Encode.some"] [%e v]])
  in
  let items =
    List.map
      (fun (r : Desc.relation) ->
        let loc = r.loc in
        match r.kind with
        | Has_many {target} ->
            let c, _, kf = back g t target in
            value ~loc r.name
              [%expr
                [%e lib g ~loc "Relation.has_many"]
                  ~name:[%e estring ~loc r.name] [%e pack t] [%e pack c]
                  ~key:[%e key_fn ~loc c kf] ~set:[%e setter ~loc t r.name]]
        | Belongs_to {target; key= k, _; nullable; _} ->
            let p = typ_named g target in
            let kf = List.find (fun (f : Desc.field) -> f.name = k) t.fields in
            let set =
              if nullable then setter ~loc t r.name
              else
                (* The notes of the review of 4a: no parent of a key that is not
                   nullable leaves the relation Unloaded. *)
                [%expr
                  [%e lib g ~loc "Encode.parent"] [%e setter ~loc t r.name]
                    (fun (cistern__r : [%t stage2 t]) ->
                      [%e
                        pexp_record ~loc
                          [ ( lid ~loc r.name
                            , pexp_construct ~loc
                                (lid ~loc (alias g ^ ".Deferred.Unloaded"))
                                None ) ]
                          (Some [%expr cistern__r])] )]
            in
            value ~loc r.name
              [%expr
                [%e lib g ~loc "Relation.belongs_to"]
                  ~name:[%e estring ~loc r.name] [%e pack t] [%e pack p]
                  ~key:[%e key_fn ~loc t kf] ~set:[%e set]] )
      t.relations
  in
  let loc = t.loc in
  pstr_module ~loc
    (module_binding ~loc ~name:{txt= Some "Rel"; loc}
       ~expr:(pmod_structure ~loc items) )

let item5 g (t : Desc.typ) =
  let loc = t.loc in
  let model = pexp_pack ~loc (pmod_ident ~loc (lid ~loc (model_module t))) in
  let draft_fn = evar ~loc (gen_module g ^ "." ^ sub_module t ^ ".draft") in
  (* E10: one group of children for each ?<has_many> that the caller gave. *)
  let children =
    match has_many t with
    | [] ->
        []
    | rs ->
        [ ( Labelled "children"
          , [%expr
              [%e lib g ~loc "Encode.somes"]
                [%e
                  elist ~loc
                    (List.map
                       (fun (r : Desc.relation) ->
                         let loc = r.loc in
                         let target =
                           match r.kind with
                           | Has_many {target} ->
                               target
                           | Belongs_to _ ->
                               assert false
                         in
                         let c, _, kf = back g t target in
                         let fk =
                           evar ~loc
                             ( drafts_module g ^ "." ^ set_module c ^ "."
                             ^ kf.name )
                         in
                         let set =
                           evar ~loc
                             ( fields_module g ^ "." ^ set_module t ^ "."
                             ^ r.name )
                         in
                         [%expr
                           [%e lib g ~loc "Encode.group"]
                             ([%e lib g ~loc "Model.child"]
                                [%e
                                  pexp_pack ~loc
                                    (pmod_ident ~loc
                                       (lid ~loc (model_module c)) )]
                                ~fk:[%e fk] ~set:[%e set] )
                             [%e evar ~loc r.name]] )
                       rs )]] ) ]
  in
  let has_many_params =
    List.map
      (fun (r : Desc.relation) -> param ~loc:r.loc (Optional r.name) r.name)
      (has_many t)
  in
  let create =
    let call draft =
      pexp_apply ~loc
        (lib g ~loc "Model.create")
        (((Nolabel, model) :: children) @ [(Nolabel, draft)])
    in
    match draft_fields t with
    | [] ->
        func ~loc
          (has_many_params @ [pparam_val ~loc Nolabel None (punit ~loc)])
          (call [%expr [%e draft_fn] ()])
    | fs ->
        func ~loc
          ( List.map
              (fun (f : Desc.field) -> param ~loc:f.loc (draft_label t f) f.name)
              fs
          @ has_many_params
          @ [pparam_val ~loc Nolabel None (punit ~loc)] )
          (call
             (pexp_apply ~loc draft_fn
                ( List.map
                    (fun (f : Desc.field) ->
                      (draft_label t f, evar ~loc:f.loc f.name) )
                    fs
                @ [(Nolabel, eunit ~loc)] ) ) )
  in
  let relations =
    match t.relations with [] -> [] | _ -> scopes g t @ [rel g t]
  in
  module_item ~loc (sub_module t)
    (pmod_structure ~loc
       ( [ pstr_include ~loc
             (include_infos ~loc
                (pmod_ident ~loc (lid ~loc (gen_module g ^ "." ^ sub_module t))) )
         ; pstr_module ~loc
             (module_binding ~loc ~name:{txt= Some "Drafts"; loc}
                ~expr:(pmod_ident ~loc (lid ~loc (drafts_module g))) )
         ; [%stri
             type draft =
               [%t ptyp_constr ~loc (lid ~loc ("Drafts." ^ t.name)) []]]
         ; [%stri let model : (module Cistern.MODEL) = [%e model]]
         ; value ~loc "create" create ]
       @ relations ) )

let all g =
  let loc = (List.hd g : Desc.typ).loc in
  stage1 g @ [item4 ~loc g] @ List.map (item5 g) g
