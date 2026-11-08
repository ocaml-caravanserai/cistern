(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* A tree, written by hand in the three stages of DESIGN.md, "Generated code"
   (phase 4c2): a self relation with a nullable key. The source is this group:

   {[ type node = { id : int64; [@key] [@auto] name : string; parent_id : int64
   option; parent : node option Deferred.t; [@belongs_to { key = parent_id;
   references = id }] children : node list Deferred.t; [@has_many] } ]}

   Each call of the library goes through Cistern.Private. *)

type node =
  { id: int64
  ; name: string
  ; parent_id: int64 option
  ; parent: node option Cistern.Deferred.t
  ; children: node list Cistern.Deferred.t }

(* Stage 1: the schema, the fields and the setters of the relation fields. *)
module Cistern_fields__node = struct
  module S = Cistern.Private.Schema
  module F = Cistern.Private.Field
  module D = Cistern.Private.Decoder
  module E = Cistern.Private.Encode

  module Node = struct
    let s_id : S.field =
      { name= "id"
      ; column= "id"
      ; scalar= Int64
      ; nullable= false
      ; key= true
      ; auto= true
      ; unique= false
      ; index= false }

    let s_name : S.field =
      { s_id with
        name= "name"
      ; column= "name"
      ; scalar= String
      ; key= false
      ; auto= false }

    let s_parent_id : S.field =
      { s_id with
        name= "parent_id"
      ; column= "parent_id"
      ; nullable= true
      ; key= false
      ; auto= false }

    let schema : S.model =
      { name= "node"
      ; table= "node"
      ; fields= [s_id; s_name; s_parent_id]
      ; relations=
          [ Belongs_to
              { name= "parent"
              ; target= "node"
              ; key= "parent_id"
              ; references= "id" }
          ; Has_many {name= "children"; target= "node"} ] }

    let id : (node, int64) F.t =
      F.make ~model:schema ~field:s_id ~encode:E.int64 ~decode:D.int64
        ~get:(fun (r : node) -> r.id)
        ~set:(fun (r : node) v -> {r with id= v})

    let name : (node, string) F.t =
      F.make ~model:schema ~field:s_name ~encode:E.text ~decode:D.string
        ~get:(fun (r : node) -> r.name)
        ~set:(fun (r : node) v -> {r with name= v})

    (* The nullable key of the Belongs_to: a new key resets parent (E9). *)
    let parent_id : (node, int64 option) F.t =
      F.make ~model:schema ~field:s_parent_id ~encode:(E.opt E.int64)
        ~decode:(D.option D.int64)
        ~get:(fun (r : node) -> r.parent_id)
        ~set:(fun (r : node) v ->
          { r with
            parent_id= v
          ; parent= E.reset (E.opt E.int64) r.parent_id v r.parent } )
  end

  module Set_node = struct
    let parent (cistern__r : node) v =
      {cistern__r with parent= Cistern.Deferred.Loaded v}

    let children (cistern__r : node) v =
      {cistern__r with children= Cistern.Deferred.Loaded v}
  end
end

(* Stage 1: the draft. The nullable key is the key type in the draft (no option
   option, E13). *)
module Cistern_drafts__node = struct
  module Fl = Cistern_fields__node

  type node = {name: string; parent_id: int64 option}

  let encode_node (d : node) =
    let module N = Fl.Node in
    [ (Fl.F.column N.name, Fl.F.encode N.name d.name)
    ; (Fl.F.column N.parent_id, Fl.F.encode N.parent_id d.parent_id) ]

  module Set_node = struct
    let parent_id (d : node) k = {d with parent_id= Some k}
  end
end

(* Stage 1: the model module. *)
module Cistern_model__node :
  Cistern.MODEL
    with type t = node
     and type key = int64
     and type draft = Cistern_drafts__node.node = struct
  module Fl = Cistern_fields__node.Node
  module D = Cistern.Private.Decoder
  module F = Cistern.Private.Field

  type t = node

  type key = int64

  type draft = Cistern_drafts__node.node

  let schema = Fl.schema

  let key = Fl.id

  let fields = F.[Any Fl.id; Any Fl.name; Any Fl.parent_id]

  let decode c : t =
    let id = D.next c (F.decode Fl.id) in
    let name = D.next c (F.decode Fl.name) in
    let parent_id = D.next c (F.decode Fl.parent_id) in
    { id
    ; name
    ; parent_id
    ; parent= Cistern.Deferred.Unloaded
    ; children= Cistern.Deferred.Unloaded }

  let encode_draft = Cistern_drafts__node.encode_node

  let of_draft (d : draft) k : t =
    { id= Cistern.Private.Encode.auto k
    ; name= d.name
    ; parent_id= d.parent_id
    ; parent= Cistern.Deferred.Unloaded
    ; children= Cistern.Deferred.Unloaded }
end

(* Stage 2: the namespace module. *)
module Cistern_gen__node = struct
  module M = Cistern.Private.Model

  module Node = struct
    type t = node

    module Q = struct
      let id = Cistern_fields__node.Node.id

      let name = Cistern_fields__node.Node.name

      let parent_id = Cistern_fields__node.Node.parent_id

      include Cistern.Private.Expr.Ops
    end

    let get_by_id k = M.get (module Cistern_model__node) k

    let all = M.all (module Cistern_model__node)

    let where e = M.where (module Cistern_model__node) e

    let filter_by_parent_id v = where Q.(parent_id ==. v)

    let draft ~name ?parent_id () : Cistern_drafts__node.node = {name; parent_id}
  end
end

(* Stage 3: create with children, model, the scopes (E11) and Rel (E2). *)
module Node = struct
  include Cistern_gen__node.Node
  module Drafts = Cistern_drafts__node

  type draft = Drafts.node

  let model : (module Cistern.MODEL) = (module Cistern_model__node)

  (* A group only for a ?children that the caller gave (the rule of 4c1). *)
  let create ~name ?parent_id ?children () =
    Cistern.Private.Model.create
      (module Cistern_model__node)
      ~children:
        ( match children with
        | None ->
            []
        | Some cistern__ds ->
            [ Cistern.Private.Model.child
                (module Cistern_model__node)
                ~fk:Drafts.Set_node.parent_id
                ~set:Cistern_fields__node.Set_node.children cistern__ds ] )
      (draft ~name ?parent_id ())

  (* The scope of the has_many: the key of the child is nullable, thus the
     filter takes Some of the key. *)
  let children (cistern__r : node) =
    where Q.(parent_id ==. Cistern.Private.Encode.some cistern__r.id)

  (* The scope of the nullable belongs_to: None gives a pure None. *)
  let parent (cistern__r : node) =
    Cistern.Private.Model.get_parent
      (module Cistern_model__node)
      cistern__r.parent_id

  module Rel = struct
    let parent : (node, node) Cistern.Relation.t =
      Cistern.Private.Relation.belongs_to ~name:"parent"
        (module Cistern_model__node)
        (module Cistern_model__node)
        ~key:(fun (cistern__r : node) -> cistern__r.parent_id)
        ~set:Cistern_fields__node.Set_node.parent

    let children : (node, node) Cistern.Relation.t =
      Cistern.Private.Relation.has_many ~name:"children"
        (module Cistern_model__node)
        (module Cistern_model__node)
        ~key:(fun (cistern__r : node) -> cistern__r.parent_id)
        ~set:Cistern_fields__node.Set_node.children
  end
end
