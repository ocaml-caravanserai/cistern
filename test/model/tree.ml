(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* A tree, written by hand (phase 4c2): a self relation with a nullable key.
   Phase 4g wrote the items after the declaration again from the generator: they
   have the form of the ppx (see kinds.ml). *)

type node =
  { id: int64 [@key] [@auto]
  ; name: string
  ; parent_id: int64 option
  ; parent: node option Cistern.Deferred.t
        [@belongs_to {key= parent_id; references= id}]
  ; children: node list Cistern.Deferred.t [@has_many] }

module Cistern__private__node = Cistern.Private
[@@ocaml.warning "-23-27-30-32-34-37-40-41-42-60-69"]

module Cistern_fields__node = struct
  module Node = struct
    module Schema = struct
      let id =
        { Cistern__private__node.Schema.name= "id"
        ; column= "id"
        ; scalar= Cistern__private__node.Schema.Int64
        ; nullable= false
        ; key= true
        ; auto= true
        ; unique= false
        ; index= false }

      let name =
        { Cistern__private__node.Schema.name= "name"
        ; column= "name"
        ; scalar= Cistern__private__node.Schema.String
        ; nullable= false
        ; key= false
        ; auto= false
        ; unique= false
        ; index= false }

      let parent_id =
        { Cistern__private__node.Schema.name= "parent_id"
        ; column= "parent_id"
        ; scalar= Cistern__private__node.Schema.Int64
        ; nullable= true
        ; key= false
        ; auto= false
        ; unique= false
        ; index= false }

      let cistern__model =
        { Cistern__private__node.Schema.name= "node"
        ; table= "node"
        ; fields= [id; name; parent_id]
        ; relations=
            [ Cistern__private__node.Schema.Belongs_to
                { name= "parent"
                ; target= "node"
                ; key= "parent_id"
                ; references= "id" }
            ; Cistern__private__node.Schema.Has_many
                {name= "children"; target= "node"} ] }
    end

    let id =
      Cistern__private__node.Field.make ~model:Schema.cistern__model
        ~field:Schema.id ~encode:Cistern__private__node.Encode.int64
        ~decode:Cistern__private__node.Decoder.int64
        ~get:(fun (cistern__r : node) -> cistern__r.id)
        ~set:(fun (cistern__r : node) cistern__v ->
          {cistern__r with id= cistern__v} )

    let name =
      Cistern__private__node.Field.make ~model:Schema.cistern__model
        ~field:Schema.name ~encode:Cistern__private__node.Encode.text
        ~decode:Cistern__private__node.Decoder.string
        ~get:(fun (cistern__r : node) -> cistern__r.name)
        ~set:(fun (cistern__r : node) cistern__v ->
          {cistern__r with name= cistern__v} )

    let parent_id =
      Cistern__private__node.Field.make ~model:Schema.cistern__model
        ~field:Schema.parent_id
        ~encode:
          (Cistern__private__node.Encode.opt Cistern__private__node.Encode.int64)
        ~decode:
          (Cistern__private__node.Decoder.option
             Cistern__private__node.Decoder.int64 )
        ~get:(fun (cistern__r : node) -> cistern__r.parent_id)
        ~set:(fun (cistern__r : node) cistern__v ->
          { cistern__r with
            parent_id= cistern__v
          ; parent=
              Cistern__private__node.Encode.reset
                (Cistern__private__node.Encode.opt
                   Cistern__private__node.Encode.int64 )
                cistern__r.parent_id cistern__v cistern__r.parent } )
  end

  module Set_node = struct
    let parent (cistern__r : node) cistern__v =
      {cistern__r with parent= Cistern__private__node.Deferred.Loaded cistern__v}

    let children (cistern__r : node) cistern__v =
      { cistern__r with
        children= Cistern__private__node.Deferred.Loaded cistern__v }
  end
end
[@@ocaml.warning "-23-27-30-32-34-37-40-41-42-60-69"]

module Cistern_drafts__node = struct
  type node = {name: string; parent_id: int64 option}

  let encode_node (cistern__d : node) =
    [ ( Cistern__private__node.Field.column Cistern_fields__node.Node.name
      , Cistern__private__node.Field.encode Cistern_fields__node.Node.name
          cistern__d.name )
    ; ( Cistern__private__node.Field.column Cistern_fields__node.Node.parent_id
      , Cistern__private__node.Field.encode Cistern_fields__node.Node.parent_id
          cistern__d.parent_id ) ]

  module Set_node = struct
    let parent_id (cistern__d : node) cistern__k =
      {cistern__d with parent_id= Cistern__private__node.Encode.some cistern__k}
  end
end
[@@ocaml.warning "-23-27-30-32-34-37-40-41-42-60-69"]

module Cistern_model__node :
  Cistern.MODEL
    with type t = node
     and type key = int64
     and type draft = Cistern_drafts__node.node = struct
  type nonrec t = node

  type key = int64

  type draft = Cistern_drafts__node.node

  let schema = Cistern_fields__node.Node.Schema.cistern__model

  let key = Cistern_fields__node.Node.id

  let fields =
    [ Cistern__private__node.Field.Any Cistern_fields__node.Node.id
    ; Cistern__private__node.Field.Any Cistern_fields__node.Node.name
    ; Cistern__private__node.Field.Any Cistern_fields__node.Node.parent_id ]

  let decode cistern__c : t =
    let cistern__0 =
      Cistern__private__node.Decoder.next cistern__c
        (Cistern__private__node.Field.decode Cistern_fields__node.Node.id)
    in
    let cistern__1 =
      Cistern__private__node.Decoder.next cistern__c
        (Cistern__private__node.Field.decode Cistern_fields__node.Node.name)
    in
    let cistern__2 =
      Cistern__private__node.Decoder.next cistern__c
        (Cistern__private__node.Field.decode Cistern_fields__node.Node.parent_id)
    in
    { id= cistern__0
    ; name= cistern__1
    ; parent_id= cistern__2
    ; parent= Cistern__private__node.Deferred.Unloaded
    ; children= Cistern__private__node.Deferred.Unloaded }

  let encode_draft = Cistern_drafts__node.encode_node

  let of_draft (cistern__d : draft) cistern__k : t =
    { id= Cistern__private__node.Encode.auto cistern__k
    ; name= cistern__d.name
    ; parent_id= cistern__d.parent_id
    ; parent= Cistern__private__node.Deferred.Unloaded
    ; children= Cistern__private__node.Deferred.Unloaded }
end
[@@ocaml.warning "-23-27-30-32-34-37-40-41-42-60-69"]

module Cistern_gen__node = struct
  module Node = struct
    type nonrec t = node

    module Q = struct
      let id = Cistern_fields__node.Node.id

      let name = Cistern_fields__node.Node.name

      let parent_id = Cistern_fields__node.Node.parent_id

      include Cistern__private__node.Expr.Ops
    end

    let get_by_id cistern__k =
      Cistern__private__node.Model.get (module Cistern_model__node) cistern__k

    let find_by_id cistern__k =
      Cistern__private__node.Model.find (module Cistern_model__node) cistern__k

    let filter_by_parent_id cistern__v =
      Cistern__private__node.Model.where
        (module Cistern_model__node)
        (Cistern__private__node.Expr.Ops.( ==. ) Q.parent_id cistern__v)

    let all = Cistern__private__node.Model.all (module Cistern_model__node)

    let where cistern__e =
      Cistern__private__node.Model.where (module Cistern_model__node) cistern__e

    let update ?name ?parent_id cistern__r =
      Cistern__private__node.Model.update
        (module Cistern_model__node)
        (Cistern__private__node.Encode.updates
           [ Cistern__private__node.Encode.set Q.name name
           ; Cistern__private__node.Encode.set Q.parent_id parent_id ] )
        cistern__r

    let delete cistern__r =
      Cistern__private__node.Model.delete
        (module Cistern_model__node)
        cistern__r

    let draft ~name ?parent_id () : Cistern_drafts__node.node = {name; parent_id}
  end
end
[@@ocaml.warning "-23-27-30-32-34-37-40-41-42-60-69"]

module Node = struct
  include Cistern_gen__node.Node
  module Drafts = Cistern_drafts__node

  type draft = Drafts.node

  let model : (module Cistern.MODEL) = (module Cistern_model__node)

  let create ~name ?parent_id ?children () =
    Cistern__private__node.Model.create
      (module Cistern_model__node)
      ~children:
        (Cistern__private__node.Encode.somes
           [ Cistern__private__node.Encode.group
               (Cistern__private__node.Model.child
                  (module Cistern_model__node)
                  ~fk:Cistern_drafts__node.Set_node.parent_id
                  ~set:Cistern_fields__node.Set_node.children )
               children ] )
      (Cistern_gen__node.Node.draft ~name ?parent_id ())

  let parent (cistern__r : Cistern_gen__node.Node.t) =
    Cistern__private__node.Model.get_parent
      (module Cistern_model__node)
      cistern__r.parent_id

  let children (cistern__r : Cistern_gen__node.Node.t) =
    Cistern__private__node.Model.where
      (module Cistern_model__node)
      (Cistern__private__node.Expr.Ops.( ==. )
         Cistern_fields__node.Node.parent_id
         (Cistern__private__node.Encode.some cistern__r.id) )

  module Rel = struct
    let parent =
      Cistern__private__node.Relation.belongs_to ~name:"parent"
        (module Cistern_model__node)
        (module Cistern_model__node)
        ~key:(fun (cistern__r : Cistern_gen__node.Node.t) ->
          cistern__r.parent_id )
        ~set:Cistern_fields__node.Set_node.parent

    let children =
      Cistern__private__node.Relation.has_many ~name:"children"
        (module Cistern_model__node)
        (module Cistern_model__node)
        ~key:(fun (cistern__r : Cistern_gen__node.Node.t) ->
          cistern__r.parent_id )
        ~set:Cistern_fields__node.Set_node.children
  end
end
[@@ocaml.warning "-23-27-30-32-34-37-40-41-42-60-69"]
