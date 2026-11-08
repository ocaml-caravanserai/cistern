(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* The group of auto_only, written by hand: a model with only an [@auto] key.
   Its draft is empty (Cistern.Private.Encode.empty), and its create is INSERT
   INTO auto_only () VALUES (). Stage 1 has the form of the ppx (see
   kinds.ml). *)

type auto_only = {id: int64 [@key] [@auto]}

module Cistern__private__auto_only = Cistern.Private
[@@ocaml.warning "-23-27-30-32-34-37-40-41-42-60-69"]

module Cistern_fields__auto_only = struct
  module Auto_only = struct
    module Schema = struct
      let id =
        { Cistern__private__auto_only.Schema.name= "id"
        ; column= "id"
        ; scalar= Cistern__private__auto_only.Schema.Int64
        ; nullable= false
        ; key= true
        ; auto= true
        ; unique= false
        ; index= false }

      let cistern__model =
        { Cistern__private__auto_only.Schema.name= "auto_only"
        ; table= "auto_only"
        ; fields= [id]
        ; relations= [] }
    end

    let id =
      Cistern__private__auto_only.Field.make ~model:Schema.cistern__model
        ~field:Schema.id ~encode:Cistern__private__auto_only.Encode.int64
        ~decode:Cistern__private__auto_only.Decoder.int64
        ~get:(fun (cistern__r : auto_only) -> cistern__r.id)
        ~set:(fun (cistern__r : auto_only) cistern__v ->
          {cistern__r with id= cistern__v} )
  end
end
[@@ocaml.warning "-23-27-30-32-34-37-40-41-42-60-69"]

module Cistern_drafts__auto_only = struct
  type auto_only = Cistern__private__auto_only.Encode.empty

  let encode_auto_only () = []
end
[@@ocaml.warning "-23-27-30-32-34-37-40-41-42-60-69"]

module Cistern_model__auto_only :
  Cistern.MODEL
    with type t = auto_only
     and type key = int64
     and type draft = Cistern_drafts__auto_only.auto_only = struct
  type nonrec t = auto_only

  type key = int64

  type draft = Cistern_drafts__auto_only.auto_only

  let schema = Cistern_fields__auto_only.Auto_only.Schema.cistern__model

  let key = Cistern_fields__auto_only.Auto_only.id

  let fields =
    [ Cistern__private__auto_only.Field.Any
        Cistern_fields__auto_only.Auto_only.id ]

  let decode cistern__c : t =
    let cistern__0 =
      Cistern__private__auto_only.Decoder.next cistern__c
        (Cistern__private__auto_only.Field.decode
           Cistern_fields__auto_only.Auto_only.id )
    in
    {id= cistern__0}

  let encode_draft = Cistern_drafts__auto_only.encode_auto_only

  let of_draft (cistern__d : draft) cistern__k : t =
    {id= Cistern__private__auto_only.Encode.auto cistern__k}
end
[@@ocaml.warning "-23-27-30-32-34-37-40-41-42-60-69"]

module Auto_only = struct
  type t = auto_only

  let get_by_id k =
    Cistern.Private.Model.get (module Cistern_model__auto_only) k

  let delete r =
    Cistern.Private.Model.delete (module Cistern_model__auto_only) r

  let model : (module Cistern.MODEL) = (module Cistern_model__auto_only)

  let create () =
    Cistern.Private.Model.create (module Cistern_model__auto_only) ()
end
