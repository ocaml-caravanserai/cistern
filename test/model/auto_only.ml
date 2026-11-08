(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* The group of auto_only, written by hand: a model with only an [@auto] key.
   Its create is INSERT INTO auto_only () VALUES (). The source:

   {[ type auto_only = { id : int64 [@key] [@auto] } ]} *)

type auto_only = {id: int64}

module Cistern_fields__auto_only = struct
  module S = Cistern.Private.Schema

  let s_id : S.field =
    { name= "id"
    ; column= "id"
    ; scalar= Int64
    ; nullable= false
    ; key= true
    ; auto= true
    ; unique= false
    ; index= false }

  let schema : S.model =
    {name= "auto_only"; table= "auto_only"; fields= [s_id]; relations= []}

  let id : (auto_only, int64) Cistern.Private.Field.t =
    Cistern.Private.Field.make ~model:schema ~field:s_id
      ~encode:(fun i -> Cistern.Value.Int i)
      ~decode:Cistern.Private.Decoder.int64
      ~get:(fun r -> r.id)
      ~set:(fun _ v -> {id= v})
end

module Cistern_drafts__auto_only = struct
  type auto_only = unit

  let encode_auto_only () = []
end

module Cistern_model__auto_only :
  Cistern.MODEL
    with type t = auto_only
     and type key = int64
     and type draft = Cistern_drafts__auto_only.auto_only = struct
  module Fl = Cistern_fields__auto_only

  type t = auto_only

  type key = int64

  type draft = Cistern_drafts__auto_only.auto_only

  let schema = Fl.schema

  let key = Fl.id

  let fields = [Cistern.Private.Field.Any Fl.id]

  let decode c : t =
    {id= Cistern.Private.Decoder.next c Cistern.Private.Decoder.int64}

  let encode_draft = Cistern_drafts__auto_only.encode_auto_only

  let of_draft () k : t = {id= Option.value k ~default:0L}
end

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
