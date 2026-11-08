(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

type ('m, 'a) t =
  { table: string
  ; field: Schema.field
  ; encode: 'a -> Value.t
  ; decode: 'a Decoder.t
  ; get: 'm -> 'a
  ; set: 'm -> 'a -> 'm }

let make ~(model : Schema.model) ~field ~encode ~decode ~get ~set =
  if not (List.mem field model.fields) then
    invalid_arg
      (Printf.sprintf "Field.make: the field %s is not a field of the model %s"
         (Cistern_names.quote field.Schema.name)
         (Cistern_names.quote model.name) ) ;
  {table= model.table; field; encode; decode; get; set}

let table f = f.table

let column f = f.field.column

let scalar f = f.field.scalar

let nullable f = f.field.nullable

let is_key f = f.field.key

let encode f = f.encode

let decode f = f.decode

let get f = f.get

let set f = f.set

type 'm any = Any : ('m, 'a) t -> 'm any
