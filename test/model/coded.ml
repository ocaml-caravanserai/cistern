(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* A parent with a key that is not [@auto] (a code) and two has_many, kids and
   pets, for the nested create (phase 4c1, the review of 4c1, M1). The children
   have an [@auto] key. Each model has a float field (a NaN has no literal).
   Only the parts that the nested create reads are real: decode is never
   read. *)

module S = Cistern.Private.Schema
module F = Cistern.Private.Field
module E = Cistern.Private.Encode
module Dc = Cistern.Private.Decoder

let fld ?(key = false) ?(auto = false) name scalar : S.field =
  { name
  ; column= name
  ; scalar
  ; nullable= false
  ; key
  ; auto
  ; unique= false
  ; index= false }

type p =
  { code: string
  ; x: float
  ; kids: c list Cistern.Deferred.t
  ; pets: d list Cistern.Deferred.t }

and c = {id: int64; pcode: string; y: float}

and d = {did: int64; owner: string; name: string}

let child_schema name ~id ~key ~other : S.model =
  { name
  ; table= name
  ; fields= [fld ~key:true ~auto:true id Int64; fld key String; other]
  ; relations= [Belongs_to {name= "p"; target= "p"; key; references= "code"}] }

module P = struct
  type t = p

  type key = string

  type draft = {code: string; x: float}

  let schema : S.model =
    { name= "p"
    ; table= "p"
    ; fields= [fld ~key:true "code" String; fld "x" Float]
    ; relations=
        [ Has_many {name= "kids"; target= "c"}
        ; Has_many {name= "pets"; target= "d"} ] }

  let key : (t, key) F.t =
    F.make ~model:schema ~field:(List.hd schema.fields) ~encode:E.text
      ~decode:Dc.string
      ~get:(fun (r : p) -> r.code)
      ~set:(fun (r : p) v -> {r with code= v})

  let fields = []

  let decode _ = failwith "not read"

  let encode_draft (d : draft) = [("code", E.text d.code); ("x", E.float d.x)]

  let of_draft (d : draft) _ : t =
    {code= d.code; x= d.x; kids= Unloaded; pets= Unloaded}
end

module C = struct
  type t = c

  type key = int64

  type draft = {pcode: string; y: float}

  let schema = child_schema "c" ~id:"id" ~key:"pcode" ~other:(fld "y" Float)

  let key : (t, key) F.t =
    F.make ~model:schema ~field:(List.hd schema.fields) ~encode:E.int64
      ~decode:Dc.int64
      ~get:(fun (r : c) -> r.id)
      ~set:(fun (r : c) v -> {r with id= v})

  let fields = []

  let decode _ = failwith "not read"

  let encode_draft (d : draft) = [("pcode", E.text d.pcode); ("y", E.float d.y)]

  let of_draft (d : draft) k : t = {id= E.auto k; pcode= d.pcode; y= d.y}
end

module D = struct
  type t = d

  type key = int64

  type draft = {owner: string; name: string}

  let schema =
    child_schema "d" ~id:"did" ~key:"owner" ~other:(fld "name" String)

  let key : (t, key) F.t =
    F.make ~model:schema ~field:(List.hd schema.fields) ~encode:E.int64
      ~decode:Dc.int64
      ~get:(fun (r : d) -> r.did)
      ~set:(fun (r : d) v -> {r with did= v})

  let fields = []

  let decode _ = failwith "not read"

  let encode_draft (d : draft) =
    [("owner", E.text d.owner); ("name", E.text d.name)]

  let of_draft (d : draft) k : t = {did= E.auto k; owner= d.owner; name= d.name}
end

(* [create ?x ?pets kids]: the parent "A" with the kids of the floats [kids],
   and the pets of the names [pets] if given. *)
let create ?(x = 1.) ?pets kids =
  let kids =
    Cistern.Private.Model.child
      (module C)
      ~fk:(fun (d : C.draft) k -> {d with pcode= k})
      ~set:(fun (r : p) l -> {r with kids= Loaded l})
      (List.map (fun y : C.draft -> {pcode= ""; y}) kids)
  in
  let pets =
    match pets with
    | None ->
        []
    | Some names ->
        [ Cistern.Private.Model.child
            (module D)
            ~fk:(fun (d : D.draft) k -> {d with owner= k})
            ~set:(fun (r : p) l -> {r with pets= Loaded l})
            (List.map (fun name : D.draft -> {owner= ""; name}) names) ]
  in
  Cistern.Private.Model.create (module P) ~children:(kids :: pets) {code= "A"; x}
