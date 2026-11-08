(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

module type MODEL = Cistern_core.Model_intf.MODEL

module Schema = Cistern_core.Schema

type ('m, 'r) t =
  | Has_many :
      { name: string
      ; parent: (module MODEL with type t = 'm and type key = 'k)
      ; child: (module MODEL with type t = 'r)
      ; key: 'r -> 'k option
      ; set: 'm -> 'r list -> 'm }
      -> ('m, 'r) t
  | Belongs_to :
      { name: string
      ; child: (module MODEL with type t = 'm)
      ; parent: (module MODEL with type t = 'r and type key = 'k)
      ; key: 'm -> 'k option
      ; set: 'm -> 'r option -> 'm }
      -> ('m, 'r) t

let name : type m r. (m, r) t -> string = function
  | Has_many r ->
      r.name
  | Belongs_to r ->
      r.name

let kind : type m r. (m, r) t -> [`Has_many | `Belongs_to] = function
  | Has_many _ ->
      `Has_many
  | Belongs_to _ ->
      `Belongs_to

(* The relation [name] of the schema [s] must be a relation of [kind] to
   [target]. The message names the relation that the schema has. *)
let check kind (s : Schema.model) ~name ~target =
  let describe : Schema.relation -> string * string * string = function
    | Has_many {name; target} ->
        ("has_many", name, target)
    | Belongs_to {name; target; _} ->
        ("belongs_to", name, target)
  in
  let fail fmt =
    Printf.ksprintf
      (fun m ->
        invalid_arg
          (Printf.sprintf "Relation.%s: the model %s %s" kind
             (Cistern_names.quote s.name)
             m ) )
      fmt
  in
  let q = Cistern_names.quote in
  match
    List.find_opt
      (fun r ->
        let _, n, _ = describe r in
        n = name )
      s.relations
  with
  | None ->
      fail "has no relation %s" (q name)
  | Some r ->
      let k, _, t = describe r in
      if k <> kind then fail "has a %s %s to %s, not a %s" k (q name) (q t) kind
      else if t <> target then
        fail "has a %s %s to %s, not to %s" k (q name) (q t) (q target)

let has_many :
       name:string
    -> (module P : MODEL)
       -> (module C : MODEL)
          ->    key:(C.t -> P.key option)
             -> set:(P.t -> C.t list -> P.t)
             -> (P.t, C.t) t =
 fun ~name (module P) (module C) ~key ~set ->
  check "has_many" P.schema ~name ~target:C.schema.name ;
  Has_many {name; parent= (module P); child= (module C); key; set}

let belongs_to :
       name:string
    -> (module C : MODEL)
       -> (module P : MODEL)
          ->    key:(C.t -> P.key option)
             -> set:(C.t -> P.t option -> C.t)
             -> (C.t, P.t) t =
 fun ~name (module C) (module P) ~key ~set ->
  check "belongs_to" C.schema ~name ~target:P.schema.name ;
  Belongs_to {name; child= (module C); parent= (module P); key; set}
