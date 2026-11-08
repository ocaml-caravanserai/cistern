(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* The hostile relation group (phase 4g, the plan of phase 4, section 4.9): user
   modules List, Option, Fun and Deferred before the group, and a user
   constructor Some; relation names c, r, k, fields and decode; a child k with
   two belongs_to of two parents, one with a nullable key; a self relation as a
   tree; a relation field c of a and a column c of k (the case of C-M3); and
   [@column] on the key of a parent and on the key of a child, so that include_
   uses the column (the notes of the review of 4d). test/ppx runs the scopes,
   each Rel with include_, and each label of create. *)

module List = struct
  let filter_map = ()
end

module Option = struct
  let map = ()
end

module Fun = struct
  let id = ()
end

type hostile_option = Some of int | None

(* The types of the group share labels (id, c, r, fields), which the program
   permits for its own types. *)
[@@@ocaml.warning "-30"]

type a =
  { id: int64 [@key] [@auto] [@column "a id"]
  ; fields: int
  ; c: k list Cistern.Deferred.t [@has_many] }

(* The key of b has a name of its own: the scope of r takes the key of b, not
   the key of k. *)
and b = {bk: int64 [@key] [@auto]; r: k list Cistern.Deferred.t [@has_many]}

and k =
  { id: int64 [@key] [@auto]
  ; a_id: int64 [@column "a key"]
  ; fields: a Cistern.Deferred.t [@belongs_to {key= a_id; references= id}]
  ; b_id: int64 option
  ; decode: b option Cistern.Deferred.t
        [@belongs_to {key= b_id; references= bk}]
  ; c: string }

and n =
  { id: int64 [@key] [@auto]
  ; up: int64 option
  ; k: n option Cistern.Deferred.t [@belongs_to {key= up; references= id}]
  ; r: n list Cistern.Deferred.t [@has_many] }

(* Two has_many of p to one child type q (the review of 4g, L1). *)
and p =
  { id: int64 [@key] [@auto]
  ; x1: q list Cistern.Deferred.t [@has_many]
  ; x2: q list Cistern.Deferred.t [@has_many] }

and q =
  { id: int64 [@key] [@auto]
  ; p_id: int64
  ; p: p Cistern.Deferred.t [@belongs_to {key= p_id; references= id}]
  ; v: int }

(* A parent with a key that is not [@auto], its own belongs_to with a key that
   is not nullable, and children (the review of 4g, L3). *)
and team =
  { code: string [@key]
  ; o_id: int64
  ; org: p Cistern.Deferred.t [@belongs_to {key= o_id; references= id}]
  ; members: member list Cistern.Deferred.t [@has_many] }

and member =
  { id: int64 [@key] [@auto]
  ; team_code: string
  ; team: team Cistern.Deferred.t
        [@belongs_to {key= team_code; references= code}] }
[@@deriving cistern]

(* The user constructor Some stays the one of the program. *)
let _some : hostile_option = Some 1
