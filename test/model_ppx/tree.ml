(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* The tree of node through the ppx (phase 4h): the type declaration of
   test/model/tree.ml. *)

type node =
  { id: int64 [@key] [@auto]
  ; name: string
  ; parent_id: int64 option
  ; parent: node option Cistern.Deferred.t
        [@belongs_to {key= parent_id; references= id}]
  ; children: node list Cistern.Deferred.t [@has_many] }
[@@deriving cistern]
