(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* The models of the other examples: an author has posts. *)

(* README begin: models *)
type author =
  { id: int64 [@key] [@auto]
  ; name: string
  ; email: string [@unique]
  ; posts: post list Cistern.Deferred.t [@has_many] }

and post =
  { id: int64 [@key] [@auto]
  ; author_id: int64
  ; author: author Cistern.Deferred.t
        [@belongs_to {key= author_id; references= id}]
  ; title: string [@index]
  ; views: int [@default 0]
  ; published: bool [@default false] }
[@@deriving cistern]

let models = [Author.model; Post.model]
(* README end *)
