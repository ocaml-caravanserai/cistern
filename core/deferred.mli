(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(** The value of a relation field (DESIGN.md, "Deferred values"; the plan of
    phase 4, E1): immutable, with no closure, so that [=] and printing work. A
    query gives [Unloaded], unless [Query.include_] loads the relation; [create]
    gives [Loaded] children. *)

type 'a t = Unloaded | Loaded of 'a

val get : 'a t -> 'a option
(** [get (Loaded v)] is [Some v], and [get Unloaded] is [None]. *)

val get_exn : 'a t -> 'a
(** [get_exn (Loaded v)] is [v].

    It raises [Invalid_argument] for [Unloaded]. *)
