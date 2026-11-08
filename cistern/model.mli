(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(** The generic operations of one row on a model module (DESIGN.md, "Generated
    code" and "OCaml 5.5 features"). The generated code (phase 3) and the
    hand-written models of phase 2 call them through [Cistern.Private.Model].
    Each one gives a statement for [Db.exec] or [Tx.exec]. *)

open Cistern_core

module type MODEL = Model_intf.MODEL

val get : (module M : MODEL) -> M.key -> M.t Stmt.t
(** [get (module M) k] is the row with the key [k]: [Not_found] if none. A read.
*)

val find : (module M : MODEL) -> M.key -> M.t option Stmt.t
(** [find (module M) k] is [get] with [None] for no row. *)

val get_parent : (module M : MODEL) -> M.key option -> M.t option Stmt.t
(** [get_parent (module M) k] is the scope of a belongs_to with a nullable key
    (phase 4c2, the plan of phase 4, E11). [None] gives [None] with no checkout,
    no command and no event ({!Stmt.Private.pure}). [Some k] gives [get] of [k]
    as [Some], and [Not_found] if the row is gone. A key that is not nullable
    uses {!get}. *)

val get_by : (module M : MODEL) -> (M.t, 'a) Field.t -> 'a -> M.t Stmt.t
(** [get_by (module M) f v] is the row with [v] in the field [f], a field with
    [[@unique]]: [Not_found] if none. The generated code makes [get_by_<field>]
    and [find_by_<field>] only for [[@unique]] fields: for another field, the
    server chooses one of the rows ([LIMIT 1] with no [ORDER BY]). The generated
    [get_by_<field>] of a nullable field takes the value with no [option] and
    gives [Some v] here.

    It raises [Invalid_argument] if the value of [v] is [Null]. *)

val find_by : (module M : MODEL) -> (M.t, 'a) Field.t -> 'a -> M.t option Stmt.t
(** [find_by] is [get_by] with [None] for no row. *)

val all : (module M : MODEL) -> M.t Query.t
(** [all (module M)] is the query of all the rows of [M]. *)

val where : (module M : MODEL) -> (M.t, bool) Expr.t -> M.t Query.t
(** [where (module M) e] is [Query.where e (all (module M))]. The generated
    [filter_by_<field>] of an [[@index]] field is [where] with [==.]. *)

(** The children of one has_many of a nested create: the drafts of the child
    model, the setter of the key of the parent in a child draft, and the setter
    of the relation field of the parent (phase 4c1, the plan of phase 4, E10).
*)
type ('p, 'k) child

val child :
  (module C : MODEL)
  ->    fk:(C.draft -> 'k -> C.draft)
     -> set:('p -> C.t list -> 'p)
     -> C.draft list
     -> ('p, 'k) child
(** [child (module C) ~fk ~set ds] is the children [ds] of a parent with the key
    type ['k]. [fk d k] is the draft [d] with the key [k] of the parent, and
    [set p cs] the parent [p] with its children [cs] ([Loaded]). *)

val create :
  (module M : MODEL)
  -> ?children:(M.t, M.key) child list -> M.draft -> M.t Stmt.t
(** [create (module M) d] inserts the row of [d], and gives it with the key: for
    an [[@auto]] key, the last insert id ([Decode] if it is 0), else the key of
    [d]. No [SELECT] after it.

    With [children] (a nested create of one level, phase 4c1): the parent
    [INSERT], then one [INSERT] for each child, in the order of the list, with
    the key of the parent through [fk]. The statement is atomic: in [Db.exec] it
    runs in the transaction of [Db.transaction], in [Tx.exec] in a savepoint
    ([Stmt.Private.meta]). The result is the parent with its children through
    [set], and each child comes from its [of_draft] (its relations [Unloaded]).
    Each group gives [Loaded] children, also an empty group ([Loaded []]); a
    relation with no group stays [Unloaded]. No child (no group, or only empty
    groups) gives one [INSERT], which is not atomic. {!Stmt.to_sql} shows the
    key of an [[@auto]] parent in a child as [<last_insert_id>]. *)

val update : (module M : MODEL) -> M.t Update.t list -> M.t -> M.t Stmt.t
(** [update (module M) us r] sets the fields of [us] in the row of the key of
    [r], and gives [r] with the new values ({!Update.apply}). The list is joined
    with [&] first. No matched row gives [Not_found]. An empty list gives [r]
    with no checkout, no command and no event ({!Stmt.Private.pure}).

    It raises [Invalid_argument] if [us] sets the key, or sets one column two
    times, also across the list. *)

val delete : (module M : MODEL) -> M.t -> unit Stmt.t
(** [delete (module M) r] deletes the row of the key of [r]. No row gives
    [Not_found]. *)
