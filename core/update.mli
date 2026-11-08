(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(** The changes of an update (DESIGN.md, "Core user types"):
    [Model.update : (module M : MODEL) -> M.t Update.t list -> M.t -> M.t
     Stmt.t] and [Query.update : 'm Update.t -> 'm Query.t -> int Stmt.t]. *)

type 'm t

val set : ('m, 'a) Field.t -> 'a -> 'm t
(** [set f v] sets the field [f] to [v]. *)

val ( & ) : 'm t -> 'm t -> 'm t
(** [a & b] makes the changes of [a], then the changes of [b]. *)

val assignments : 'm t -> (string * Value.t) list
(** [assignments u] is the column and the value of each change, in order. The
    statement of an update calls it when it is made. A column occurs only one
    time, thus the order of the assignments on the server does not matter.

    It raises [Invalid_argument] if [u] sets the key, or sets one column two
    times. *)

val apply : 'm t -> 'm -> 'm
(** [apply u r] is [r] with the changes of [u], made with {!Field.set}.

    It raises [Invalid_argument] as {!assignments}: if [u] sets the key, or sets
    one column two times. *)
