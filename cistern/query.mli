(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(** The queries of the typed API (DESIGN.md, "Core user types"). A query holds
    the filters, the order, the limit, the offset and the lock of a model. Its
    finishers give statements for [Db.exec] or [Tx.exec]. The type is abstract:
    a filter or a field of one model does not type with a query of another
    model. *)

open Cistern_core

type 'm t

val where : ('m, bool) Expr.t -> 'm t -> 'm t
(** [where e q] is [q] with the filter [e], joined with [AND] to the filters of
    [q]. *)

val order_by : ('m, _) Field.t -> [`Asc | `Desc] -> 'm t -> 'm t
(** [order_by f dir q] adds [f] as the last key of the order. With a limit or an
    offset, the key of the model goes after the keys of the order. *)

val limit : int -> 'm t -> 'm t
(** @raise Invalid_argument for a negative limit. *)

val offset : int -> 'm t -> 'm t
(** Without a limit: [LIMIT 18446744073709551615].

    @raise Invalid_argument for a negative offset. *)

val for_update : 'm t -> 'm t
(** [FOR UPDATE]: only in a [Tx]. [Db.exec] raises [Invalid_argument] for a
    statement of it, before any event, also after [Stmt.map]. *)

val for_share : 'm t -> 'm t
(** [FOR SHARE] on MySQL, [LOCK IN SHARE MODE] on MariaDB. [Db.exec] permits it:
    the lock then ends with the statement. *)

val include_ : ('m, _) Relation.t -> 'm t -> 'm t
(** [include_ rel q] loads the relation [rel] of the rows of [q] (phase 4d, the
    plan of phase 4, E8). After the main query of {!to_list} or {!first}, one
    more [SELECT] with [IN (...)] of the keys of the rows, unique on the client,
    in chunks of 1000 keys. No key gives no query. Each query of [include_]
    takes the lock of [q] ([FOR UPDATE], or the shared lock of the dialect).

    - A has_many: the children of the keys, in the order of the key of the
      child, grouped by their key (the column of the one belongs_to of the child
      to the parent). A row with no child gets [Loaded []].
    - A belongs_to: the parent of each key. A nullable key that is [None] gives
      the setter [None] ([Loaded None]). A key whose parent row is gone (another
      session deleted it) leaves the relation [Unloaded], with no error.

    The keys are compared with {!Value.equal}. [include_] never fills the
    inverse relation. Each command emits its own event ([?] for each key), with
    no event of the statement ({!Stmt.Private.meta}). {!Stmt.to_sql} shows the
    keys as [<keys>]. Two [include_] give two more queries, in order. {!count}
    and {!exists} ignore [include_]. *)

val to_list : 'm t -> 'm list Stmt.t
(** The rows, in the order of the server. *)

val first : 'm t -> 'm option Stmt.t
(** The first row: [LIMIT 1] (or [LIMIT 0] for a query with it). *)

val count : 'm t -> int Stmt.t
(** The count of the rows: a derived table for a query with a limit or an
    offset. It ignores the lock. *)

val exists : 'm t -> bool Stmt.t
(** It ignores the lock. *)

val update : 'm Update.t -> 'm t -> int Stmt.t
(** [update u q] sets the fields of [u] in the rows of [q], with the order and
    the limit of [q], and gives the matched rows. With no filter, it changes the
    whole table.

    @raise Invalid_argument
      when the statement is made: if [u] sets the key or one column two times,
      or if [q] has an offset, [for_update], [for_share] or [include_]. *)

val delete : 'm t -> int Stmt.t
(** [delete q] deletes the rows of [q], with the order and the limit of [q], and
    gives the count of the rows. With no filter, it deletes all the rows.

    @raise Invalid_argument
      when the statement is made: if [q] has an offset, [for_update],
      [for_share] or [include_]. *)

(** For the library ([Model.all]). *)
module Private : sig
  val make : Schema.model -> (Decoder.cursor -> 'm) -> 'm t
  (** [make m decode] is the query of all the rows of [m]. *)

  val unique : Value.t list -> Value.t list
  (** [unique l] is the keys of {!include_}: the values of [l] once each
      ({!Value.equal}), in the order of their first place. For the tests. *)
end
