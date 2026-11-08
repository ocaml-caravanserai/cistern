(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(** Typed filters (DESIGN.md, "Core user types" and "Filters"). The type is
    abstract, thus a filter of one model cannot type as a filter of another
    model.

    Each filter is two-valued: on each row it is true or false, never SQL
    [NULL]. An atom on a nullable field that SQL can make [NULL] gets the guard
    [IS NOT NULL AND]. Thus [not p] selects the rows that [p] does not select,
    and [&&], [||] and [not] act as the booleans of OCaml. *)

type ('m, 'a) t

val ast : ('m, bool) t -> Model_ast.t
(** The model AST of the filter. [cistern.sql] compiles it. *)

val and_ : ('m, bool) t list -> ('m, bool) t
(** All the filters of the list. [and_ []] is [TRUE]. *)

val or_ : ('m, bool) t list -> ('m, bool) t
(** One of the filters of the list. [or_ []] is [FALSE]. *)

(** The operators of [X.Q] (DESIGN.md, "Generated code"). On a nullable field
    [f], with the column [c]:

    - [f ==. Some v] is [c <=> v], and [f ==. None] is [c IS NULL].
    - [f <>. Some v] is [NOT (c <=> v)], and [f <>. None] is [c IS NOT NULL].
    - [f <. Some v] is [(c IS NOT NULL AND c < v)], and the same for [>.], [<=.]
      and [>=.].
    - [in_ f vs] is [(c IS NOT NULL AND c IN (...))] if [vs] has no [None],
      [(c IN (...) OR c IS NULL)] if [vs] has [None] and other values, and
      [c IS NULL] if [vs] has only [None].
    - [starts_with_opt] and [contains_opt] are
      [(c IS NOT NULL AND c LIKE <pattern> ESCAPE '!')].

    On a field that is not nullable, [==.] is [=], [<>.] is [<>], and the atoms
    have no guard. [in_ f []] is [FALSE] on each field. *)
module type OPS = sig
  val ( ==. ) : ('m, 'a) Field.t -> 'a -> ('m, bool) t
  (** [f ==. v]: the value of [f] is [v], with the equality of OCaml, also for
      [None] ([f = v], or [f <=> v] on a nullable field, or [IS NULL] for
      [None]). *)

  val ( <>. ) : ('m, 'a) Field.t -> 'a -> ('m, bool) t
  (** [f <>. v] is [not (f ==. v)]. *)

  val ( <. ) : ('m, 'a) Field.t -> 'a -> ('m, bool) t
  (** It raises [Invalid_argument] for [None]. The same for [>.], [<=.] and
      [>=.]. *)

  val ( >. ) : ('m, 'a) Field.t -> 'a -> ('m, bool) t
  (** The order of OCaml. An order never selects [None]. *)

  val ( <=. ) : ('m, 'a) Field.t -> 'a -> ('m, bool) t
  (** The order of OCaml. An order never selects [None]. *)

  val ( >=. ) : ('m, 'a) Field.t -> 'a -> ('m, bool) t
  (** The order of OCaml. An order never selects [None]. *)

  val is_null : ('m, 'a option) Field.t -> ('m, bool) t
  (** [is_null f] is [f ==. None]. *)

  val in_ : ('m, 'a) Field.t -> 'a list -> ('m, bool) t
  (** [in_ f vs]: the value of [f] is one of [vs], with the equality of
      {!( ==. )}. [in_ f []] is [FALSE]. *)

  val starts_with : ('m, string) Field.t -> string -> ('m, bool) t
  (** [starts_with f s]: the value starts with [s]. The pattern escapes [!], [%]
      and [_] with [!], then adds [%] at the end. The column has a binary
      collation, thus the match is case-sensitive. *)

  val contains : ('m, string) Field.t -> string -> ('m, bool) t
  (** [contains f s]: the value contains [s]. As {!starts_with}, with [%] at the
      two ends. *)

  val starts_with_opt : ('m, string option) Field.t -> string -> ('m, bool) t
  (** {!starts_with} on a nullable field. A [NULL] row does not match. *)

  val contains_opt : ('m, string option) Field.t -> string -> ('m, bool) t
  (** {!contains} on a nullable field. A [NULL] row does not match. *)

  val ( && ) : ('m, bool) t -> ('m, bool) t -> ('m, bool) t
  (** The two filters. Each filter is true or false, never [NULL]. *)

  val ( || ) : ('m, bool) t -> ('m, bool) t -> ('m, bool) t
  (** One of the two filters. *)

  val not : ('m, bool) t -> ('m, bool) t
  (** [not p] selects each row that [p] does not select. *)
end

module Ops : OPS
