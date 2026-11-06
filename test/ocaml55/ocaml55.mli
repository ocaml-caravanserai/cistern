(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(** The two OCaml 5.5 features of DESIGN.md, "OCaml 5.5 features", in the forms
    that Cistern uses. The suite test_ocaml55 kills each mutant of this file. *)

module type MODEL = sig
  type t

  type key

  val table : string

  val key : t -> key

  val key_to_string : key -> string
end

val describe : (module M : MODEL) -> M.t -> string
(** [describe (module M) v] is ["<table>/<key>"], or ["<table>"] if the key is
    empty. A module-dependent function: the type of the second argument refers
    to [M]. *)

val count : (module M : MODEL) -> (M.key -> bool) -> M.t list -> int
(** [count (module M) p vs] is the number of values in [vs] whose key satisfies
    [p]. *)

val sum_lengths : ('a. 'a list -> int) -> int list -> string list -> int
(** [sum_lengths f xs ys] is [f xs + f ys]. A polymorphic parameter: the
    function uses [f] at two types. *)
