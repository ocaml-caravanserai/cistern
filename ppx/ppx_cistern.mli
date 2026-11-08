(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(** The deriver [[@@deriving cistern]] (DESIGN.md, "Model definition" and
    "Generated code"). Phase 3a parses and checks a group. Phase 3b generates
    stage 1, and phase 3c generates stages 2 and 3. *)

(** For the tests of the ppx. It can change in each version. *)
module Private : sig
  module Desc = Desc
  module Gen = Gen

  val expand :
       loc:Ppxlib.Location.t
    -> Ppxlib.rec_flag * Ppxlib.type_declaration list
    -> Ppxlib.structure
  (** [expand ~loc (rec_flag, tds)] is the code that the deriver puts after the
      group [tds], after the checks of {!Desc.group}. It raises the located
      error of the first fault. It is {!Gen.all} of the group. *)
end
