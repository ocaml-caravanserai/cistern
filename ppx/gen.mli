(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(** The generated code of a group (DESIGN.md, "Generated code"; the plan of
    phase 3, 3.1 and D7 to D10). *)

val stage1 : Desc.typ list -> Ppxlib.structure
(** [stage1 g] is stage 1 of the group [g]: the default thunks and the alias
    [Cistern__private__<first>] of [Cistern.Private] (item 0),
    [Cistern_fields__<first>] with the lifted value of {!Desc.schema} and the
    typed fields (item 1), [Cistern_drafts__<first>] (item 2), and one
    [Cistern_model__<type>] for each type (item 3). Each item has the warning
    attribute of D13, and the code of each field has the location of its field.
*)

val all : Desc.typ list -> Ppxlib.structure
(** [all g] is the code that comes right after the group [g]: {!stage1}, then
    stage 2 ([Cistern_gen__<first>], with one submodule for each type: [type t],
    [Q], [get_by_*], [find_by_*], [filter_by_*], [all], [where], [update],
    [delete] and [draft]; item 4), then stage 3 (one module for each type, with
    the include of stage 2, [Drafts], [type draft], [model] and [create]; item
    5). *)
