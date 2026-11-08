(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

open Ppxlib

module Private = struct
  module Desc = Desc

  (* Phase 3a: the checks only. *)
  let expand ~loc (_rec_flag, tds) =
    ignore (Desc.group ~loc tds : Desc.typ list) ;
    []
end

let generate ~ctxt group =
  Private.expand ~loc:(Expansion_context.Deriver.derived_item_loc ctxt) group

let () =
  ignore
    ( Deriving.add "cistern"
        ~str_type_decl:(Deriving.Generator.V2.make_noarg generate)
      : Deriving.t )
