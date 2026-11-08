(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(** The description of the models: {!Cistern_schema.Schema}, in the library
    [cistern.schema] since phase 3a0. The path [Cistern_core.Schema] stays, so
    that no user path changes. *)

include module type of struct
  include Cistern_schema.Schema
end
