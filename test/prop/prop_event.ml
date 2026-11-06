(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* Event.pp never prints a message of a server (decision Q9). *)

open Windtrap
open Cistern_core

let tests =
  group "Event"
    [ prop "pp never prints the message of an error" ~count:300 Prop_gen.event
        (fun (m, e) ->
          is_false (Prop_gen.contains (Format.asprintf "%a" Event.pp e) m) ) ]
