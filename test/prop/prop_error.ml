(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* The printer of Error for logs: pp_summary never prints a message. *)

open Windtrap
open Cistern_core

let summary e = Format.asprintf "%a" Error.pp_summary e

let tests =
  group "Error"
    [ prop "pp_summary never prints the message" ~count:300 Prop_gen.error
        (fun (m, e) -> is_false (Prop_gen.contains (summary e) m) ) ]
