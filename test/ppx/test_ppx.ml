(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* The suite of ppx_cistern. *)

let () = exit (Windtrap.run "ppx" [Desc_tests.tests; Error_tests.tests])
