(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* The expect suite of cistern.sql. *)

let () = exit (Windtrap.run "expect" [Expect_sql.tests; Prop_redacted.tests])
