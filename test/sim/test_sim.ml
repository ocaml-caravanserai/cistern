(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* The simulator suite of cistern.mysql. *)

let () =
  exit
    (Windtrap.run "sim"
       [Test_connect.tests; Test_connect.ping_tests; Test_query.tests] )
