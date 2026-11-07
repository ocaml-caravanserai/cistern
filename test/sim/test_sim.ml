(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* The simulator suite of cistern.mysql and of the pool. *)

(* With CISTERN_SLOW set (make sim-slow), the slow tests run too. Without it,
   they are not in the list: a tag filter would make the run partial, and the
   mutation gate would not keep its verdicts. Each test of this suite gives its
   seed to Weft_sim.run, thus each run is the same. *)
let () =
  let slow =
    match Sys.getenv_opt "CISTERN_SLOW" with
    | Some _ ->
        [Test_pool.slow_tests; Test_tx.slow_tests]
    | None ->
        []
  in
  exit
    (Windtrap.run "sim"
       ( [ Test_connect.tests
         ; Test_connect.ping_tests
         ; Test_query.tests
         ; Test_query.pool_tests
         ; Test_pool.tests
         ; Test_tx.tests
         ; Test_tx.storm_tests ]
       @ slow ) )
