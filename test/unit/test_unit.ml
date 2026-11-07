(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* The unit suite of cistern.core, cistern.names and cistern.mysql. *)

let () =
  exit
    (Windtrap.run "unit"
       [ Test_decimal.tests
       ; Test_value.tests
       ; Test_literal.tests
       ; Test_raw.tests
       ; Test_decoder.tests
       ; Test_error.tests
       ; Test_mapping.tests
       ; Test_event.tests
       ; Test_dialect.tests
       ; Test_names.tests
       ; Test_schema.tests
       ; Test_config.tests
       ; Test_wire.tests
       ; Test_packet.tests
       ; Test_message.tests
       ; Test_resultset.tests
       ; Test_column.tests
       ; Test_outcome.tests
       ; Test_scramble.tests ] )
