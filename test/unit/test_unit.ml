(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* The unit suite of cistern.core, cistern.names and cistern.mysql. *)

let () =
  exit
    (Windtrap.run "unit"
       [ Test_decimal.tests
       ; Test_limits.tests
       ; Test_value.tests
       ; Test_literal.tests
       ; Test_raw.tests
       ; Test_decoder.tests
       ; Test_encode.tests
       ; Test_error.tests
       ; Test_mapping.tests
       ; Test_event.tests
       ; Test_dialect.tests
       ; Test_names.tests
       ; Test_schema.tests
       ; Test_relations.tests
       ; Test_relations.nested
       ; Test_relations.scopes
       ; Test_relations.includes
       ; Test_expr.tests
       ; Test_models.tests
       ; Test_config.tests
       ; Test_tls_setup.tests
       ; Test_wire.tests
       ; Test_packet.tests
       ; Test_message.tests
       ; Test_resultset.tests
       ; Test_column.tests
       ; Test_outcome.tests
       ; Test_pool_state.tests
       ; Test_close_flag.tests
       ; Test_tx_rules.tests
       ; Test_scramble.tests ] )
