(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* The property suite of cistern.core, cistern.names and cistern.mysql. *)

let () =
  exit
    (Windtrap.run "prop"
       [ Prop_decimal.tests
       ; Prop_literal.tests
       ; Prop_decoder.tests
       ; Prop_error.tests
       ; Prop_mapping.tests
       ; Prop_event.tests
       ; Prop_names.tests
       ; Prop_config.tests
       ; Prop_wire.tests
       ; Prop_message.tests ] )
