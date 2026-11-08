(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* The type-error tests: the message of the compiler for each file that it must
   refuse. The message names the two types. *)

open Windtrap

let files =
  [ "mix_expr"
  ; "mix_update"
  ; "starts_with_int"
  ; "starts_with_option"
  ; "starts_with_opt_string" ]

let read path = In_channel.with_open_bin path In_channel.input_all

let () =
  exit
    (Windtrap.run "typeerr"
       [ cases "the compiler refuses" ~id:Fun.id files (fun f ->
             expect_file
               ~file:("test/typeerr/" ^ f ^ ".expected")
               (read (f ^ ".out")) ) ] )
