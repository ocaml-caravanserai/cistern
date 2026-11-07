(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* The bytes that a function writes to a Weft_flow.Writer, with an in-memory
   flow (no fiber). *)

module Writer = Weft_flow.Writer

(* [written_with create f]: the bytes that [f] writes to the writer that
   [create] makes. *)
let written_with create f =
  let out = Buffer.create 64 in
  let w = create (Weft_flow.Flow.of_string "" out) in
  f w ; Writer.flush w ; Buffer.contents out

let written f = written_with (fun flow -> Writer.create flow) f
