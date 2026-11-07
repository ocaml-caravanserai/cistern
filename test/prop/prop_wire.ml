(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* Wire: a written length-encoded value reads back, near each boundary of its
   forms, and a reader on any bytes raises only Truncated or Malformed. *)

open Windtrap
module W = Cistern_mysql.Wire
module Writer = Weft_flow.Writer

let written f =
  let out = Buffer.create 64 in
  let w = Writer.create (Weft_flow.Flow.of_string "" out) in
  f w ; Writer.flush w ; Buffer.contents out

(* Values near 251, 2^16, 2^24, 2^63 and 2^64, and any value. *)
let lenenc_value =
  let near b =
    Gen.map (fun d -> Int64.add b (Int64.of_int d)) (Gen.int_range (-3) 3)
  in
  Gen.one_of
    [ near 251L
    ; near 0x1_0000L
    ; near 0x100_0000L
    ; near Int64.max_int
    ; near (-1L)
    ; Gen.int64
    ; Gen.map Int64.of_int (Gen.int_range 0 300) ]

let only_wire_exceptions f =
  match f () with _ -> true | exception (W.Truncated | W.Malformed _) -> true

let tests =
  group "Wire"
    [ prop "lenenc round trip" ~count:2000 lenenc_value (fun n ->
          let k = W.cursor (written (fun w -> W.write_lenenc w n)) in
          equal bool true (W.lenenc k = W.Value n) ;
          equal int 0 (W.remaining k) )
    ; prop "lenenc_string round trip" ~count:500
        (Gen.string_of ~size:(Gen.int_range 0 300) Gen.char)
        (fun s ->
          let k = W.cursor (written (fun w -> W.write_lenenc_string w s)) in
          equal (option string) (Some s) (W.lenenc_string k) ;
          equal int 0 (W.remaining k) )
    ; prop "readers on any bytes raise only Truncated or Malformed" ~count:2000
        Gen.string (fun s ->
          is_true
            (List.for_all only_wire_exceptions
               [ (fun () -> ignore (W.lenenc (W.cursor s)))
               ; (fun () -> ignore (W.lenenc_string (W.cursor s)))
               ; (fun () -> ignore (W.string_nul (W.cursor s)))
               ; (fun () -> ignore (W.uint32 (W.cursor s)))
               ; (fun () -> ignore (W.int64 (W.cursor s))) ] ) ) ]
