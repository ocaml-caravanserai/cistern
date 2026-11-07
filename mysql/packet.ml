(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

module Reader = Weft_flow.Reader
module Writer = Weft_flow.Writer

let reader flow = Reader.create flow

(* The largest payload is 0xFFFFFE bytes, and the frame has 4 more bytes of
   header. The constants are inside functions, so that the mutation gate
   evaluates them in the tests. *)
let writer flow = Writer.create ~max_frame:(0xFFFFFE + 4) flow

let next seq = (seq + 1) land 0xFF

type error =
  | Sequence of {expected: int; got: int}
  | Unsolicited of string
  | Too_long

let read r ~seq =
  (* The header first, then the payload: an ensure of 4 + len bytes would raise
     Reader.Too_large for a payload near 16 MiB. *)
  Reader.ensure r 4 ;
  let len = Reader.get_uint24_le r 0 and got = Reader.get_uint8 r 3 in
  Reader.consume r 4 ;
  if len = 0xFFFFFF then Error Too_long
  else (
    Reader.ensure r len ;
    let payload = Reader.sub_string r 0 len in
    Reader.consume r len ;
    if got = seq then Ok payload
    else if seq = 1 && got = 0 && len > 0 && payload.[0] = '\xff' then
      Error (Unsolicited payload)
    else Error (Sequence {expected= seq; got}) )

let write w ~seq body =
  Writer.start_frame w ~header:4 ;
  match body w with
  | () ->
      let n = Writer.end_frame w in
      Writer.write_uint24_le w n ; Writer.write_uint8 w seq
  | exception e ->
      Writer.abort_frame w ; raise e

let query_fits ~max_allowed_packet ~sql_length =
  1 + sql_length < min 0xFFFFFF max_allowed_packet
