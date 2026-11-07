(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

module Value = Cistern_core.Value
module Decoder = Cistern_core.Decoder
module Decimal = Cistern_core.Decimal

let binary = 63

let unsigned_flag = 0x20

(* The decoder of a value that is not NULL. *)
let value (c : Message.column) =
  let int = Decoder.map (fun i -> Value.Int i) Decoder.int64
  and text = Decoder.map (fun s -> Value.Text s) Decoder.string
  and bytes =
    Decoder.map (fun b -> Value.Bytes (Bytes.to_string b)) Decoder.bytes
  in
  match c.typ with
  | 1 | 2 | 3 | 9 | 13 ->
      int
  | 8 when c.flags land unsigned_flag = 0 ->
      int
  | 8 ->
      (* An integer above Int64.max_int has no int64. *)
      Decoder.map
        (fun d ->
          match Int64.of_string_opt (Decimal.to_string d) with
          | Some i ->
              Value.Int i
          | None ->
              Value.Decimal d )
        Decoder.decimal
  | 4 | 5 ->
      Decoder.map (fun f -> Value.Float f) Decoder.float
  | 0 | 246 ->
      Decoder.map (fun d -> Value.Decimal d) Decoder.decimal
  | 7 | 12 ->
      Decoder.map (fun t -> Value.Datetime t) Decoder.ptime
  | 10 | 11 | 14 | 245 ->
      text
  | 16 | 255 ->
      bytes
  | _ ->
      if c.charset = binary then bytes else text

let decoder (c : Message.column) =
  if c.typ = 6 then
    Decoder.map (fun _ -> Value.Null) (Decoder.option Decoder.bytes)
  else
    Decoder.map
      (function None -> Value.Null | Some v -> v)
      (Decoder.option (value c))

let row columns values =
  Decoder.run
    (fun cur -> Array.map (fun c -> Decoder.next cur (decoder c)) columns)
    values
