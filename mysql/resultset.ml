(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

type t =
  {columns: Message.column array; rows: string option array list; ok: Message.ok}

type reply =
  | Ok_packet of Message.ok
  | Err_packet of Message.err
  | Local_infile of string
  | Rows of t

type cause = Packet of Packet.error | Malformed of string

type error = {in_result: bool; cause: cause}

let ( let* ) = Result.bind

let first payload = if payload = "" then -1 else Char.code payload.[0]

(* Read one packet, and give its payload and the next sequence id. *)
let packet r ~seq ~in_result =
  match Packet.read r ~seq with
  | Ok payload ->
      Ok (payload, Packet.next seq)
  | Error e ->
      Error {in_result; cause= Packet e}

let decoded ~in_result = function
  | Ok v ->
      Ok v
  | Error m ->
      Error {in_result; cause= Malformed m}

(* The column count: one length-encoded integer, from 1 to 65535, and no other
   byte. *)
let column_count payload =
  let c = Wire.cursor payload in
  match Wire.lenenc c with
  | Wire.Value n when Wire.remaining c = 0 && n >= 1L && n <= 0xFFFFL ->
      Ok (Int64.to_int n)
  | _ ->
      Error "the column count is not valid"
  | exception (Wire.Truncated | Wire.Malformed _) ->
      Error "the column count is not valid"

let rec columns r ~seq n acc =
  if n = 0 then Ok (Array.of_list (List.rev acc), seq)
  else
    let* payload, seq = packet r ~seq ~in_result:true in
    let* column = decoded ~in_result:true (Message.column payload) in
    columns r ~seq (n - 1) (column :: acc)

let rec rows r ~seq ~columns acc =
  let* payload, seq = packet r ~seq ~in_result:true in
  match first payload with
  | 0xFE ->
      let* ok = decoded ~in_result:true (Message.terminator payload) in
      Ok (Rows {columns; rows= List.rev acc; ok}, seq)
  | 0xFF ->
      let* e = decoded ~in_result:true (Message.err payload) in
      Ok (Err_packet e, seq)
  | _ ->
      let* row =
        decoded ~in_result:true
          (Message.row ~columns:(Array.length columns) payload)
      in
      rows r ~seq ~columns (row :: acc)

let read r ~seq =
  let* payload, seq = packet r ~seq ~in_result:false in
  match first payload with
  | 0x00 ->
      let* ok = decoded ~in_result:false (Message.ok payload) in
      Ok (Ok_packet ok, seq)
  | 0xFF ->
      let* e = decoded ~in_result:false (Message.err payload) in
      Ok (Err_packet e, seq)
  | 0xFB ->
      Ok (Local_infile (String.sub payload 1 (String.length payload - 1)), seq)
  | _ ->
      let* n = decoded ~in_result:false (column_count payload) in
      let* columns, seq = columns r ~seq n [] in
      rows r ~seq ~columns []
