(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

module Writer = Weft_flow.Writer

exception Truncated

exception Malformed of string

type cursor = {payload: string; mutable pos: int}

let peer_text ~limit s =
  let printable c = c >= ' ' && c <= '~' && c <> '"' && c <> '\\' in
  let n = String.length s in
  if n >= 1 && n <= limit && String.for_all printable s then s
  else
    let b = Buffer.create 64 in
    Buffer.add_char b '"' ;
    String.iter
      (fun c ->
        if printable c then Buffer.add_char b c
        else Printf.bprintf b "\\x%02x" (Char.code c) )
      (String.sub s 0 (Int.min n limit)) ;
    Buffer.add_char b '"' ;
    if n > limit then Buffer.add_string b "..." ;
    Buffer.contents b

let cursor payload = {payload; pos= 0}

let remaining c = String.length c.payload - c.pos

(* Take [n] bytes: the position of the first one. *)
let take c n =
  if n < 0 || n > remaining c then raise Truncated ;
  let p = c.pos in
  c.pos <- p + n ;
  p

let uint8 c = String.get_uint8 c.payload (take c 1)

let uint16 c = String.get_uint16_le c.payload (take c 2)

let uint24 c =
  let p = take c 3 in
  String.get_uint16_le c.payload p
  lor (String.get_uint8 c.payload (p + 2) lsl 16)

let uint32 c =
  Int32.to_int (String.get_int32_le c.payload (take c 4)) land 0xFFFF_FFFF

let int64 c = String.get_int64_le c.payload (take c 8)

let bytes c n = String.sub c.payload (take c n) n

let skip c n = ignore (take c n)

let rest c = bytes c (remaining c)

let string_nul c =
  match String.index_from_opt c.payload c.pos '\000' with
  | None ->
      raise Truncated
  | Some i ->
      let s = bytes c (i - c.pos) in
      skip c 1 ; s

type lenenc = Value of int64 | Null

let lenenc c =
  match uint8 c with
  | n when n < 0xFB ->
      Value (Int64.of_int n)
  | 0xFB ->
      Null
  | 0xFC ->
      Value (Int64.of_int (uint16 c))
  | 0xFD ->
      Value (Int64.of_int (uint24 c))
  | 0xFE ->
      Value (int64 c)
  | _ ->
      raise (Malformed "a length-encoded integer starts with 0xFF")

let lenenc_string c =
  match lenenc c with
  | Null ->
      None
  | Value n ->
      (* A negative [n] is a length of 2^63 or more. Int64.min_int would give
         the int 0, thus it needs its own check. Another length above the
         payload gives a negative int or one above [remaining], and [take]
         refuses it. *)
      if Int64.compare n 0L < 0 then raise Truncated
      else Some (bytes c (Int64.to_int n))

let write_lenenc w n =
  let below limit = Int64.unsigned_compare n limit < 0 in
  if below 0xFBL then Writer.write_uint8 w (Int64.to_int n)
  else if below 0x1_0000L then (
    Writer.write_uint8 w 0xFC ;
    Writer.write_uint16_le w (Int64.to_int n) )
  else if below 0x100_0000L then (
    Writer.write_uint8 w 0xFD ;
    Writer.write_uint24_le w (Int64.to_int n) )
  else (Writer.write_uint8 w 0xFE ; Writer.write_int64_le w n)

let write_lenenc_string w s =
  write_lenenc w (Int64.of_int (String.length s)) ;
  Writer.write_string w s

let write_string_nul w s =
  if String.contains s '\000' then invalid_arg "Wire.write_string_nul: a NUL" ;
  Writer.write_string w s ;
  Writer.write_uint8 w 0
