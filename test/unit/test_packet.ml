(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* Packet: the frame, the sequence ids (with the rule R-SEQ0), the limit of 16
   MiB, and the size rule of COM_QUERY. *)

open Windtrap
module P = Cistern_mysql.Packet

let frame seq payload =
  let n = String.length payload in
  String.init 3 (fun i -> Char.chr ((n lsr (8 * i)) land 0xFF))
  ^ String.make 1 (Char.chr seq)
  ^ payload

let reader input = P.reader (Weft_flow.Flow.of_string input (Buffer.create 1))

let show = function
  | Ok p ->
      "ok " ^ String.escaped p
  | Error (P.Sequence {expected; got}) ->
      Printf.sprintf "sequence %d %d" expected got
  | Error (Unsolicited p) ->
      "unsolicited " ^ String.escaped p
  | Error Too_long ->
      "too long"

let read r ~seq = show (P.read r ~seq)

let tests =
  group "Packet"
    [ test "read gives the payload and consumes the packet" (fun () ->
          let r = reader (frame 1 "abc" ^ frame 2 "" ^ frame 3 "d") in
          equal string "ok abc" (read r ~seq:1) ;
          equal string "ok " (read r ~seq:2) ;
          equal string "ok d" (read r ~seq:3) )
    ; test "a wrong sequence id, then the next packet" (fun () ->
          let r = reader (frame 5 "abc" ^ frame 6 "d") in
          equal string "sequence 1 5" (read r ~seq:1) ;
          equal string "ok d" (read r ~seq:6) )
    ; test "R-SEQ0: an ERR with id 0 in place of id 1" (fun () ->
          let err = "\xff\xbf\x0f#HY000timeout" in
          let r = reader (frame 0 err) in
          equal string ("unsolicited " ^ String.escaped err) (read r ~seq:1) ;
          equal string "unsolicited \\255"
            (read (reader (frame 0 "\xff")) ~seq:1) )
    ; test "R-SEQ0 is only for the first packet of a reply" (fun () ->
          let err = "\xff\xbf\x0f#HY000timeout" in
          equal string "sequence 2 0" (read (reader (frame 0 err)) ~seq:2) ;
          equal string "sequence 5 0" (read (reader (frame 0 err)) ~seq:5) )
    ; test "id 0 that is not an ERR, and an empty payload with id 0" (fun () ->
          equal string "sequence 1 0" (read (reader (frame 0 "\x00ok")) ~seq:1) ;
          equal string "sequence 1 0" (read (reader (frame 0 "")) ~seq:1) )
    ; test "id 0 where 0 is expected" (fun () ->
          equal string "ok \\255x" (read (reader (frame 0 "\xffx")) ~seq:0) )
    ; test "a length of 0xFFFFFF" (fun () ->
          equal string "too long" (read (reader "\xff\xff\xff\x01") ~seq:1) )
    ; test "the end of the stream" (fun () ->
          raises End_of_file (fun () -> P.read (reader "") ~seq:0) ;
          raises End_of_file (fun () ->
              P.read (reader "\x05\x00\x00\x00ab") ~seq:0 ) )
    ; test "the largest payload" (fun () ->
          let payload = String.make 0xFFFFFE 'x' in
          match P.read (reader (frame 0 payload)) ~seq:0 with
          | Ok p ->
              equal int 0xFFFFFE (String.length p)
          | Error _ ->
              fail "an error" )
    ; test "next" (fun () ->
          equal int 1 (P.next 0) ;
          equal int 255 (P.next 254) ;
          equal int 0 (P.next 255) )
    ; test "write" (fun () ->
          let out =
            Wire_io.written_with P.writer (fun w ->
                P.write w ~seq:3 (fun w ->
                    Weft_flow.Writer.write_string w "abc" ) ;
                P.write w ~seq:255 (fun _ -> ()) )
          in
          equal string (frame 3 "abc" ^ frame 255 "") out )
    ; test "a body that raises drops its packet" (fun () ->
          let out =
            Wire_io.written_with P.writer (fun w ->
                P.write w ~seq:1 (fun w -> Weft_flow.Writer.write_string w "a") ;
                ( try
                    P.write w ~seq:2 (fun w ->
                        Weft_flow.Writer.write_string w "lost" ;
                        failwith "body" )
                  with Failure _ -> () ) ;
                P.write w ~seq:3 (fun w -> Weft_flow.Writer.write_string w "b") )
          in
          equal string (frame 1 "a" ^ frame 3 "b") out )
    ; test "the largest payload that a writer takes" (fun () ->
          let out =
            Wire_io.written_with P.writer (fun w ->
                P.write w ~seq:0 (fun w ->
                    Weft_flow.Writer.write_string w (String.make 0xFFFFFE 'x') ) )
          in
          equal int (0xFFFFFE + 4) (String.length out) ;
          raises Weft_flow.Writer.Too_large (fun () ->
              Wire_io.written_with P.writer (fun w ->
                  P.write w ~seq:0 (fun w ->
                      Weft_flow.Writer.write_string w (String.make 0xFFFFFF 'x') ) ) ) )
    ; cases "query_fits"
        ~id:(fun (m, n, _) -> Printf.sprintf "%d %d" m n)
        [ (64 * 1024 * 1024, 0xFFFFFD, true)
        ; (64 * 1024 * 1024, 0xFFFFFE, false)
        ; (1048576, 1048574, true)
        ; (1048576, 1048575, false)
        ; (0xFFFFFF, 0xFFFFFD, true)
        ; (0xFFFFFF, 0xFFFFFE, false)
        ; (16, 0, true) ]
        (fun (max_allowed_packet, sql_length, expected) ->
          equal bool expected (P.query_fits ~max_allowed_packet ~sql_length) )
    ]
