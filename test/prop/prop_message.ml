(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* Message: no decoder raises, on any bytes and on each prefix of a valid
   packet; and a test decoder reads back each field of a handshake response. *)

open Windtrap
module M = Cistern_mysql.Message
module W = Cistern_mysql.Wire

let written f =
  let out = Buffer.create 64 in
  let w = Weft_flow.Writer.create (Weft_flow.Flow.of_string "" out) in
  f w ; Weft_flow.Writer.flush w ; Buffer.contents out

(* Each decoder, with its result dropped. *)
let decoders =
  [ (fun p -> ignore (M.greeting p))
  ; (fun p -> ignore (M.ok p))
  ; (fun p -> ignore (M.err p))
  ; (fun p -> ignore (M.auth_switch p))
  ; (fun p -> ignore (M.auth_more_data p))
  ; (fun p -> ignore (M.column p))
  ; (fun p -> ignore (M.row ~columns:4 p)) ]

let unhex h =
  String.init
    (String.length h / 2)
    (fun i -> Char.chr (int_of_string ("0x" ^ String.sub h (2 * i) 2)))

(* Valid packets of the pinned servers (test/unit/test_message.ml). *)
let valid =
  List.map unhex
    [ "0a382e342e3131000b0000001277514b776c776500ffffff0200ffdf150000000000000000000047771b3b236d222852332b790063616368696e675f736861325f70617373776f726400"
    ; "0a31312e382e382d4d617269614442000a000000744f6c543d734d6500feffe00200ff81150000000000003d000000367c307522436c7528654464006d7973716c5f6e61746976655f70617373776f726400"
    ; "036465660000000163000c2d0004000000fd01001f0000"
    ; "0131fb017803322e35"
    ; "fe000002000000"
    ; "ff2604233233303030447570"
    ; "fe6d7973716c5f6e61746976655f70617373776f726400616263" ]

let no_nul =
  Gen.string_of ~size:(Gen.int_range 0 30) (Gen.char_range '\001' '\255')

let tests =
  group "Message"
    [ prop "no decoder raises on any bytes" ~count:3000 Gen.string (fun p ->
          List.iter (fun d -> d p) decoders )
    ; prop "no decoder raises on a prefix of a valid packet" ~count:1000
        (Gen.pair (Gen.of_list valid) Gen.nat)
        (fun (p, n) ->
          let p = String.sub p 0 (n mod (String.length p + 1)) in
          List.iter (fun d -> d p) decoders )
    ; prop "a test decoder reads back the handshake response" ~count:1000
        (Gen.quad no_nul
           (Gen.string_of ~size:(Gen.int_range 0 255) Gen.char)
           no_nul
           (Gen.pair
              (Gen.map
                 (fun c -> c land lnot 0x30_0000)
                 (Gen.int_range 0 0xFFFF_FFFF) )
              (Gen.int_range 0 255) ) )
        (fun (user, auth, database, (capabilities, charset)) ->
          let p =
            written (fun w ->
                M.write_handshake_response w ~capabilities ~charset ~user ~auth
                  ~database ~plugin:"mysql_native_password" )
          in
          let c = W.cursor p in
          equal int capabilities (W.uint32 c) ;
          equal int 0x100_0000 (W.uint32 c) ;
          equal int charset (W.uint8 c) ;
          equal string (String.make 23 '\000') (W.bytes c 23) ;
          equal string user (W.string_nul c) ;
          equal string auth (W.bytes c (W.uint8 c)) ;
          equal string database (W.string_nul c) ;
          equal string "mysql_native_password" (W.string_nul c) ;
          equal int 0 (W.remaining c) ) ]
