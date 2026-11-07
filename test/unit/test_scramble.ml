(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* Scramble: known values. The expected values come from an independent
   implementation of the same formulas (Python hashlib), not from a MySQL
   source. Two independent logins on the pinned servers (MySQL 8.4.11, MariaDB
   11.4.13 and 11.8.8) checked the formulas on 2026-11-07: the Python client of
   the plan reviews of phase 1.1, and the code review of PR 1.1b with Cistern's
   own Scramble and handshake response. The integration suite of PR 1.1c2 is the
   proof in the tests. The third row has a nonce of a recorded MySQL 8.4.11
   greeting. *)

open Windtrap
module S = Cistern_mysql.Scramble

let hex s =
  String.concat ""
    (List.map
       (fun c -> Printf.sprintf "%02x" (Char.code c))
       (List.of_seq (String.to_seq s)) )

let unhex h =
  String.init
    (String.length h / 2)
    (fun i -> Char.chr (int_of_string ("0x" ^ String.sub h (2 * i) 2)))

let n1 = String.init 20 (fun i -> Char.chr (i + 1))

let rows =
  [ ( "secret"
    , n1
    , "b32bb3a583e1340c0a1108d58b1be49781ad8c2f"
    , "746ebe205d56a0707acb3e796e834e0dd7b1d61743b26bd5202c7a623230c7c9" )
  ; ( "\xc3\xa9t\xc3\xa9"
    , n1
    , "321ed1ff1c83f8e42cfe8ff6564f9b94bbc7c8e9"
    , "1fd31500fef2b362b7b815f7273c300d74a2913d04196184d748da89c0e62a42" )
  ; ( "cistern"
    , unhex "1277514b776c776547771b3b236d222852332b79"
    , "c45093fa0312a3d2ecaa0e941517426b20da30c3"
    , "e6ed8616da94768b0679f57748c37471eac25d674850d2f2189f14852b036d2c" ) ]

let tests =
  group "Scramble"
    [ cases "known values"
        ~id:(fun (p, _, _, _) -> String.escaped p)
        rows
        (fun (password, nonce, native, sha2) ->
          equal string native (hex (S.native ~password ~nonce)) ;
          equal string sha2 (hex (S.sha2 ~password ~nonce)) )
    ; test "an empty password gives an empty response" (fun () ->
          equal string "" (S.native ~password:"" ~nonce:n1) ;
          equal string "" (S.sha2 ~password:"" ~nonce:n1) ) ]
