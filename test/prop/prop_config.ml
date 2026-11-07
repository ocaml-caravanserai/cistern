(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* Config.of_url: it never raises, and a URL that the generator builds from its
   parts gives these parts back. *)

open Windtrap
module C = Cistern_mysql.Config

(* Percent-encode each byte that is not a letter or a digit. *)
let encode s =
  String.concat ""
    (List.map
       (fun c ->
         if Char.Ascii.is_alphanum c then String.make 1 c
         else Printf.sprintf "%%%02X" (Char.code c) )
       (List.of_seq (String.to_seq s)) )

(* Any bytes but the control characters, which a value must not have. *)
let field lo =
  Gen.string_of ~size:(Gen.int_range lo 20)
    (Gen.map
       (fun c -> if Char.code c < 0x20 || c = '\x7f' then 'x' else c)
       Gen.char )

let host = Gen.of_list ["127.0.0.1"; "localhost"; "db.example"; "a-b_c"; "[::1]"]

let parts =
  Gen.quad (field 1) (field 0) (field 1)
    (Gen.pair host (Gen.option (Gen.int_range 1 65535)))

let tests =
  group "Config"
    [ prop "of_url never raises" ~count:3000
        (Gen.one_of
           [ Gen.string
           ; Gen.map (fun s -> "mysql://" ^ s) Gen.string
           ; Gen.map (fun s -> "mysql://u:p@h/db?" ^ s) Gen.string ] )
        (fun s -> ignore (C.of_url s))
    ; prop "of_url gives the parts back" ~count:1000 parts
        (fun (user, password, database, (h, port)) ->
          let url =
            Printf.sprintf "mysql://%s:%s@%s%s/%s?ssl-mode=DISABLED"
              (encode user) (encode password) h
              (Option.fold ~none:"" ~some:(Printf.sprintf ":%d") port)
              (encode database)
          in
          match C.of_url url with
          | Error m ->
              fail m
          | Ok c ->
              equal string user (C.user c) ;
              equal string password (C.password c) ;
              equal string database (C.database c) ;
              let expected_host = if h = "[::1]" then "::1" else h in
              ( match C.address c with
              | C.Tcp {host; port= p} ->
                  equal string expected_host host ;
                  equal int (Option.value port ~default:3306) p
              | Socket _ ->
                  fail "a socket address" ) ;
              is_false (C.needs_tls c) ) ]
