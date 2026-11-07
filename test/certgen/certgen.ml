(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* certgen DIR: make the directory DIR with ca.pem, server-cert.pem and
   server-key.pem, server-cert.sha256 and other-ca.pem (DESIGN.md, "Database
   servers for the tests"). DIR must not exist. *)

let () =
  match Sys.argv with
  | [|_; dir|] -> (
      Mirage_crypto_rng_unix.use_default () ;
      match Cistern_certgen.Pki.write ~dir (Cistern_certgen.Pki.make ()) with
      | Ok () ->
          ()
      | Error m ->
          prerr_endline ("certgen: " ^ m) ;
          exit 1 )
  | _ ->
      prerr_endline "usage: certgen DIR" ;
      exit 2
