(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* Pki: the certificate verifies for the names that the test servers use, and
   only for them. The time of each check is fixed, not the clock. *)

open Windtrap
open Cistern_certgen

let () = Mirage_crypto_rng_unix.use_default ()

let pki = Pki.make ()

let other = Pki.make ()

let at s =
  match Ptime.of_rfc3339 s with Ok (t, _, _) -> t | Error _ -> assert false

let today = at "2026-11-06T00:00:00Z"

let host name = Some (Domain_name.host_exn (Domain_name.of_string_exn name))

let ip = Ipaddr.of_string_exn

(* True if the chain [pki.cert] verifies against [cas]. *)
let verifies ?(time = today) ?ip ?host cas =
  let auth =
    X509.Authenticator.chain_of_trust ~time:(fun () -> Some time) cas
  in
  Result.is_ok (auth ?ip ~host:(Option.join host) [pki.cert])

let extension k cert = X509.Extension.find k (X509.Certificate.extensions cert)

let perm path = (Unix.stat path).st_perm land 0o777

let temp () = Filename.temp_dir "test_certgen" ""

let read path = In_channel.with_open_bin path In_channel.input_all

let tests =
  group "Pki"
    [ test "the host name localhost verifies" (fun () ->
          is_true (verifies ~host:(host "localhost") [pki.ca]) )
    ; test "the IP address 127.0.0.1 verifies" (fun () ->
          is_true (verifies ~ip:(ip "127.0.0.1") ~host:None [pki.ca]) )
    ; test "other names do not verify" (fun () ->
          is_false (verifies ~host:(host "example.com") [pki.ca]) ;
          is_false (verifies ~ip:(ip "127.0.0.2") ~host:None [pki.ca]) ;
          is_false (verifies ~ip:(ip "::1") ~host:None [pki.ca]) )
    ; test "the CA of another Pki does not verify" (fun () ->
          is_false (verifies ~host:(host "localhost") [other.ca]) ;
          is_false (verifies ~host:(host "localhost") [pki.other_ca]) )
    ; test "the names of make" (fun () ->
          (* The names of the simulator, and no names at all. *)
          let sim = Pki.make ~dns:[] ~ips:["10.0.0.9"; "127.0.0.1"] () in
          let none =
            Pki.make ~cn:"MySQL_Server_Auto_Generated" ~dns:[] ~ips:[] ()
          in
          let check ?ip ?host p =
            Result.is_ok
              (X509.Authenticator.chain_of_trust
                 ~time:(fun () -> Some today)
                 [p.Pki.ca] ?ip ~host:(Option.join host) [p.cert] )
          in
          is_true (check ~ip:(ip "10.0.0.9") ~host:None sim) ;
          is_true (check ~ip:(ip "127.0.0.1") ~host:None sim) ;
          is_false (check ~host:(host "localhost") sim) ;
          is_false (check ~host:(host "localhost") none) ;
          is_false (check ~ip:(ip "127.0.0.1") ~host:None none) ;
          is_true (check ~host:None none) ;
          equal (option bool) None
            (Option.map (fun _ -> true) (extension Subject_alt_name none.cert)) )
    ; test "the validity is 2020 to 2040" (fun () ->
          let v time = verifies ~time ~host:(host "localhost") [pki.ca] in
          is_false (v (at "2019-12-31T23:59:59Z")) ;
          is_true (v (at "2020-01-01T00:00:01Z")) ;
          is_true (v (at "2039-12-31T23:59:59Z")) ;
          is_false (v (at "2040-01-02T00:00:00Z")) )
    ; test "the key is the key of the certificate" (fun () ->
          equal string
            (X509.Public_key.id (X509.Certificate.public_key pki.cert))
            (X509.Public_key.id (X509.Private_key.public pki.key)) ;
          is_false
            ( X509.Public_key.id (X509.Certificate.public_key pki.ca)
            = X509.Public_key.id (X509.Private_key.public pki.key) ) )
    ; test "the extensions" (fun () ->
          is_true
            ( extension Basic_constraints pki.ca = Some (true, (true, None))
            && extension Key_usage pki.ca = Some (true, [`Key_cert_sign])
            && extension Basic_constraints pki.cert = Some (true, (false, None))
            && extension Ext_key_usage pki.cert = Some (false, [`Server_auth])
            ) )
    ; test "the files decode to the same values" (fun () ->
          let fs = Pki.files pki in
          equal (list string)
            [ "ca.pem"
            ; "server-cert.pem"
            ; "server-key.pem"
            ; "server-cert.sha256"
            ; "other-ca.pem" ]
            (List.map fst fs) ;
          let text name = List.assoc name fs in
          let cert name =
            Result.get_ok (X509.Certificate.decode_pem (text name))
          in
          let der = X509.Certificate.encode_der in
          equal string (der pki.ca) (der (cert "ca.pem")) ;
          equal string (der pki.cert) (der (cert "server-cert.pem")) ;
          equal string (der pki.other_ca) (der (cert "other-ca.pem")) ;
          (* The hex SHA-256 of the DER, as openssl x509 -fingerprint -sha256
             gives it (in lowercase, with no colons). *)
          equal string
            (Digestif.SHA256.to_hex
               (Digestif.SHA256.digest_string (der pki.cert)) )
            (text "server-cert.sha256") ;
          is_true
            (String.starts_with ~prefix:"-----BEGIN PRIVATE KEY-----"
               (text "server-key.pem") ) ;
          equal string (text "server-key.pem")
            (X509.Private_key.encode_pem
               (Result.get_ok
                  (X509.Private_key.decode_pem (text "server-key.pem")) ) ) )
    ; test "write makes the directory with the files" (fun () ->
          let parent = temp () in
          let dir = Filename.concat parent "certs" in
          equal (result unit string) (Ok ()) (Pki.write ~dir pki) ;
          equal (list string) ["certs"] (Array.to_list (Sys.readdir parent)) ;
          equal (list string)
            [ "ca.pem"
            ; "other-ca.pem"
            ; "server-cert.pem"
            ; "server-cert.sha256"
            ; "server-key.pem" ]
            (List.sort compare (Array.to_list (Sys.readdir dir))) ;
          List.iter
            (fun (name, text) ->
              equal string text (read (Filename.concat dir name)) )
            (Pki.files pki) ;
          (* No group or other bits on the key, with any umask. *)
          equal int 0 (perm (Filename.concat dir "server-key.pem") land 0o077) ;
          is_true (perm (Filename.concat dir "ca.pem") land 0o400 <> 0) ;
          equal int 0 (perm dir land 0o077) )
    ; test "write refuses a path that exists" (fun () ->
          let dir = temp () in
          equal (result unit string)
            (Error (dir ^ ": the path exists"))
            (Pki.write ~dir pki) ;
          equal (list string) [] (Array.to_list (Sys.readdir dir)) )
    ; test "write into a parent that does not exist" (fun () ->
          let parent = Filename.concat (temp ()) "none" in
          match Pki.write ~dir:(Filename.concat parent "certs") pki with
          | Ok () ->
              fail "write gave Ok"
          | Error m ->
              is_true (String.starts_with ~prefix:(parent ^ ": ") m) ) ]

let () = exit (Windtrap.run "certgen" [tests])
