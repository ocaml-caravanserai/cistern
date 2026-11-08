(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* The TLS values of the sim tests (the plan of phase 1.4, D13): a seeded
   Fortuna generator, the certificates of Cistern_certgen.Pki with the address
   of the fake server, the server configurations, the CA files, and the time of
   the simulator. Weft_sim runs all virtual domains on one OS thread, and a call
   of the generator has no switch point, thus a seeded Fortuna is safe there. A
   real program uses Mirage_crypto_rng_unix.use_default (). *)

module C = Cistern_mysql.Config
module Pki = Cistern_certgen.Pki

let seed () =
  Mirage_crypto_rng.set_default_generator
    (Mirage_crypto_rng.create ~seed:"cistern sim tls"
       (module Mirage_crypto_rng.Fortuna) )

let () = seed ()

(* The certificate of the fake servers: the names localhost, db.test, 10.0.0.1
   (Fake_server) and 10.0.0.9 (Fake_db). *)
let pki =
  Pki.make ~dns:["localhost"; "db.test"] ~ips:["10.0.0.1"; "10.0.0.9"] ()

(* A certificate whose common name has newlines, ESC bytes and 3 KB of text, and
   no other names (the review of the plan of phase 1.4, A-M2). *)
let hostile =
  Pki.make
    ~cn:(String.concat "" (List.init 300 (fun _ -> "evil\n\x1b[31mX ")))
    ~dns:[] ~ips:[] ()

let pin cert = X509.Certificate.fingerprint `SHA256 cert

let server_config ?ticket_cache (p : Pki.t) =
  Result.get_ok
    (Tls.Config.server ?ticket_cache
       ~certificates:(`Single ([p.cert], p.key))
       () )

(* The time of the simulator in the tests: 2025-06-15. The certificates are
   valid from 2020 to 2040. *)
let at_2025 sim =
  Weft_sim.set_posix sim (Weft.Time.posix_of_unix_s 1_750_000_000.)

(* [write path text]: a file of the simulator, which has its own file system in
   memory. *)
let write path text =
  let f = Weft.File.openfile [Write; Create; Truncate] path in
  Fun.protect
    ~finally:(fun () -> Weft.File.close f)
    (fun () -> Weft.File.write f text)

let ca_file = "/ca.pem"

let other_ca_file = "/other-ca.pem"

let hostile_ca_file = "/hostile-ca.pem"

(* The CA files, in the file system of a new simulation. *)
let install () =
  write ca_file (X509.Certificate.encode_pem pki.ca) ;
  write other_ca_file (X509.Certificate.encode_pem pki.other_ca) ;
  write hostile_ca_file (X509.Certificate.encode_pem hostile.ca)

let password = "secret-pw"

(* A TCP configuration with TLS. [ca] is a path. With [kill=false]: the scripts
   of Fake_server have no round trip 3. *)
let config ?(kill = false) ?(host = "10.0.0.1") ?ca ?fingerprint ?authenticator
    mode =
  Result.get_ok
    (C.make ~kill
       ~tls:(C.Tls {mode; ca; fingerprint})
       ?authenticator
       ~address:(C.Tcp {host; port= 3306})
       ~user:"u" ~password ~database:"db" () )
