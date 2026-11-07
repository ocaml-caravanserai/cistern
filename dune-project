(lang dune 3.20)

(name cistern)

(generate_opam_files true)

(source
 (github ocaml-caravanserai/cistern))

(license MPL-2.0)

(package
 (name cistern)
 (synopsis "An ORM for OCaml 5.5 on weft, for MySQL and MariaDB")
 (description
  "Cistern defines models as OCaml records with a deriver, and runs typed queries on MySQL and MariaDB through a pure OCaml client on weft.")
 (depends
  (ocaml
   (>= 5.5))
  weft
  (ptime
   (>= 1.2.0))
  (digestif
   (>= 1.0.0))
  (tls
   (>= 2.1.0))
  (x509
   (>= 1.0.0))
  domain-name
  ipaddr
  (windtrap
   (and
    :with-test
    (>= 0.3.0)))
  (ppx_windtrap
   (and
    :with-test
    (>= 0.3.0)))
  (mirage-crypto-ec :with-test)
  (mirage-crypto-rng :with-test)
  (fmt :with-test)))

(package
 (name ppx_cistern)
 (synopsis "The deriver of Cistern models")
 (depends
  (ocaml
   (>= 5.5))
  (ppxlib
   (>= 0.38))))
