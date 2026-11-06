(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* Dialect: the dialect from @@version and @@version_comment, the collation and
   the clause of a shared lock. The versions and comments are the ones of the
   pinned servers and of common builds. *)

open Windtrap
open Cistern_core

let dialect =
  Testable.make
    ~pp:(fun ppf (d : Dialect.t) ->
      Format.pp_print_string ppf
        (match d with Mysql -> "Mysql" | Mariadb -> "Mariadb") )
    ~equal:( = )

let found =
  [ ("8.4.11", "Source distribution", Dialect.Mysql)
  ; ("8.4.11", "MySQL Community Server - GPL", Mysql)
  ; ("8.4.11-log", "Source distribution", Mysql)
  ; ("8.0.36-0ubuntu0.22.04.1", "(Ubuntu)", Mysql)
  ; ("8.0.36-28", "Percona Server (GPL), Release 28", Mysql)
  ; ("11.4.13-MariaDB", "MariaDB Server", Mariadb)
  ; ("11.8.8-MariaDB-log", "Source distribution", Mariadb)
  ; ("11.4.13-MariaDB-debug", "x", Mariadb)
  ; ("11.4.13", "MariaDB Server", Mariadb)
  ; ("8.0.36", "MariaDB Server", Mariadb)
  ; ("8.0.36", "mariadb.org binary distribution", Mariadb)
  ; ("10.11.6-mariadb", "", Mariadb)
  ; ("5.5.5-10.11.6-MariaDB", "", Mariadb) ]

let refused = [""; "abc"; "8.4"; "8."; "8..4.11"; " 8.4.11"; "8.4.x"]

let tests =
  group "Dialect"
    [ cases "of_version"
        ~id:(fun (v, c, _) -> Printf.sprintf "%S %S" v c)
        found
        (fun (version, version_comment, expected) ->
          equal (result dialect string) (Ok expected)
            (Dialect.of_version ~version ~version_comment) )
    ; cases "of_version refuses a version with no numbers"
        ~id:(Printf.sprintf "%S") refused (fun version ->
          equal (result dialect string)
            (Error
               "the server version does not start with <major>.<minor>.<patch>"
            )
            (Dialect.of_version ~version ~version_comment:"MariaDB Server") )
    ; test "collation" (fun () ->
          equal string "utf8mb4_0900_bin" (Dialect.collation Mysql) ;
          equal string "utf8mb4_nopad_bin" (Dialect.collation Mariadb) )
    ; test "for_share" (fun () ->
          equal string "FOR SHARE" (Dialect.for_share Mysql) ;
          equal string "LOCK IN SHARE MODE" (Dialect.for_share Mariadb) ) ]
