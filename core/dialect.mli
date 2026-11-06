(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(** The two server families of version 1 (DESIGN.md, "Scope of version 1"). The
    dialect of a connection comes from [@@version] and [@@version_comment] after
    the authentication, never from the greeting.

    Version 1 supports only MySQL 8.4 and MariaDB 11.4 and 11.8. Another server
    that looks like MySQL (MySQL 5.7, a fork, a proxy) gets the rules of MySQL,
    but it is not supported: for example, MySQL 5.7 refuses the session setup of
    Cistern with error 1273. *)

type t = Mysql | Mariadb

val of_version : version:string -> version_comment:string -> (t, string) result
(** [of_version ~version ~version_comment] is [Mariadb] if [version] or
    [version_comment] contains [mariadb], with no regard to case, else [Mysql].
    The pinned MySQL build has the comment [Source distribution], thus the
    comment cannot identify MySQL.

    [Error msg] if [version] does not start with [<digits>.<digits>.<digits>]. A
    suffix such as [-log], [-debug] or [-0ubuntu] after the numbers is accepted.
    [msg] does not contain the text of the server. *)

val collation : t -> string
(** The binary collation with no trailing-space padding (DESIGN.md, "DDL"):
    [utf8mb4_0900_bin] for MySQL, [utf8mb4_nopad_bin] for MariaDB. *)

val for_share : t -> string
(** The clause of a shared row lock: [FOR SHARE] for MySQL, [LOCK IN SHARE MODE]
    for MariaDB. The grammar of MariaDB 11.4 and 11.8 has no [FOR SHARE]. *)
