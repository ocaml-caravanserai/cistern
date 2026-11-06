(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(** The two server families of version 1 (DESIGN.md, "Scope of version 1"). The
    dialect of a connection comes from [@@version] after the authentication,
    never from the greeting. *)

type t = Mysql | Mariadb
