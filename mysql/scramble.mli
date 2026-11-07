(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(** The scrambles of the two authentication plugins (DESIGN.md,
    "Authentication"). The nonce is the 20 bytes of the greeting or of an
    [AuthSwitchRequest]. An empty password gives an empty response for the two
    plugins, as the MySQL client does. *)

val native : password:string -> nonce:string -> string
(** [mysql_native_password]: [SHA1(pw) XOR SHA1(nonce ^ SHA1(SHA1(pw)))], 20
    bytes. *)

val sha2 : password:string -> nonce:string -> string
(** The fast path of [caching_sha2_password]:
    [SHA256(pw) XOR SHA256(SHA256(SHA256(pw)) ^ nonce)], 32 bytes. *)
