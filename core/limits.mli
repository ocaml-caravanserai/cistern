(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(** The numbers of version 1 (DESIGN.md, "Limits and defaults"; the page
    [limits] of the doc of the user). The code of the library takes each limit
    from here, and a unit row compares each value with the page. *)

val pool_size : int
(** The default [pool_size] of [Db.with_db]. *)

val checkout_timeout : Weft.Span.t
(** The default [checkout_timeout] of [Db.with_db]. *)

val exec_timeout : Weft.Span.t
(** The default [?timeout] of [Db.exec]. *)

val transaction_timeout : Weft.Span.t
(** The default [?timeout] of [Db.transaction]. *)

val margin : Weft.Span.t
(** A statement with less remaining time is not sent (not for [COMMIT] and
    [ROLLBACK]). *)

val end_min : Weft.Span.t
(** The least deadline of [COMMIT] and [ROLLBACK]. *)

val end_max : Weft.Span.t
(** The greatest deadline of [COMMIT] and [ROLLBACK]. *)

val commit_guard : Weft.Span.t
(** The outer limit of [COMMIT] and [ROLLBACK] is their deadline and this span:
    only a guard. *)

val max_lifetime : Weft.Span.t
(** The default [max_lifetime] of [Db.with_db]. *)

val jitter : float
(** The jitter of the lifetime of each connection: at most this part of
    [max_lifetime], more or less. *)

val idle_max : Weft.Span.t
(** The greatest idle limit. The idle limit is the smaller of this value and
    [@@wait_timeout] minus {!idle_offset}, at least {!idle_min}. *)

val idle_offset : Weft.Span.t

val idle_min : Weft.Span.t

val connect_timeout : Weft.Span.t
(** The limit of one connect (the pool and the control connection). *)

val concurrent_connects : int
(** The connects that one pool runs at the same time. *)

val backoff_first : Weft.Span.t
(** The first wait after a failed connect, and after a failed try of the killer.
*)

val backoff_max : Weft.Span.t
(** The greatest wait after failed connects, and after failed tries of the
    killer. *)

val kill_limit : Weft.Span.t
(** The limit of one [KILL]. *)

val killing_age : Weft.Span.t
(** The age limit of a [Killing] slot. *)

val drain_limit : Weft.Span.t
(** The limit of the kill drain at the end of [Db.with_db]. *)

val close_limit : Weft.Span.t
(** The limit of the polite closes of the connections at the end of
    [Db.with_db]. *)

val in_keys : int
(** The keys in one [IN (...)] of [Query.include_]. *)

val identifier_length : int
(** The characters of a name of a table, a column, an index or a constraint. *)
