(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(** A statement: what [Db.exec] runs (DESIGN.md, "Core user types"). *)

type 'a t

val map : ('a -> 'b) -> 'a t -> 'b t
(** [map f s] gives [f] of the value of [s]. [f] runs after the connection went
    back to the pool, thus an exception of [f] holds no connection. *)

val to_sql : Cistern_core.Dialect.t -> 'a t -> string list
(** [to_sql d s] is the SQL of each command of [s] for the dialect [d], with the
    literals, for snapshot tests. It never goes into an event. A value that a
    later step takes from an earlier step shows as a placeholder, for example
    [<last_insert_id>] or [<keys>] (phase 2). A raw statement with a value with
    no literal gives [<Invalid_value: msg>]. *)

(** For the library (the pool and the statements). It can change in each
    version. *)
module Private : sig
  type kind = Read | Write

  type meta =
    { redacted: Cistern_core.Dialect.t -> string list
          (** The SQL of the events, with [?] in place of each literal. *)
    ; kind: kind
    ; raw: Cistern_core.Raw.t option
          (** A raw statement: it marks its connection dirty. *)
    ; sql: Cistern_core.Dialect.t -> string list  (** For {!to_sql} only. *)
    ; text: (string, string) result option
          (** The SQL text of a raw statement, made once. [Error msg]: a value
              with no literal; the pool gives [Invalid_value] before the
              checkout. *) }

  val make :
       meta
    -> run:(Exec.ctx -> ('r, Cistern_core.Error.t) result)
    -> rows:('r -> int)
    -> finish:('r -> 'a)
    -> 'a t

  val meta : 'a t -> meta

  (** A statement that ran: its count of rows, and its value, which the pool
      makes after the checkin. *)
  type 'a ran = {rows: int; finish: unit -> 'a}

  val run : 'a t -> Exec.ctx -> ('a ran, Cistern_core.Error.t) result
end
