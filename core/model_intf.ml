(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(** The signature of a model module (DESIGN.md, "Generated code"). The ppx of
    phase 3 makes one for each type of a group. Phase 2 writes them by hand. *)
module type MODEL = sig
  type t

  type key

  (** The column fields of a new row, with no [[@auto]] key. *)
  type draft

  val schema : Schema.model
  (** The model of the type: its table, its fields and its relations. *)

  val key : (t, key) Field.t
  (** The field of the [[@key]]. *)

  val fields : t Field.any list
  (** The column fields, in the order of the record and of [schema.fields]. *)

  val decode : Decoder.cursor -> t
  (** Reads the columns of a row in the order of {!fields}. *)

  val encode_draft : draft -> (string * Value.t) list
  (** The columns of an [INSERT] and their values, with the defaults of the
      model. *)

  val of_draft : draft -> key option -> t
  (** [of_draft d k] is the row of [d], with the key [k] from the server for an
      [[@auto]] key. *)
end
