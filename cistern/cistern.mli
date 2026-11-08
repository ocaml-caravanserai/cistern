(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(** Cistern: an ORM for MySQL and MariaDB on weft (DESIGN.md, "Core user
    types"). This module gives the types of the user. *)

module Db = Db
module Stmt = Stmt
module Tx = Tx
module Raw = Raw
module Error = Cistern_core.Error
module Event = Cistern_core.Event
module Value = Cistern_core.Value
module Decimal = Cistern_core.Decimal
module Dialect = Cistern_core.Dialect
module Exec_result = Cistern_core.Exec_result
module Config = Cistern_mysql.Config

(** The fields of a model (DESIGN.md, "Core user types"). The type is abstract:
    a field of one model does not type as a field of another model. The
    generated code makes each field with [Private.Field.make]. *)
module Field : sig
  type ('m, 'a) t = ('m, 'a) Cistern_core.Field.t

  val table : (_, _) t -> string

  val column : (_, _) t -> string

  val nullable : (_, _) t -> bool

  val get : ('m, 'a) t -> 'm -> 'a
  (** [get f r] is the value of [f] in the record [r]. *)

  val set : ('m, 'a) t -> 'm -> 'a -> 'm
  (** [set f r v] is [r] with the value [v] for [f]. *)
end

(** The filters (DESIGN.md, "Filters"). The operators are in [X.Q] of each
    model. *)
module Expr : sig
  type ('m, 'a) t = ('m, 'a) Cistern_core.Expr.t

  val and_ : ('m, bool) t list -> ('m, bool) t
  (** [and_ []] is [TRUE]. *)

  val or_ : ('m, bool) t list -> ('m, bool) t
  (** [or_ []] is [FALSE]. *)

  module type OPS = Cistern_core.Expr.OPS
end

(** The changes of an update (DESIGN.md, "Core user types"). *)
module Update : sig
  type 'm t = 'm Cistern_core.Update.t

  val set : ('m, 'a) Field.t -> 'a -> 'm t

  val ( & ) : 'm t -> 'm t -> 'm t
end

(** The queries (DESIGN.md, "Core user types"), with no [Private]: only the
    library makes a query of a model ([Private.Model.all]). *)
module Query : sig
  include module type of struct
      include Query
    end
    with module Private := Query.Private
end

(** The value of a relation field (DESIGN.md, "Deferred values"). *)
module Deferred = Cistern_core.Deferred

(** The signature of a model module (DESIGN.md, "Generated code"). *)
module type MODEL = Cistern_core.Model_intf.MODEL

(** The exception of the [_exn] functions. It is [Error.Cistern_error]. *)
exception Cistern_error of Error.t

(** The parts inside the library, for its tests. They can change in each
    version. *)
module Private : sig
  module Pool = Pool
  module Pool_state = Pool_state
  module Exec = Exec
  module Tx_rules = Tx_rules
  module Field = Cistern_core.Field
  module Expr = Cistern_core.Expr
  module Update = Cistern_core.Update
  module Schema = Cistern_core.Schema
  module Decoder = Cistern_core.Decoder
  module Encode = Cistern_core.Encode
  module Deferred = Cistern_core.Deferred
  module Relation = Relation
  module Model = Model
  module Query = Cistern__Query
  module Schema_ops = Schema_ops
end

(** A relation, for [Query.include_]. The generated [Rel] makes it, through
    [Cistern.Private.Relation]. It comes after [Private], so that
    [Private.Relation] is the whole module. *)
module Relation : sig
  type ('m, 'r) t = ('m, 'r) Private.Relation.t

  val name : (_, _) t -> string

  val kind : (_, _) t -> [`Has_many | `Belongs_to]
end
