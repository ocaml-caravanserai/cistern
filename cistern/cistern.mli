(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(** Cistern: an ORM for MySQL and MariaDB on weft (DESIGN.md, "Core user
    types"). This module gives the types of the user. *)

module Db = Db
module Stmt = Stmt
module Raw = Raw
module Error = Cistern_core.Error
module Event = Cistern_core.Event
module Value = Cistern_core.Value
module Decimal = Cistern_core.Decimal
module Dialect = Cistern_core.Dialect
module Exec_result = Cistern_core.Exec_result
module Config = Cistern_mysql.Config

(** The exception of the [_exn] functions. It is [Error.Cistern_error]. *)
exception Cistern_error of Error.t

(** The parts inside the library, for its tests. They can change in each
    version. *)
module Private : sig
  module Pool = Pool
  module Pool_state = Pool_state
  module Exec = Exec
end
