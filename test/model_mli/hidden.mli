(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(** Two derived models, with only their records, [model] and [create]. *)

type item = {id: int64; name: string; note: string option}

type tag = {label: string; item_id: int64}

module Item : sig
  val model : (module Cistern.MODEL)

  val create : name:string -> ?note:string option -> unit -> item Cistern.Stmt.t
end

module Tag : sig
  val model : (module Cistern.MODEL)
end
