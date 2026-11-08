(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

module Span = Weft.Span

type t = Pool.t

let with_db ?(pool_size = Cistern_core.Limits.pool_size)
    ?(max_lifetime = Cistern_core.Limits.max_lifetime)
    ?(checkout_timeout = Cistern_core.Limits.checkout_timeout) ?on_query ?random
    config f =
  if Cistern_mysql.Config.needs_tls config then
    Cistern_mysql.Connection.check_rng "Db.with_db" ;
  let pool =
    Pool.create ?random ?on_query ~pool_size ~max_lifetime ~checkout_timeout
      config
  in
  match
    Weft.Fiber.scope (fun s ->
        Weft.Fiber.fork_daemon s (fun () -> Pool.opener pool) ;
        if Cistern_mysql.Config.kill config then
          Weft.Fiber.fork_daemon s (fun () -> Pool.killer pool) ;
        f pool )
  with
  | v ->
      Pool.shutdown pool ; v
  | exception e ->
      Pool.shutdown pool ; raise e

(* An atomic statement runs through the transaction of Db.transaction (phase
   4b1), after the checks of misuse of Db.exec. *)
let exec ?(timeout = Cistern_core.Limits.exec_timeout) db s =
  let meta = Stmt.Private.meta s in
  match meta.text with
  | Some (Error _) ->
      (* Pool.exec gives Invalid_value before the checkout, with one event: also
         for an atomic statement, with no transaction. *)
      Pool.exec db ~timeout s
  | _ when meta.atomic ->
      (* The misuse checks of Db.transaction, in its order. *)
      Tx.Private.atomic ~timeout db s
  | _ ->
      Pool.exec db ~timeout s

let push_schema ?timeout db models = Schema_ops.push_schema ?timeout db models

let check_schema db models =
  Schema_ops.check_schema ~exec:(fun s -> exec db s) db models

let transaction ?(timeout = Cistern_core.Limits.transaction_timeout) db f =
  Tx.Private.transaction ~timeout db f

let exec_exn ?timeout db s =
  match exec ?timeout db s with
  | Ok v ->
      v
  | Error e ->
      raise (Cistern_core.Error.Cistern_error e)

module Private = struct
  let pool t = t
end
