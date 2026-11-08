(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

module Span = Weft.Span

type t = Pool.t

let with_db ?(pool_size = 10) ?(max_lifetime = Span.s 1800)
    ?(checkout_timeout = Span.s 30) ?on_query ?random config f =
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

let exec ?(timeout = Span.s 30) db s = Pool.exec db ~timeout s

let push_schema ?timeout db models = Schema_ops.push_schema ?timeout db models

let check_schema db models =
  Schema_ops.check_schema ~exec:(fun s -> exec db s) db models

let transaction ?(timeout = Span.s 30) db f =
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
