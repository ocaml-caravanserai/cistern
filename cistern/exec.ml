(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

module Connection = Cistern_mysql.Connection

type ctx = {conn: Connection.t; mutable sent: bool}

let make conn = {conn; sent= false}

(* After a command that returned, keep its flag: the next command clears the
   flag of the connection. *)
let keep ctx r =
  ctx.sent <- ctx.sent || Connection.sent ctx.conn ;
  r

let query ?kind ctx r = keep ctx (Connection.query ?kind ctx.conn r)

let exec ?kind ctx r = keep ctx (Connection.exec ?kind ctx.conn r)

let rows ?kind ctx r decode =
  keep ctx (Connection.query_rows ?kind ctx.conn r decode)

let dialect ctx = (Connection.info ctx.conn).dialect

let sent ctx = ctx.sent || Connection.sent ctx.conn
