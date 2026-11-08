(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

open Weft
module Connection = Cistern_mysql.Connection
module Error = Cistern_core.Error
module Event = Cistern_core.Event

type scope =
  { emit: sql:string -> t0:Clock.mono -> Event.outcome -> unit
  ; time_left: unit -> bool
  ; after_ok: unit -> (unit, Error.t) result }

type ctx = {conn: Connection.t; mutable sent: bool; scope: scope option}

let make ?scope conn = {conn; sent= false; scope}

let sent ctx = ctx.sent || Connection.sent ctx.conn

(* After a command that returned, keep its flag: the next command clears the
   flag of the connection. *)
let keep ctx r =
  ctx.sent <- ctx.sent || Connection.sent ctx.conn ;
  r

(* One command. In a scope: the margin check, then the event of the command. *)
let command ctx ~redacted ~rows f =
  match ctx.scope with
  | None ->
      keep ctx (f ())
  | Some s -> (
      let t0 = Time.now Clock.Mono in
      let emit = s.emit ~sql:redacted ~t0 in
      if not (s.time_left ()) then (
        emit (Event.Failed Error.Timeout) ;
        Error Error.Timeout )
      else
        (* No keep: the event of a cancel reads the flag of the command, and the
           caller of a scope does not read [sent]. *)
        match f () with
        | Ok v -> (
          match s.after_ok () with
          | Ok () ->
              emit (Event.Done {rows= rows v}) ;
              Ok v
          | Error e ->
              emit (Event.Failed e) ; Error e )
        | Error e ->
            emit (Event.Failed e) ; Error e
        | exception (Fiber.Cancelled _ as ex) ->
            (* The flag of this command only: the event of one command tells if
               its bytes went out. *)
            emit (Event.Cancelled {sent= Connection.sent ctx.conn}) ;
            raise ex )

let query ?kind ?(redacted = "") ctx r =
  command ctx ~redacted
    ~rows:(fun (x : Cistern_core.Raw.result) -> List.length x.rows)
    (fun () -> Connection.query ?kind ctx.conn r)

let exec ?kind ?(redacted = "") ctx r =
  command ctx ~redacted
    ~rows:(fun (x : Cistern_core.Exec_result.t) -> Int64.to_int x.affected_rows)
    (fun () -> Connection.exec ?kind ctx.conn r)

let rows ?kind ?(redacted = "") ctx r decode =
  command ctx ~redacted ~rows:List.length (fun () ->
      Connection.query_rows ?kind ctx.conn r decode )

let dialect ctx = (Connection.info ctx.conn).dialect
