(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

module Private = struct
  type kind = Read | Write

  type meta =
    { redacted: Cistern_core.Dialect.t -> string list
    ; kind: kind
    ; raw: Cistern_core.Raw.t option
    ; sql: Cistern_core.Dialect.t -> string list
    ; text: (string, string) result option }

  type 'a t =
    | Stmt :
        { meta: meta
        ; run: Exec.ctx -> ('r, Cistern_core.Error.t) result
        ; rows: 'r -> int
        ; finish: 'r -> 'a }
        -> 'a t
    | Pure : (unit -> 'a) -> 'a t

  let make meta ~run ~rows ~finish = Stmt {meta; run; rows; finish}

  let pure v = Pure (fun () -> v)

  let pure_value = function Pure f -> Some f | Stmt _ -> None

  let no_command =
    { redacted= (fun _ -> [])
    ; kind= Read
    ; raw= None
    ; sql= (fun _ -> [])
    ; text= None }

  let meta = function Stmt s -> s.meta | Pure _ -> no_command

  type 'a ran = {rows: int; finish: unit -> 'a}

  let run s ctx =
    match s with
    | Stmt s ->
        Result.map
          (fun r -> {rows= s.rows r; finish= (fun () -> s.finish r)})
          (s.run ctx)
    | Pure f ->
        Ok {rows= 0; finish= f}
        [@mutate
          off
            "Db.exec and Tx.exec give the value of a pure statement before run"]
end

type 'a t = 'a Private.t

let map f = function
  | Private.Stmt s ->
      Private.Stmt {s with finish= (fun r -> f (s.finish r))}
  | Private.Pure g ->
      Private.Pure (fun () -> f (g ()))

let to_sql d s = (Private.meta s).sql d
