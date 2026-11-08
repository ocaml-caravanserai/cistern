(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

module Print = Cistern_sql.Print

let command = function
  | Stmt.Private.Read ->
      Cistern_mysql.Outcome.Not_write
  | Write ->
      Cistern_mysql.Outcome.Write

let make ?(for_update = false) kind ast ~run ~rows ~finish =
  let text f d =
    match ast with Ok a -> [f d a] | Error m -> ["<Invalid_value: " ^ m ^ ">"]
  in
  let meta : Stmt.Private.meta =
    { redacted= text Print.redacted
    ; kind
    ; raw= None
    ; sql= text Print.to_sql
    ; text= (match ast with Ok _ -> None | Error m -> Some (Error m))
    ; for_update }
  in
  Stmt.Private.make meta
    ~run:(fun ctx ->
      match ast with
      | Ok a ->
          run ctx ~kind:(command kind)
            (Cistern_core.Raw.sql (Print.to_sql (Exec.dialect ctx) a))
      | Error message ->
          (* Not reached through Db.exec and Tx.exec: they read meta.text first,
             and fail before the checkout. A defense for another caller. *)
          Error (Cistern_core.Error.Invalid_value {code= None; message}) )
    ~rows ~finish
