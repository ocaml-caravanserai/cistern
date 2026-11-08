(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

open Weft
module Connection = Cistern_mysql.Connection
module Config = Cistern_mysql.Config
module Outcome = Cistern_mysql.Outcome
module Ddl = Cistern_sql.Ddl
open Cistern_core

module type MODEL = Model_intf.MODEL

let schemas (models : (module MODEL) list) =
  List.map (fun (module M : MODEL) -> M.schema) models

(* The division of a number that is not positive gives 0 or less, thus the
   minimum of 1 needs no branch. *)
let lock_wait_timeout ~remaining =
  let ns = Int64.sub (Span.to_ns remaining) 1_000_000_000L in
  max 1 (Int64.to_int (Int64.div (Int64.add ns 999_999_999L) 1_000_000_000L))

(* The misuse checks of push_schema and check_schema. *)
let misuse name pool =
  Pool.check_open pool ;
  if Pool.in_tx pool then invalid_arg ("Db." ^ name ^ " in the fiber of a Tx")

let fail e = raise (Error.Cistern_error e)

let push_schema ?(timeout = Span.s 30) pool models =
  misuse "push_schema" pool ;
  let ms = schemas models in
  ( match Schema.check ms with
  | Ok () ->
      ()
  | Error msgs ->
      invalid_arg ("Db.push_schema: " ^ String.concat "; " msgs) ) ;
  if ms <> [] then begin
    let deadline = Pool.add_span (Pool.now pool) timeout in
    let remaining () =
      Span.of_ns (Int64.sub (Span.to_ns deadline) (Span.to_ns (Pool.now pool)))
    in
    let config = Config.without_kill (Pool.config pool) in
    let since t0 = Time.diff Clock.Mono (Time.now Clock.Mono) t0 in
    let t0 = Time.now Clock.Mono in
    let connect_event error =
      Pool.emit pool
        (Event.Connect {address= Pool.address config; duration= since t0; error})
    in
    let c =
      match
        Fiber.with_timeout Clock.Mono (remaining ()) (fun () ->
            Connection.connect config )
      with
      | Ok c ->
          connect_event None ; c
      | Error e ->
          connect_event (Some e) ; fail e
      | exception Fiber.Timeout ->
          connect_event (Some Error.Timeout) ;
          fail Error.Timeout
    in
    (* One statement, with the margin check and the deadline of the flow. The
       statements are idempotent: a lost reply gives Timeout or Connection_lost,
       not Outcome_unknown. *)
    let run sql =
      let t0 = Time.now Clock.Mono in
      let event outcome =
        Pool.emit pool (Event.Query {sql; duration= since t0; outcome})
      in
      if not (Pool.time_left pool ~deadline) then (
        event (Event.Failed Error.Timeout) ;
        fail Error.Timeout ) ;
      Connection.set_deadline c (Pool.mono pool deadline) ;
      match Connection.exec ~kind:Outcome.Not_write c (Raw.sql sql) with
      | Ok r ->
          event (Event.Done {rows= Int64.to_int r.affected_rows})
      | Error e ->
          event (Event.Failed e) ; fail e
      | exception (Fiber.Cancelled _ as ex) ->
          event (Event.Cancelled {sent= Connection.sent c}) ;
          raise ex
    in
    match
      run
        (Printf.sprintf "SET foreign_key_checks = 0, lock_wait_timeout = %d"
           (lock_wait_timeout ~remaining:(remaining ())) ) ;
      let dialect = (Connection.info c).dialect in
      List.iter (fun m -> run (Ddl.create_table dialect ms m)) ms
    with
    | () ->
        ignore (Connection.close c : bool)
    | exception e ->
        ignore (Connection.abort c : bool) ;
        raise e
  end

let check_query ms =
  let tables =
    List.mapi
      (fun i (m : Schema.model) ->
        Raw.((if i = 0 then sql "" else sql ", ") ++ v (Value.Text m.table)) )
      ms
  in
  Raw.(
    sql
      "SELECT TABLE_NAME, COLUMN_NAME, DATA_TYPE, IS_NULLABLE, \
       CHARACTER_MAXIMUM_LENGTH, DATETIME_PRECISION, COLLATION_NAME, \
       COLUMN_TYPE LIKE '%unsigned%', EXTRA LIKE '%auto_increment%' FROM \
       information_schema.COLUMNS WHERE TABLE_SCHEMA = DATABASE() AND \
       TABLE_NAME IN ("
    ++ List.fold_left ( ++ ) (sql "") tables
    ++ sql ") ORDER BY TABLE_NAME, ORDINAL_POSITION" )

(* A row of information_schema.COLUMNS. *)
let column c : Ddl.column =
  let open Decoder in
  let table = next c string in
  let column = next c string in
  let data_type = next c string in
  let nullable = String.equal (next c string) "YES" in
  let max_length = next c (option int) in
  let precision = next c (option int) in
  let collation = next c (option string) in
  let unsigned = next c bool in
  let auto_increment = next c bool in
  { table
  ; column
  ; data_type
  ; nullable
  ; max_length
  ; precision
  ; collation
  ; unsigned
  ; auto_increment }

(* The read of check_schema: a statement that is not raw (no session reset),
   with the dialect of its connection. *)
let read r =
  (* The names of the tables passed Schema.check: valid UTF-8, thus each literal
     has its text. *)
  let sql = Result.get_ok (Raw.to_string r) in
  let redacted _ = [Raw.redacted r] in
  (* The statement is internal: no Stmt.to_sql of it, thus sql is the redacted
     text too. *)
  let meta : Stmt.Private.meta =
    { redacted
    ; kind= Read
    ; raw= None
    ; sql= redacted
    ; text= None
    ; for_update= false
    ; atomic= false
    ; multi= false }
  in
  Stmt.Private.make meta
    ~run:(fun ctx ->
      Result.map
        (fun rows -> (Exec.dialect ctx, rows))
        (Exec.rows ~kind:Outcome.Not_write ctx (Raw.sql sql)
           (Decoder.run column) ) )
    ~rows:(fun (_, rows) -> List.length rows)
    ~finish:Fun.id

let check_schema ~exec pool models =
  misuse "check_schema" pool ;
  let ms = schemas models in
  match Schema.check ms with
  | Error msgs ->
      Error msgs
  | Ok () when ms = [] ->
      Ok ()
  | Ok () -> (
    match exec (read (check_query ms)) with
    | Error e ->
        fail e
    | Ok (dialect, rows) -> (
      match Ddl.compare dialect ms rows with [] -> Ok () | msgs -> Error msgs )
    )
