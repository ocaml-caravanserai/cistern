(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

open Cistern_core
module Compile = Cistern_sql.Compile

module type MODEL = Model_intf.MODEL

(* A read of rows that M.decode decodes, and [check] of the rows. *)
let select :
    (module M : MODEL)
    ->    (Cistern_sql.Ast.t, string) result
       -> (M.t list -> ('a, Error.t) result)
       -> 'a Stmt.t =
 fun (module M) ast check ->
  Typed.make Read ast
    ~run:(fun ctx ~kind r ->
      match Exec.rows ~kind ctx r (Decoder.run M.decode) with
      | Ok l ->
          Result.map (fun v -> (v, List.length l)) (check l)
      | Error e ->
          Error e )
    ~rows:snd ~finish:fst

let one = function x :: _ -> Ok x | [] -> Error Error.Not_found

let first l = Ok (match l with x :: _ -> Some x | [] -> None)

let get : (module M : MODEL) -> M.key -> M.t Stmt.t =
 fun (module M) k ->
  select (module M) (Compile.get M.schema (Field.encode M.key k)) one

let find : (module M : MODEL) -> M.key -> M.t option Stmt.t =
 fun (module M) k ->
  select (module M) (Compile.get M.schema (Field.encode M.key k)) first

(* The SQL of get_by and find_by. *)
let by (m : Schema.model) f v =
  match Field.encode f v with
  | Value.Null ->
      invalid_arg
        (Printf.sprintf "Model.get_by: None for the field %s.%s"
           (Cistern_names.quote m.table)
           (Cistern_names.quote (Field.column f)) )
  | x ->
      Compile.get_by m ~column:(Field.column f) x

let get_by : (module M : MODEL) -> (M.t, 'a) Field.t -> 'a -> M.t Stmt.t =
 fun (module M) f v -> select (module M) (by M.schema f v) one

let find_by : (module M : MODEL) -> (M.t, 'a) Field.t -> 'a -> M.t option Stmt.t
    =
 fun (module M) f v -> select (module M) (by M.schema f v) first

let all : (module M : MODEL) -> M.t Query.t =
 fun (module M) -> Query.Private.make M.schema M.decode

let where : (module M : MODEL) -> (M.t, bool) Expr.t -> M.t Query.t =
 fun (module M) e -> Query.where e (all (module M))

(* A write, with [check] of its counts. *)
let write ast check ~finish =
  Typed.make Write ast
    ~run:(fun ctx ~kind r -> Result.bind (Exec.exec ~kind ctx r) check)
    ~rows:(fun ((x : Exec_result.t), _) -> Int64.to_int x.affected_rows)
    ~finish

(* The key of a new row: the last insert id, through the decoder of the key. *)
let auto_key : (module M : MODEL) -> Exec_result.t -> (M.key, Error.t) result =
 fun (module M) x ->
  if Int64.equal x.last_insert_id 0L then
    Error (Error.Decode "the INSERT of an [@auto] key gave no insert id")
  else
    Result.map_error
      (fun m -> Error.Decode m)
      (Decoder.run
         (fun c -> Decoder.next c (Field.decode M.key))
         [|Some (Int64.to_string x.last_insert_id)|] )

let create : (module M : MODEL) -> M.draft -> M.t Stmt.t =
 fun (module M) d ->
  let auto =
    List.exists (fun (f : Schema.field) -> f.key && f.auto) M.schema.fields
  in
  write
    (Compile.insert M.schema (M.encode_draft d))
    (fun x ->
      if auto then
        Result.map (fun k -> (x, M.of_draft d (Some k))) (auto_key (module M) x)
      else Ok (x, M.of_draft d None) )
    ~finish:snd

(* 0 matched rows: Not_found. *)
let matched (x : Exec_result.t) =
  if Int64.equal x.affected_rows 0L then Error Error.Not_found else Ok (x, ())

let key_of : (module M : MODEL) -> M.t -> Value.t =
 fun (module M) r -> Field.encode M.key (Field.get M.key r)

let update : (module M : MODEL) -> M.t Update.t list -> M.t -> M.t Stmt.t =
 fun (module M) us r ->
  match us with
  | [] ->
      Stmt.Private.pure r
  | u :: rest ->
      let u = List.fold_left Update.( & ) u rest in
      let cols = Update.assignments u in
      write
        (Compile.update M.schema ~key:(key_of (module M) r) cols)
        matched
        ~finish:(fun _ -> Update.apply u r)

let delete : (module M : MODEL) -> M.t -> unit Stmt.t =
 fun (module M) r ->
  write (Compile.delete M.schema ~key:(key_of (module M) r)) matched ~finish:snd
