(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

open Cistern_core
module Compile = Cistern_sql.Compile

type 'm t = {q: Compile.query; decode: Decoder.cursor -> 'm}

let where e t = {t with q= {t.q with where= t.q.where @ [Expr.ast e]}}

let order_by f dir t =
  let c = {Model_ast.table= Field.table f; column= Field.column f} in
  let d = match dir with `Asc -> Compile.Asc | `Desc -> Compile.Desc in
  {t with q= {t.q with order= t.q.order @ [(c, d)]}}

let not_negative what n =
  if n < 0 then invalid_arg (Printf.sprintf "Query.%s: %d is negative" what n)

let limit n t =
  not_negative "limit" n ;
  {t with q= {t.q with limit= Some n}}

let offset n t =
  not_negative "offset" n ;
  {t with q= {t.q with offset= Some n}}

let for_update t = {t with q= {t.q with lock= For_update}}

let for_share t = {t with q= {t.q with lock= For_share}}

(* A read of rows that [decode] decodes. *)
let read ast decode ~rows ~finish ~for_update =
  Typed.make ~for_update Read ast
    ~run:(fun ctx ~kind r -> Exec.rows ~kind ctx r (Decoder.run decode))
    ~rows ~finish

let locked t = t.q.lock = For_update

let to_list t =
  read (Compile.select t.q) t.decode ~rows:List.length ~finish:Fun.id
    ~for_update:(locked t)

let first t =
  read (Compile.first t.q) t.decode ~rows:List.length
    ~finish:(function x :: _ -> Some x | [] -> None)
    ~for_update:(locked t)

(* The one value of the one row of COUNT and EXISTS. Another count of rows gives
   Decode. *)
let scalar ast d =
  Typed.make Read ast
    ~run:(fun ctx ~kind r ->
      match Exec.rows ~kind ctx r (Decoder.run (fun c -> Decoder.next c d)) with
      | Ok [x] ->
          Ok x
      | Ok l ->
          Error
            (Error.Decode
               (Printf.sprintf "the reply has %d rows, not 1" (List.length l))
            )
      | Error e ->
          Error e )
    ~rows:(fun _ -> 1)
    ~finish:Fun.id

let count t = scalar (Compile.count t.q) Decoder.int

let exists t = scalar (Compile.exists t.q) Decoder.bool

(* A bulk write: the matched rows (CLIENT_FOUND_ROWS). *)
let write ast =
  let n (x : Exec_result.t) = Int64.to_int x.affected_rows in
  Typed.make Write ast
    ~run:(fun ctx ~kind r -> Exec.exec ~kind ctx r)
    ~rows:n ~finish:n

let update u t = write (Compile.update_all t.q (Update.assignments u))

let delete t = write (Compile.delete_all t.q)

module Private = struct
  let make model decode = {q= Compile.all model; decode}
end
