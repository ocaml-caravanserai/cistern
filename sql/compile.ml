(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

open Cistern_core

type direction = Asc | Desc

type query =
  { model: Schema.model
  ; where: Model_ast.t list
  ; order: (Model_ast.col * direction) list
  ; limit: int option
  ; offset: int option
  ; lock: Ast.lock }

let all model =
  {model; where= []; order= []; limit= None; offset= None; lock= Ast.No_lock}

(* A value with no literal: its message. *)
exception No_literal of string

let lit v =
  let b = Buffer.create 16 in
  match Literal.add b v with
  | Ok () ->
      Buffer.contents b
  | Error msg ->
      raise (No_literal msg)

let made f = try Ok (f ()) with No_literal msg -> Error msg

let col (m : Schema.model) (f : Schema.field) =
  {Ast.table= m.table; column= f.column}

let key_col (m : Schema.model) =
  match List.find_opt (fun (f : Schema.field) -> f.key) m.fields with
  | Some f ->
      col m f
  | None ->
      invalid_arg
        (Printf.sprintf "Compile: the model %s has no key"
           (Cistern_names.quote m.name) )

let rec expr = function
  | Model_ast.Cmp (op, c, v) ->
      Ast.Cmp (op, c, lit v)
  | Null_safe_eq (c, v) ->
      Ast.Null_safe_eq (c, lit v)
  | Is_null c ->
      Ast.Is_null c
  | Is_not_null c ->
      Ast.Is_not_null c
  | In (_, []) ->
      Ast.Bool false
  | In (c, vs) ->
      Ast.In (c, List.map lit vs)
  | Like (c, p) ->
      Ast.Like (c, lit (Value.Text p))
  | And [] ->
      Ast.Bool true
  | And es ->
      Ast.And (List.map expr es)
  | Or [] ->
      Ast.Bool false
  | Or es ->
      Ast.Or (List.map expr es)
  | Not e ->
      Ast.Not (expr e)
  | Bool b ->
      Ast.Bool b

let where = function
  | [] ->
      None
  | [e] ->
      Some (expr e)
  | es ->
      Some (Ast.And (List.map expr es))

(* The refusal of a negative limit or offset. *)
let valid q =
  let check what = function
    | Some n when n < 0 ->
        invalid_arg (Printf.sprintf "Query: the %s %d is negative" what n)
    | _ ->
        ()
  in
  check "limit" q.limit ; check "offset" q.offset

(* A query with a limit or an offset. *)
let paged q = q.limit <> None || q.offset <> None

(* The order, with the key appended for a limit or an offset (stable pages). *)
let order q =
  let os = List.map (fun (c, d) -> {Ast.by= c; desc= d = Desc}) q.order in
  if not (paged q) then os
  else
    let k = key_col q.model in
    if List.exists (fun (o : Ast.order) -> o.by = k) os then os
    else os @ [{Ast.by= k; desc= false}]

let columns (m : Schema.model) = Ast.Columns (List.map (col m) m.fields)

let select_of q =
  { Ast.columns= columns q.model
  ; table= q.model.table
  ; where= where q.where
  ; order= order q
  ; limit= q.limit
  ; offset= q.offset
  ; lock= q.lock }

let select q =
  valid q ;
  made (fun () -> Ast.Select (select_of q))

let first q =
  valid q ;
  let limit = match q.limit with Some 0 -> Some 0 | _ -> Some 1 in
  select {q with limit}

let count q =
  valid q ;
  made (fun () ->
      if paged q then
        Ast.Count_derived {(select_of q) with columns= Ast.One; lock= No_lock}
      else Ast.Count {table= q.model.table; where= where q.where} )

let exists q =
  valid q ;
  made (fun () ->
      let s = {(select_of q) with columns= Ast.One; lock= No_lock} in
      Ast.Exists (if paged q then s else {s with order= []}) )

let key_is m k = Some (Ast.Cmp (Eq, key_col m, lit k))

let get_by (m : Schema.model) ~column v =
  made (fun () ->
      Ast.Select
        { columns= columns m
        ; table= m.table
        ; where= Some (Ast.Cmp (Eq, {table= m.table; column}, lit v))
        ; order= []
        ; limit= Some 1
        ; offset= None
        ; lock= No_lock } )

let get (m : Schema.model) k = get_by m ~column:(key_col m).column k

let insert (m : Schema.model) cols =
  made (fun () ->
      Ast.Insert
        { table= m.table
        ; columns= List.map fst cols
        ; values= List.map (fun (_, v) -> lit v) cols } )

(* The assignments. [what] names the caller for the refusal of no column. *)
let set what cols =
  if List.is_empty cols then invalid_arg (what ^ ": no column to set") ;
  fun () -> List.map (fun (c, v) -> (c, lit v)) cols

let update (m : Schema.model) ~key cols =
  let set = set "Compile.update" cols in
  made (fun () ->
      Ast.Update
        { table= m.table
        ; set= set ()
        ; where= key_is m key
        ; order= []
        ; limit= None } )

let delete (m : Schema.model) ~key =
  made (fun () ->
      Ast.Delete {table= m.table; where= key_is m key; order= []; limit= None} )

(* The refusals of a bulk write (DESIGN.md, "Core user types"). *)
let bulk what q =
  if q.offset <> None then
    invalid_arg (what ^ ": OFFSET is not permitted (MySQL has no OFFSET here)") ;
  if q.lock <> Ast.No_lock then
    invalid_arg (what ^ ": FOR UPDATE and FOR SHARE are not permitted")

let update_all q cols =
  valid q ;
  bulk "Query.update" q ;
  let set = set "Query.update" cols in
  made (fun () ->
      Ast.Update
        { table= q.model.table
        ; set= set ()
        ; where= where q.where
        ; order= order q
        ; limit= q.limit } )

let delete_all q =
  valid q ;
  bulk "Query.delete" q ;
  made (fun () ->
      Ast.Delete
        { table= q.model.table
        ; where= where q.where
        ; order= order q
        ; limit= q.limit } )
