(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

open Cistern_core
module Compile = Cistern_sql.Compile
module Print = Cistern_sql.Print

(* A relation that include_ loads (phase 4d, E8): the texts of its query for
   to_sql (the keys as <keys>) and for the events, and its load of the rows of
   the main query. *)
type 'm inc =
  { sql: Cistern_sql.Ast.lock -> Dialect.t -> string
  ; redacted: Cistern_sql.Ast.lock -> Dialect.t -> string
  ; load:
      Exec.ctx -> Cistern_sql.Ast.lock -> 'm list -> ('m list, Error.t) result
  }

type 'm t = {q: Compile.query; decode: Decoder.cursor -> 'm; incs: 'm inc list}

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

(* The keys of the chunks of include_ (E8). *)
let chunk_size = Cistern_core.Limits.in_keys

let rec chunks l =
  let rec take n acc = function
    | x :: rest when n > 0 ->
        take (n - 1) (x :: acc) rest
    | rest ->
        (List.rev acc, rest)
  in
  match take chunk_size [] l with [], _ -> [] | c, rest -> c :: chunks rest

(* A table of values with the equality of Value (the notes of the review of 4a).
   Value.equal compares a Decimal by its text (Decimal.equal), a Float by its
   bits and a Datetime with Ptime.equal (a normal form), thus two equal values
   have the same structural hash. The keys come from the server, which gives the
   text of a DECIMAL with the scale of its column. *)
module Vt = Hashtbl.Make (struct
  type t = Value.t

  let equal = Value.equal

  let hash = Hashtbl.hash
end)

(* The values of [l], once each, in the order of their first place. *)
let unique l =
  let seen = Vt.create 16 in
  List.filter
    (fun v -> if Vt.mem seen v then false else (Vt.add seen v () ; true))
    l

(* The query of the rows of [model] whose [column] is in [keys], with the lock
   of the main query, in the order of [order]. *)
let keyed (model : Schema.model) ~column ~order ~lock keys =
  let col = {Model_ast.table= model.table; column} in
  Compile.select
    { (Compile.all model) with
      where= [Model_ast.In (col, keys)]
    ; order=
        List.map
          (fun c -> ({Model_ast.table= model.table; column= c}, Compile.Asc))
          order
    ; lock }

(* The texts of a query of include_: <keys> in place of the keys. *)
let texts model ~column ~order =
  let shown f lock dialect =
    (* Any one key: the text shows <keys> in its place. *)
    match
      keyed model ~column ~order ~lock
        [Value.Int (0L [@mutate off "the text replaces the key"])]
    with
    | Ok (Cistern_sql.Ast.Select s) ->
        let rec keys : Cistern_sql.Ast.expr -> Cistern_sql.Ast.expr = function
          | In (c, _) ->
              In (c, ["<keys>"])
          | And l ->
              And (List.map keys l)
          | e ->
              e
        in
        f dialect
          (Cistern_sql.Ast.Select {s with where= Option.map keys s.where})
    | Ok a ->
        f dialect a
    | Error m ->
        "<Invalid_value: " ^ m ^ ">"
  in
  (shown Print.to_sql, shown Print.redacted)

(* The rows of each chunk of [keys], or the first error. No key: no query. *)
let fetch (type c) ctx (module C : Model_intf.MODEL with type t = c) ~column
    ~order ~lock keys : (c list, Error.t) result =
  List.fold_left
    (fun acc chunk ->
      Result.bind acc (fun rows ->
          match keyed C.schema ~column ~order ~lock chunk with
          | Error message ->
              (* Not reached: the keys come from the server through the key
                 field, and rule 3 refuses a float key, thus each key has a
                 literal. *)
              Error (Error.Invalid_value {code= None; message})
          | Ok a ->
              let dl = Exec.dialect ctx in
              Result.map
                (fun l -> List.rev_append l rows)
                (Exec.rows ~kind:Cistern_mysql.Outcome.Not_write
                   ~redacted:(Print.redacted dl a) ctx
                   (Raw.sql (Print.to_sql dl a))
                   (Decoder.run C.decode) ) ) )
    (Ok []) (chunks keys)
  |> Result.map List.rev

(* The column of the key of the parent in the child: the one Belongs_to of the
   child to the parent (rule 9), through its field ([@column] can change the
   column). *)
let child_column (c : Schema.model) (p : Schema.model) =
  let key =
    List.find_map
      (function
        | Schema.Belongs_to {target; key; _} when target = p.name ->
            Some key
        | _ ->
            None )
      c.relations
  in
  match key with
  | Some key -> (
    match List.find_opt (fun (f : Schema.field) -> f.name = key) c.fields with
    | Some f ->
        f.column
    | None ->
        invalid_arg ("Query.include_: no field " ^ key) )
  | None ->
      invalid_arg
        (Printf.sprintf "Query.include_: the model %s has no Belongs_to to %s"
           c.name p.name )

let key_column (m : Schema.model) =
  match List.find_opt (fun (f : Schema.field) -> f.key) m.fields with
  | Some f ->
      f.column
  | None ->
      invalid_arg ("Query.include_: the model " ^ m.name ^ " has no key")

let include_ : type m r. (m, r) Relation.t -> m t -> m t =
 fun rel t ->
  let inc =
    match rel with
    | Has_many h ->
        let (module P) = h.parent in
        let (module C) = h.child in
        let column = child_column C.schema P.schema in
        let order = [key_column C.schema] in
        let sql, redacted = texts C.schema ~column ~order in
        let pkey r = Field.encode P.key (Field.get P.key r) in
        (* No key gives no chunk, thus no query. *)
        let load ctx lock (rows : m list) =
          Result.map
            (fun children ->
              (* The children of each key, in the order of the reply (the key of
                 the child). *)
              let groups = Vt.create 16 in
              List.iter
                (fun c ->
                  match h.key c with
                  | Some k ->
                      let v = Field.encode P.key k in
                      Vt.replace groups v
                        (c :: Option.value (Vt.find_opt groups v) ~default:[])
                  | None ->
                      () )
                children ;
              List.map
                (fun r ->
                  h.set r
                    (List.rev
                       (Option.value (Vt.find_opt groups (pkey r)) ~default:[]) ) )
                rows )
            (fetch ctx
               (module C)
               ~column ~order ~lock
               (unique (List.map pkey rows)) )
        in
        {sql; redacted; load}
    | Belongs_to b ->
        let (module P) = b.parent in
        let column = key_column P.schema in
        let sql, redacted = texts P.schema ~column ~order:[] in
        let ckey r = Option.map (Field.encode P.key) (b.key r) in
        let load ctx lock (rows : m list) =
          let parents =
            fetch ctx
              (module P)
              ~column ~order:[] ~lock
              (unique (List.filter_map ckey rows))
          in
          Result.map
            (fun parents ->
              let by_key = Vt.create 16 in
              List.iter
                (fun p ->
                  Vt.replace by_key (Field.encode P.key (Field.get P.key p)) p )
                parents ;
              List.map
                (fun r ->
                  match ckey r with
                  | None ->
                      b.set r None
                  | Some v -> (
                    match Vt.find_opt by_key v with
                    | Some p ->
                        b.set r (Some p)
                    | None ->
                        (* The parent is gone (E17): the relation stays. *)
                        r ) )
                rows )
            parents
        in
        {sql; redacted; load}
  in
  {t with incs= t.incs @ [inc]}

(* A query with include_: the main query, then the query of each relation, in
   one statement of several commands (multi): each command emits its own
   event. *)
let with_incs t ast ~finish =
  let text f dl =
    match ast with Ok a -> f dl a | Error m -> "<Invalid_value: " ^ m ^ ">"
  in
  let meta : Stmt.Private.meta =
    { redacted=
        (fun dl ->
          text Print.redacted dl
          :: List.map (fun i -> i.redacted t.q.lock dl) t.incs )
    ; kind= Read
    ; raw= None
    ; sql=
        (fun dl ->
          text Print.to_sql dl :: List.map (fun i -> i.sql t.q.lock dl) t.incs )
    ; text= (match ast with Ok _ -> None | Error m -> Some (Error m))
    ; for_update= locked t
    ; atomic= false
    ; multi= true }
  in
  Stmt.Private.make meta
    ~run:(fun ctx ->
      match ast with
      | Error message ->
          Error (Error.Invalid_value {code= None; message})
      | Ok a ->
          let dl = Exec.dialect ctx in
          List.fold_left
            (fun acc i -> Result.bind acc (i.load ctx t.q.lock))
            (Exec.rows ~kind:Cistern_mysql.Outcome.Not_write
               ~redacted:(Print.redacted dl a) ctx
               (Raw.sql (Print.to_sql dl a))
               (Decoder.run t.decode) )
            t.incs )
    ~rows:List.length ~finish

let to_list t =
  match t.incs with
  | [] ->
      read (Compile.select t.q) t.decode ~rows:List.length ~finish:Fun.id
        ~for_update:(locked t)
  | _ ->
      with_incs t (Compile.select t.q) ~finish:Fun.id

let first t =
  let finish = function x :: _ -> Some x | [] -> None in
  match t.incs with
  | [] ->
      read (Compile.first t.q) t.decode ~rows:List.length ~finish
        ~for_update:(locked t)
  | _ ->
      with_incs t (Compile.first t.q) ~finish

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

let no_include what t =
  match t.incs with
  | [] ->
      ()
  | _ ->
      invalid_arg (Printf.sprintf "Query.%s: a query with include_" what)

let update u t =
  no_include "update" t ;
  write (Compile.update_all t.q (Update.assignments u))

let delete t =
  no_include "delete" t ;
  write (Compile.delete_all t.q)

module Private = struct
  let make model decode = {q= Compile.all model; decode; incs= []}

  let unique = unique
end
