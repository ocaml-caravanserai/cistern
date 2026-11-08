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

(* The scope of a belongs_to with a nullable key (phase 4c2, E11): no key gives
   a pure None, with no checkout and no event. *)
let get_parent : (module M : MODEL) -> M.key option -> M.t option Stmt.t =
 fun (module M) -> function
  | None ->
      Stmt.Private.pure None
  | Some k ->
      Stmt.map Option.some (get (module M) k)

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

let has_auto (m : Schema.model) =
  List.exists (fun (f : Schema.field) -> f.key && f.auto) m.fields

(* The key and the record of a new row: for an [@auto] key, the last insert id,
   else the key of the draft. *)
let inserted :
    (module M : MODEL)
    -> M.draft -> Exec_result.t -> (M.key * M.t, Error.t) result =
 fun (module M) d x ->
  if has_auto M.schema then
    Result.map (fun k -> (k, M.of_draft d (Some k))) (auto_key (module M) x)
  else
    let r = M.of_draft d None in
    Ok (Field.get M.key r, r)

(* The children of one has_many of a nested create (phase 4c1, the plan of phase
   4, E10). *)
type ('p, 'k) child =
  | Child :
      { model: (module MODEL with type t = 'c and type draft = 'd)
      ; fk: 'd -> 'k -> 'd
      ; set: 'p -> 'c list -> 'p
      ; drafts: 'd list }
      -> ('p, 'k) child

let child :
    (module C : MODEL)
    ->    fk:(C.draft -> 'k -> C.draft)
       -> set:('p -> C.t list -> 'p)
       -> C.draft list
       -> ('p, 'k) child =
 fun (module C) ~fk ~set drafts -> Child {model= (module C); fk; set; drafts}

(* One INSERT. Each group of [cs] (all empty) gives Loaded [] (the review of
   4c1, L1): a new row has no child. *)
let plain_create :
    (module M : MODEL) -> (M.t, M.key) child list -> M.draft -> M.t Stmt.t =
 fun (module M) cs d ->
  write
    (Compile.insert M.schema (M.encode_draft d))
    (fun x ->
      Result.map
        (fun (_, r) -> (x, List.fold_left (fun r (Child c) -> c.set r []) r cs))
        (inserted (module M) d x) )
    ~finish:snd

(* The INSERT of each child of [cs] with the key [k] of the parent, in order. *)
let child_inserts cs k =
  List.concat_map
    (fun (Child c) ->
      let module C = (val c.model) in
      List.map
        (fun d -> Compile.insert C.schema (C.encode_draft (c.fk d k)))
        c.drafts )
    cs

(* The SQL of to_sql: a literal that differs between two keys of the parent is
   the key, and shows as <last_insert_id>. *)
let placeholder a b =
  match (a, b) with
  | Ok (Cistern_sql.Ast.Insert x), Ok (Cistern_sql.Ast.Insert y) ->
      Ok
        (Cistern_sql.Ast.Insert
           { x with
             values=
               List.map2
                 (fun u v -> if String.equal u v then u else "<last_insert_id>")
                 x.values y.values } )
  | a, _ ->
      a

(* A nested create: the parent INSERT, then one INSERT for each child, in one
   atomic statement (E4, E10). *)
let nested :
    (module M : MODEL) -> (M.t, M.key) child list -> M.draft -> M.t Stmt.t =
 fun (module M) cs d ->
  let parent = Compile.insert M.schema (M.encode_draft d) in
  (* Two keys of the parent for the texts: for an [@auto] key, 1 and 2 through
     the decoder of the key, else the key of the draft (no placeholder). *)
  let sample n =
    match
      Decoder.run (fun c -> Decoder.next c (Field.decode M.key)) [|Some n|]
    with
    | Ok k ->
        k
    | Error m ->
        invalid_arg ("Model.create: the [@auto] key: " ^ m)
  in
  let k1, k2 =
    if has_auto M.schema then (sample "1", sample "2")
    else
      let k = Field.get M.key (M.of_draft d None) in
      (k, k)
  in
  let first = child_inserts cs k1 in
  let shown = List.map2 placeholder first (child_inserts cs k2) in
  let text f dialect = function
    | Ok a ->
        f dialect a
    | Error m ->
        "<Invalid_value: " ^ m ^ ">"
  in
  let meta : Stmt.Private.meta =
    { redacted=
        (fun dl ->
          List.map (text Cistern_sql.Print.redacted dl) (parent :: first) )
    ; kind= Write
    ; raw= None
    ; sql=
        (fun dl -> List.map (text Cistern_sql.Print.to_sql dl) (parent :: shown))
    ; text=
        List.find_map
          (function Error m -> Some (Error m) | Ok _ -> None)
          (parent :: first)
    ; for_update= false
    ; atomic= true }
  in
  let exec ctx ast =
    match ast with
    | Ok a ->
        let dl = Exec.dialect ctx in
        Exec.exec ~kind:Cistern_mysql.Outcome.Write
          ~redacted:(Cistern_sql.Print.redacted dl a)
          ctx
          (Raw.sql (Cistern_sql.Print.to_sql dl a))
    | Error message ->
        Error (Error.Invalid_value {code= None; message})
  in
  let ( let* ) = Result.bind in
  Stmt.Private.make meta
    ~run:(fun ctx ->
      let* x = exec ctx parent in
      let* k, p = inserted (module M) d x in
      let group (p, n) (Child c) =
        let module C = (val c.model) in
        let* rows, n =
          List.fold_left
            (fun acc d ->
              let* rows, n = acc in
              let d = c.fk d k in
              let* x = exec ctx (Compile.insert C.schema (C.encode_draft d)) in
              let* _, r = inserted (module C) d x in
              Ok
                ( r :: rows
                , ( n + 1 )
                  [@mutate
                    off
                      "an atomic statement emits no event of its own, thus no \
                       code reads its rows"] ) )
            (Ok ([], n))
            c.drafts
        in
        Ok (c.set p (List.rev rows), n)
      in
      List.fold_left
        (fun acc ch -> Result.bind acc (fun pn -> group pn ch))
        (Ok
           ( p
           , (1
             [@mutate
               off
                 "an atomic statement emits no event of its own, thus no code \
                  reads its rows"] ) ) )
        cs )
    ~rows:snd ~finish:fst

let create :
    (module M : MODEL)
    -> ?children:(M.t, M.key) child list -> M.draft -> M.t Stmt.t =
 fun (module M) ?(children = []) d ->
  if List.for_all (fun (Child c) -> List.is_empty c.drafts) children then
    plain_create (module M) children d
  else nested (module M) children d

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
