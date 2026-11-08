(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

type ('m, 'a) t = Model_ast.t

let ast e = e

let and_ = function [] -> Model_ast.Bool true | l -> Model_ast.And l

let or_ = function [] -> Model_ast.Bool false | l -> Model_ast.Or l

module type OPS = sig
  val ( ==. ) : ('m, 'a) Field.t -> 'a -> ('m, bool) t

  val ( <>. ) : ('m, 'a) Field.t -> 'a -> ('m, bool) t

  val ( <. ) : ('m, 'a) Field.t -> 'a -> ('m, bool) t

  val ( >. ) : ('m, 'a) Field.t -> 'a -> ('m, bool) t

  val ( <=. ) : ('m, 'a) Field.t -> 'a -> ('m, bool) t

  val ( >=. ) : ('m, 'a) Field.t -> 'a -> ('m, bool) t

  val is_null : ('m, 'a option) Field.t -> ('m, bool) t

  val in_ : ('m, 'a) Field.t -> 'a list -> ('m, bool) t

  val starts_with : ('m, string) Field.t -> string -> ('m, bool) t

  val contains : ('m, string) Field.t -> string -> ('m, bool) t

  val starts_with_opt : ('m, string option) Field.t -> string -> ('m, bool) t

  val contains_opt : ('m, string option) Field.t -> string -> ('m, bool) t

  val ( && ) : ('m, bool) t -> ('m, bool) t -> ('m, bool) t

  val ( || ) : ('m, bool) t -> ('m, bool) t -> ('m, bool) t

  val not : ('m, bool) t -> ('m, bool) t
end

module Ops : OPS = struct
  open Model_ast

  let col f = {table= Field.table f; column= Field.column f}

  (* The guard of a two-valued atom: on a nullable field, SQL gives NULL for an
     atom on a NULL row, and "IS NOT NULL AND" makes it FALSE. *)
  let guard f e = if Field.nullable f then And [Is_not_null (col f); e] else e

  let ( ==. ) f v =
    match Field.encode f v with
    | Value.Null ->
        Is_null (col f)
    | x ->
        if Field.nullable f then Null_safe_eq (col f, x) else Cmp (Eq, col f, x)

  let ( <>. ) f v =
    match Field.encode f v with
    | Value.Null ->
        Is_not_null (col f)
    | x ->
        if Field.nullable f then Not (Null_safe_eq (col f, x))
        else Cmp (Ne, col f, x)

  let order op name f v =
    match Field.encode f v with
    | Value.Null ->
        invalid_arg
          (Printf.sprintf "Expr.( %s ): None on the field %s.%s" name
             (Cistern_names.quote (Field.table f))
             (Cistern_names.quote (Field.column f)) )
    | x ->
        guard f (Cmp (op, col f, x))

  let ( <. ) f v = order Lt "<." f v

  let ( >. ) f v = order Gt ">." f v

  let ( <=. ) f v = order Le "<=." f v

  let ( >=. ) f v = order Ge ">=." f v

  let is_null f = Is_null (col f)

  let in_ f vs =
    let vs = List.map (Field.encode f) vs in
    let is_null = function Value.Null -> true | _ -> false in
    let values = List.filter (fun v -> Stdlib.not (is_null v)) vs in
    match (values, List.exists is_null vs) with
    | [], false ->
        Bool false
    | [], true ->
        Is_null (col f)
    | _, false ->
        guard f (In (col f, values))
    | _, true ->
        Or [In (col f, values); Is_null (col f)]

  (* The pattern of LIKE: "!" escapes "!", "%" and "_". *)
  let escape s =
    let b = Buffer.create (String.length s) in
    String.iter
      (fun c ->
        (match c with '!' | '%' | '_' -> Buffer.add_char b '!' | _ -> ()) ;
        Buffer.add_char b c )
      s ;
    Buffer.contents b

  let like f p = guard f (Like (col f, p))

  let starts_with f s = like f (escape s ^ "%")

  let contains f s = like f ("%" ^ escape s ^ "%")

  let starts_with_opt f s = like f (escape s ^ "%")

  let contains_opt f s = like f ("%" ^ escape s ^ "%")

  let ( && ) a b = And [a; b]

  let ( || ) a b = Or [a; b]

  let not a = Not a
end
