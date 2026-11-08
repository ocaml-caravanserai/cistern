(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* The hostile names of D17 (the plan of phase 3): user modules List, Option,
   Fun, Q and M before the groups, a type unit, field names that are local names
   or module values of the generated code, and type names that are short. The
   library compiles with each warning as an error but 70. *)

module List = struct
  let filter_map = ()
end

module Option = struct
  let map = ()
end

module Fun = struct
  let id = ()
end

module Q = struct
  let v = 7

  let d = "q"
end

type t =
  { c: int64 [@key] [@auto]
  ; r: string [@unique]
  ; v: int [@default Q.v]
  ; k: string option [@index]
  ; d: string [@default Q.d]
  ; e: bool }

and s =
  { draft: int64 [@key]
  ; schema: string [@unique]
  ; s_name: string [@column "name"]
  ; name: string option [@column "s_name"] [@unique]
  ; model: int [@index] }

and f = {update: string [@key]; key: int; all: int option [@default None]}

and d = {where: int64 [@key] [@auto]; delete: int}

and m = {fields: int64 [@key] [@auto]; decode: string}

and u = {cistern: int64 [@key] [@auto]} [@@deriving cistern]

(* Each stage-2 type t is the type of its own group member. *)
let _t (x : T.t) : t = x

let _s (x : S.t) : s = x

let _f (x : F.t) : f = x

let _d (x : D.t) : d = x

let _m (x : M.t) : m = x

let _u (x : U.t) : u = x

(* A user type unit after the groups does not change the empty draft of u. *)
type nonrec unit = int

let (_ : U.draft) = U.draft ()

(* Two thunks with two numbers (D10): a type a_ with a field b and a type a with
   a field _b. *)
type a_ = {id: int64 [@key]; b: int [@default 1]}

and a = {key_a: int64 [@key]; _b: int [@default 2]} [@@deriving cistern]
