(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

type t = Sql of string | V of Value.t | Cat of t * t

let sql s = Sql s

let v x = V x

let ( ++ ) a b = Cat (a, b)

exception No_literal of string

let to_string r =
  let b = Buffer.create 64 in
  let rec add = function
    | Sql s ->
        Buffer.add_string b s
    | V x -> (
        Buffer.add_char b ' ' ;
        match Literal.add b x with
        | Ok () ->
            Buffer.add_char b ' '
        | Error m ->
            raise (No_literal m) )
    | Cat (x, y) ->
        add x ; add y
  in
  match add r with
  | () ->
      Ok (Buffer.contents b)
  | exception No_literal m ->
      Error m

type result = {columns: string array; rows: Value.t array list}
