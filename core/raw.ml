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

let redacted r =
  let b = Buffer.create 64 in
  let rec add = function
    | Sql s ->
        Buffer.add_string b s
    | V _ ->
        Buffer.add_string b " ? "
    | Cat (x, y) ->
        add x ; add y
  in
  add r ; Buffer.contents b

(* The text before the first value: the first word starts after leading white
   space, and ends at the first character that is not a letter, or at a value. A
   value before the first letter gives None. *)
let first_word r =
  let b = Buffer.create 16 in
  let exception Stop in
  let rec walk = function
    | Sql s ->
        String.iter
          (fun c ->
            match c with
            | (' ' | '\t' | '\r' | '\n') when Buffer.length b = 0 ->
                ()
            | 'A' .. 'Z' | 'a' .. 'z' ->
                Buffer.add_char b c
            | _ ->
                raise Stop )
          s
    | V _ ->
        raise Stop
    | Cat (x, y) ->
        walk x ; walk y
  in
  (try walk r with Stop -> ()) ;
  if Buffer.length b = 0 then None else Some (Buffer.contents b)

type result = {columns: string array; rows: Value.t array list}
