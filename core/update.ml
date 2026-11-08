(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

type 'm t = Set : ('m, 'a) Field.t * 'a -> 'm t | Both : 'm t * 'm t -> 'm t

let set f v = Set (f, v)

let ( & ) a b = Both (a, b)

let rec fold : type m. ('b -> m t -> 'b) -> 'b -> m t -> 'b =
 fun g acc u ->
  match u with Set _ -> g acc u | Both (a, b) -> fold g (fold g acc a) b

let assignments u =
  let add acc = function
    | Set (f, v) ->
        let column = Field.column f in
        let name () =
          Printf.sprintf "%s.%s"
            (Cistern_names.quote (Field.table f))
            (Cistern_names.quote column)
        in
        if Field.is_key f then
          invalid_arg
            (Printf.sprintf "Update: the key %s cannot change" (name ())) ;
        if List.exists (fun (c, _) -> String.equal c column) acc then
          invalid_arg
            (Printf.sprintf "Update: the column %s is set two times" (name ())) ;
        (column, Field.encode f v) :: acc
    | Both _ ->
        acc
  in
  List.rev (fold add [] u)

let apply u r =
  ignore (assignments u) ;
  fold (fun r -> function Set (f, v) -> Field.set f r v | Both _ -> r) r u
