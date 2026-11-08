(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

type empty = unit

let int64 i = Value.Int i

let int i = Value.Int (Int64.of_int i)

let text s = Value.Text s

let bool b = Value.Int (if b then 1L else 0L)

let float f = Value.Float f

let bytes y = Value.Bytes (Bytes.to_string y)

let ptime t = Value.Datetime t

let opt f = function None -> Value.Null | Some x -> f x

let cut t = Ptime.truncate ~frac_s:6 t

let cut_opt t = Option.map cut t

let some x = Some x

let set f v = Option.map (Update.set f) v

let updates l = List.filter_map Fun.id l

let reset encode old k rel =
  if Value.equal (encode old) (encode k) then rel else Deferred.Unloaded

let auto = function
  | Some k ->
      k
  | None ->
      invalid_arg "Cistern: of_draft of an [@auto] key with no key"
