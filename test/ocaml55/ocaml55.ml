(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

module type MODEL = sig
  type t

  type key

  val table : string

  val key : t -> key

  val key_to_string : key -> string
end

(* A module-dependent function needs a full type annotation (DESIGN.md,
   "Generated code"). *)
let describe : (module M : MODEL) -> M.t -> string =
 fun (module M) v ->
  let key = M.key_to_string (M.key v) in
  if key = "" then M.table else M.table ^ "/" ^ key

let count : (module M : MODEL) -> (M.key -> bool) -> M.t list -> int =
 fun (module M) p vs ->
  List.fold_left (fun n v -> if p (M.key v) then n + 1 else n) 0 vs

let sum_lengths (f : 'a. 'a list -> int) xs ys = f xs + f ys
