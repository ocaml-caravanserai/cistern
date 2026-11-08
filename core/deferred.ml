(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

type 'a t = Unloaded | Loaded of 'a

let get = function Loaded v -> Some v | Unloaded -> None

let get_exn = function
  | Loaded v ->
      v
  | Unloaded ->
      invalid_arg "Deferred.get_exn: the relation is not loaded"
