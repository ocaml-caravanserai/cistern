(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* The suite of the OCaml 5.5 check (test/ocaml55/dune). *)

open Windtrap

module User = struct
  type t = {id: int64}

  type key = int64

  let table = "user"

  let key u = u.id

  let key_to_string = Int64.to_string
end

let users = List.map (fun id -> {User.id}) [7L; 8L; 9L]

let tests =
  [ test "describe gives the table and the key" (fun () ->
        equal string "user/7" (Ocaml55.describe (module User) (List.hd users)) )
  ; test "count counts the keys that satisfy the predicate" (fun () ->
        equal int 2 (Ocaml55.count (module User) (fun k -> k > 7L) users) )
  ; test "sum_lengths adds the two lengths" (fun () ->
        equal int 5 (Ocaml55.sum_lengths List.length [1; 2] ["a"; "b"; "c"]) )
  ]

let () = exit (Windtrap.run "ocaml55" tests)
