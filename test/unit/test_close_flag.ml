(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* The closed flag of Connection.abort and Connection.close: true only for the
   first claim, also across domains (review of PR 1.2b, L1). *)

open Windtrap
module C = Cistern_mysql.Connection

let tests =
  group "the closed flag of a connection"
    [ test "the first claim gives true, the next ones false" (fun () ->
          let a = Atomic.make false in
          equal (list bool) [true; false; false]
            (List.map (fun _ -> C.Private.claim a) [1; 2; 3]) )
    ; test "two domains: exactly one true for each flag" (fun () ->
          (* Each round, two domains claim the same new flag at the same
             time. *)
          let rounds = 200 in
          let flags = Array.init rounds (fun _ -> Atomic.make false) in
          let go = Atomic.make false in
          let claim_all () =
            while not (Atomic.get go) do
              Domain.cpu_relax ()
            done ;
            Array.map C.Private.claim flags
          in
          let d1 = Domain.spawn claim_all and d2 = Domain.spawn claim_all in
          Atomic.set go true ;
          let r1 = Domain.join d1 and r2 = Domain.join d2 in
          let trues =
            Array.to_list
              (Array.mapi (fun i b -> Bool.to_int b + Bool.to_int r2.(i)) r1)
          in
          equal (list int) (List.init rounds (fun _ -> 1)) trues ) ]
