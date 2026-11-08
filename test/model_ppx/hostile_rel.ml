(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* The hostile relation group with SQL names (phase 4h, the plan of phase 4,
   section 4.10): the tables "group", "select" and "order by", key columns with
   a space and SQL words, relation names c, r, k, fields and decode, a child
   with two belongs_to (one with a nullable key), a self relation as a tree, a
   parent with a key that is not [@auto], a key column with a long name (a
   hashed foreign key name), and user modules List, Option and Fun before the
   group. The integration suite runs it on the three servers. *)

module List = struct
  let filter_map = ()
end

module Option = struct
  let map = ()
end

module Fun = struct
  let id = ()
end

[@@@ocaml.warning "-30"]

type ga =
  { id: int64 [@key] [@auto] [@column "a id"]
  ; fields: int [@column "select"]
  ; c: gk list Cistern.Deferred.t [@has_many] }
[@@table "group"]

and gb =
  { bk: int64 [@key] [@auto] [@column "key"]
  ; r: gk list Cistern.Deferred.t [@has_many] }
[@@table "select"]

and gk =
  { id: int64 [@key] [@auto]
  ; a_id: int64 [@column "a key"]
  ; fields: ga Cistern.Deferred.t [@belongs_to {key= a_id; references= id}]
  ; b_id: int64 option [@column "order"]
  ; decode: gb option Cistern.Deferred.t
        [@belongs_to {key= b_id; references= bk}]
  ; c: string [@column "desc"] }
[@@table "order by"]

and gn =
  { id: int64 [@key] [@auto]
  ; up: int64 option [@column "from"]
  ; k: gn option Cistern.Deferred.t [@belongs_to {key= up; references= id}]
  ; r: gn list Cistern.Deferred.t [@has_many] }
[@@table "where"]

(* A parent with a key that is not [@auto] (the review of 4h, L1), and a child
   whose key column has a long name, so that its foreign key name gets the hash
   (L2). *)
and gt =
  { code: string [@key] [@column "team code"]
  ; members: gm list Cistern.Deferred.t [@has_many] }

and gm =
  { id: int64 [@key] [@auto]
  ; t_code: string
        [@column
          "team_code_of_the_member_with_a_long_name_so_that_the_fk_is_hash"]
  ; team: gt Cistern.Deferred.t [@belongs_to {key= t_code; references= code}] }
[@@deriving cistern]
