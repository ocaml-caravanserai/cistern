(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* The run-time rows of the hostile relation group of test/ppx/driver (phase 4g,
   the plan of phase 4, section 4.9): each scope, each Rel with include_, and
   each label of create, through Stmt.to_sql (no server). *)

open Windtrap
module R = Ppx_driver_check.Relations
module Query = Cistern.Query
module Deferred = Cistern.Deferred
module Relation = Cistern.Private.Relation

let sql s =
  String.concat "\n" (Cistern.Stmt.to_sql Cistern_core.Dialect.Mysql s)

let pure s =
  match Cistern.Stmt.Private.pure_value s with
  | Some v ->
      Some (v ())
  | None ->
      None

let a : R.a = {id= 1L; fields= 5; c= Unloaded}

let b : R.b = {bk= 2L; r= Unloaded}

let k ?b_id () : R.k =
  {id= 3L; a_id= 1L; fields= Unloaded; b_id; decode= Unloaded; c= "x"}

let n ?up id : R.n = {id; up; k= Unloaded; r= Unloaded}

let tests =
  group "the hostile relation group"
    [ test "the scopes" (fun () ->
          equal string
            "SELECT `k`.`id`, `k`.`a key`, `k`.`b_id`, `k`.`c` FROM `k` WHERE \
             `k`.`a key` = 1"
            (sql (Query.to_list (R.A.c a))) ;
          equal string
            "SELECT `k`.`id`, `k`.`a key`, `k`.`b_id`, `k`.`c` FROM `k` WHERE \
             `k`.`b_id` <=> 2"
            (sql (Query.to_list (R.B.r b))) ;
          equal string
            "SELECT `a`.`a id`, `a`.`fields` FROM `a` WHERE `a`.`a id` = 1 \
             LIMIT 1"
            (sql (R.K.fields (k ()))) ;
          is_true (pure (R.K.decode (k ())) = Some None) ;
          equal string "SELECT `b`.`bk` FROM `b` WHERE `b`.`bk` = 2 LIMIT 1"
            (sql (R.K.decode (k ~b_id:2L ()))) ;
          is_true (pure (R.N.k (n 4L)) = Some None) ;
          equal string
            "SELECT `n`.`id`, `n`.`up` FROM `n` WHERE `n`.`id` = 4 LIMIT 1"
            (sql (R.N.k (n ~up:4L 5L))) ;
          equal string "SELECT `n`.`id`, `n`.`up` FROM `n` WHERE `n`.`up` <=> 4"
            (sql (Query.to_list (R.N.r (n 4L)))) )
    ; test "each Rel with include_" (fun () ->
          equal string
            "SELECT `a`.`a id`, `a`.`fields` FROM `a`\n\
             SELECT `k`.`id`, `k`.`a key`, `k`.`b_id`, `k`.`c` FROM `k` WHERE \
             `k`.`a key` IN (<keys>) ORDER BY `k`.`id` ASC"
            (sql (R.A.all |> Query.include_ R.A.Rel.c |> Query.to_list)) ;
          equal string
            "SELECT `b`.`bk` FROM `b`\n\
             SELECT `k`.`id`, `k`.`a key`, `k`.`b_id`, `k`.`c` FROM `k` WHERE \
             `k`.`b_id` IN (<keys>) ORDER BY `k`.`id` ASC"
            (sql (R.B.all |> Query.include_ R.B.Rel.r |> Query.to_list)) ;
          equal string
            "SELECT `k`.`id`, `k`.`a key`, `k`.`b_id`, `k`.`c` FROM `k`\n\
             SELECT `a`.`a id`, `a`.`fields` FROM `a` WHERE `a`.`a id` IN \
             (<keys>)\n\
             SELECT `b`.`bk` FROM `b` WHERE `b`.`bk` IN (<keys>)"
            (sql
               ( R.K.all
               |> Query.include_ R.K.Rel.fields
               |> Query.include_ R.K.Rel.decode
               |> Query.to_list ) ) ;
          equal string
            "SELECT `n`.`id`, `n`.`up` FROM `n`\n\
             SELECT `n`.`id`, `n`.`up` FROM `n` WHERE `n`.`id` IN (<keys>)\n\
             SELECT `n`.`id`, `n`.`up` FROM `n` WHERE `n`.`up` IN (<keys>) \
             ORDER BY `n`.`id` ASC"
            (sql
               ( R.N.all |> Query.include_ R.N.Rel.k |> Query.include_ R.N.Rel.r
               |> Query.to_list ) ) )
    ; test "each label of create, and the key of each child" (fun () ->
          equal string
            "INSERT INTO `a` (`fields`) VALUES (7)\n\
             INSERT INTO `k` (`a key`, `b_id`, `c`) VALUES (<last_insert_id>, \
             NULL, _utf8mb4 X'6331')"
            (sql (R.A.create ~fields:7 ~c:[R.K.draft ~c:"c1" ()] ())) ;
          equal string
            "INSERT INTO `b` () VALUES ()\n\
             INSERT INTO `k` (`a key`, `b_id`, `c`) VALUES (1, \
             <last_insert_id>, _utf8mb4 X'6332')"
            (sql (R.B.create ~r:[R.K.draft ~a_id:1L ~c:"c2" ()] ())) ;
          equal string
            "INSERT INTO `k` (`a key`, `b_id`, `c`) VALUES (1, 2, _utf8mb4 \
             X'6333')"
            (sql (R.K.create ~a_id:1L ~b_id:2L ~c:"c3" ())) ;
          equal string
            "INSERT INTO `n` (`up`) VALUES (4)\n\
             INSERT INTO `n` (`up`) VALUES (<last_insert_id>)\n\
             INSERT INTO `n` (`up`) VALUES (<last_insert_id>)"
            (sql (R.N.create ~up:4L ~r:[R.N.draft (); R.N.draft ()] ())) ;
          (* No list: no child, and the relation stays Unloaded. *)
          equal string "INSERT INTO `n` (`up`) VALUES (NULL)"
            (sql (R.N.create ())) )
    ; test "the setters of each Rel" (fun () ->
          let parent = a in
          ( match R.K.Rel.fields with
          | Belongs_to x ->
              is_true
                ((x.set (k ()) (Some parent)).fields = Deferred.Loaded parent) ;
              (* A key that is not nullable: no parent gives Unloaded. *)
              let loaded = {(k ()) with fields= Deferred.Loaded parent} in
              is_true ((x.set loaded None).fields = Deferred.Unloaded)
          | Has_many _ ->
              fail "kind" ) ;
          ( match R.K.Rel.decode with
          | Belongs_to x ->
              is_true ((x.set (k ()) None).decode = Deferred.Loaded None)
          | Has_many _ ->
              fail "kind" ) ;
          match R.A.Rel.c with
          | Has_many x ->
              let k1 = k () and k2 = {(k ()) with id= 9L} in
              is_true ((x.set a [k1; k2]).c = Deferred.Loaded [k1; k2])
          | Belongs_to _ ->
              fail "kind" )
    ; test "the keys of each Rel on a type with two belongs_to" (fun () ->
          let key_of : type m r.
              (m, r) Relation.t -> m -> r -> Cistern.Value.t option =
           fun rel m r ->
            match rel with
            | Has_many h ->
                let (module P) = h.parent in
                Option.map (Cistern.Private.Field.encode P.key) (h.key r)
            | Belongs_to x ->
                let (module P) = x.parent in
                Option.map (Cistern.Private.Field.encode P.key) (x.key m)
          in
          let child = k ~b_id:2L () in
          let int n = Some (Cistern.Value.Int n) in
          (* a_id is 1 and b_id is 2: each relation reads its own key. *)
          is_true (key_of R.B.Rel.r b child = int 2L) ;
          is_true (key_of R.A.Rel.c a child = int 1L) ;
          is_true (key_of R.K.Rel.decode child b = int 2L) ;
          is_true (key_of R.K.Rel.fields child a = int 1L) ;
          is_true (key_of R.K.Rel.decode (k ()) b = None) )
    ; test "the reset of each key of a type with two belongs_to" (fun () ->
          let both =
            { (k ~b_id:2L ()) with
              fields= Deferred.Loaded a
            ; decode= Deferred.Loaded (Some b) }
          in
          let set_b = Cistern.Private.Field.set R.K.Q.b_id both (Some 3L) in
          is_true (set_b.decode = Deferred.Unloaded) ;
          is_true (set_b.fields = Deferred.Loaded a) ;
          let set_a = Cistern.Private.Field.set R.K.Q.a_id both 7L in
          is_true (set_a.fields = Deferred.Unloaded) ;
          is_true (set_a.decode = Deferred.Loaded (Some b)) )
    ; test
        "two has_many to one child type: the setters and the groups of create"
        (fun () ->
          let p : R.p = {id= 1L; x1= Unloaded; x2= Unloaded} in
          let q v : R.q = {id= Int64.of_int v; p_id= 1L; p= Unloaded; v} in
          ( match (R.P.Rel.x1, R.P.Rel.x2) with
          | Has_many h1, Has_many h2 ->
              let p1 = h1.set p [q 1] and p2 = h2.set p [q 2; q 3] in
              is_true
                (p1.x1 = Deferred.Loaded [q 1] && p1.x2 = Deferred.Unloaded) ;
              is_true
                (p2.x2 = Deferred.Loaded [q 2; q 3] && p2.x1 = Deferred.Unloaded)
          | _ ->
              fail "kind" ) ;
          (* The groups in the order of the labels: x1, then x2. *)
          equal string
            "INSERT INTO `p` () VALUES ()\n\
             INSERT INTO `q` (`p_id`, `v`) VALUES (<last_insert_id>, 1)\n\
             INSERT INTO `q` (`p_id`, `v`) VALUES (<last_insert_id>, 2)\n\
             INSERT INTO `q` (`p_id`, `v`) VALUES (<last_insert_id>, 3)"
            (sql
               (R.P.create
                  ~x1:[R.Q.draft ~v:1 ()]
                  ~x2:[R.Q.draft ~v:2 (); R.Q.draft ~v:3 ()]
                  () ) ) ;
          equal string
            "INSERT INTO `p` () VALUES ()\n\
             INSERT INTO `q` (`p_id`, `v`) VALUES (<last_insert_id>, 2)"
            (sql (R.P.create ~x2:[R.Q.draft ~v:2 ()] ())) )
    ; test "filter_by of a nullable belongs_to key" (fun () ->
          equal string
            "SELECT `k`.`id`, `k`.`a key`, `k`.`b_id`, `k`.`c` FROM `k` WHERE \
             `k`.`b_id` <=> 2"
            (sql (Query.to_list (R.K.filter_by_b_id (Some 2L)))) ;
          equal string "SELECT `n`.`id`, `n`.`up` FROM `n` WHERE `n`.`up` <=> 4"
            (sql (Query.to_list (R.N.filter_by_up (Some 4L)))) )
    ; test
        "a coded parent with no key of its belongs_to, with children: \
         Invalid_value before the checkout" (fun () ->
          let s = R.Team.create ~code:"t" ~members:[R.Member.draft ()] () in
          is_true
            ( (Cistern.Stmt.Private.meta s).text
            = Some (Error "Cistern: of_draft of a belongs_to key with no key")
            ) ;
          equal string
            "INSERT INTO `team` (`code`, `o_id`) VALUES (_utf8mb4 X'74', NULL)\n\
             <Invalid_value: Cistern: of_draft of a belongs_to key with no key>"
            (sql s) ;
          (* With the key: the key of the draft in each child. *)
          equal string
            "INSERT INTO `team` (`code`, `o_id`) VALUES (_utf8mb4 X'74', 1)\n\
             INSERT INTO `member` (`team_code`) VALUES (_utf8mb4 X'74')"
            (sql
               (R.Team.create ~code:"t" ~o_id:1L
                  ~members:[R.Member.draft ()]
                  () ) ) ) ]
