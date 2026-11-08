(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* The description and the models of the groups that the checks accept (the plan
   of phase 3, 4.2). *)

open Windtrap

let person =
  {|type person = {
  id : int64; [@key] [@auto]
  email : string; [@unique]
  nick : string option; [@unique] [@column "nick_name"]
  city : string; [@index]
  age : int option;
  active : bool; [@default true]
  score : float;
  photo : bytes option;
  born : Ptime.t;
}
and tag = {
  name : string; [@key]
  note : string option [@default Some "none"]
}
[@@table "tags"] [@@deriving cistern]|}

let long_forms =
  {|type t = {
  id : int; [@cistern.key]
  c : string; [@cistern.column "cc"] [@cistern.unique]
  d : int [@cistern.default 1 + 2] [@cistern.index]
} [@@cistern.table "tt"] [@@deriving cistern]|}

let other_namespaces =
  {|type t = {
  id : int64; [@key] [@yojson.key "x"] [@kye.x] [@ocaml.doc "d"]
  v : int [@deprecated "d"] [@yojson.uniqe]
} [@@ocaml.warning "-69"] [@@deriving cistern]|}

let names =
  {|type userAccount = { id : int64; [@key] café : string; draft : int; q : int; schema : int }
[@@table "user_account"] [@@deriving cistern]|}

(* The names of the hostile group of D17. *)
let hostile =
  {|type t = { id : int64 [@key] }
and s = { id : int64; [@key] t : int64 }
and f = { id : int64 [@key] }
and d = { id : int64 [@key] }
and m = { id : int64 [@key] }
and u = { id : int64 [@key] } [@@deriving cistern]|}

(* A trailing underscore, and bare names at a distance of 3 from each Cistern
   name. *)
let accepted =
  {|type t_ = { id : int64; [@key] v : int [@ab] [@keyy_x] } [@@deriving cistern]|}

(* Short bare names of other derivers (A-L1 of review A of 3a), other names at a
   distance of 2 from key or of 3 from unique, and a dotted name near a Cistern
   name: the near-miss check passes them. *)
let short_names =
  {|type t = { id : int64; [@key] v : int [@gen] [@sep] [@keep] [@hex] [@at] [@x.key] [@y] [@abkey] [@kae] [@uni] [@x.index] } [@@deriving cistern]|}

(* An attribute on its own line after the ";" of a field goes to that field. *)
let own_line =
  {|type t = {
  id : int64;
  [@key]
  name : string;
} [@@deriving cistern]|}

(* Two types and fields whose names would give one thunk name, if the name came
   from the type and the field (D10). *)
let thunks =
  {|type a_ = { id : int64; [@key] b : int [@default 1] }
and a = { id : int64; [@key] _b : int [@default 2] } [@@deriving cistern]|}

let tests =
  group "the description and the models"
    [ test "a group with each attribute" (fun () ->
          expect (Src.desc person)
          @@ __POS_OF__
               {|
            type person (line 1, characters 5-11)
              0 id (line 2, characters 2-4): int64 key auto
              1 email (line 3, characters 2-7): string unique
              2 nick (line 4, characters 2-6): string option unique column nick_name (line 4, characters 34-55)
              3 city (line 5, characters 2-6): string index
              4 age (line 6, characters 2-5): int option
              5 active (line 7, characters 2-8): bool default true (line 7, characters 27-31)
              6 score (line 8, characters 2-7): float
              7 photo (line 9, characters 2-7): bytes option
              8 born (line 10, characters 2-6): Ptime.t
            type tag (line 12, characters 4-7) table tags (line 16, characters 0-16)
              9 name (line 13, characters 2-6): string key
              10 note (line 14, characters 2-6): string option default Some "none" (line 14, characters 33-44)
            |} ;
          expect (Src.schema person)
          @@ __POS_OF__
               {|
            person table person, 0 relations
              id column id: int64 key auto
              email column email: string unique
              nick column nick_name: string option unique
              city column city: string index
              age column age: int option
              active column active: bool
              score column score: float
              photo column photo: bytes option
              born column born: Ptime.t
            tag table tags, 0 relations
              name column name: string key
              note column note: string option
            |} ;
          expect (Src.error person) @@ __POS_OF__ {| no error |} )
    ; test "the long forms of the attributes" (fun () ->
          expect (Src.desc long_forms)
          @@ __POS_OF__
               {|
            type t (line 1, characters 5-6) table tt (line 5, characters 2-24)
              0 id (line 2, characters 2-4): int key
              1 c (line 3, characters 2-3): string unique column cc (line 3, characters 14-36)
              2 d (line 4, characters 2-3): int index default 1 + 2 (line 4, characters 28-33)
            |} ;
          expect (Src.schema long_forms)
          @@ __POS_OF__
               {|
            t table tt, 0 relations
              id column id: int key
              c column cc: string unique
              d column d: int index
            |} )
    ; test "the attributes of other namespaces are not examined" (fun () ->
          expect (Src.desc other_namespaces)
          @@ __POS_OF__
               {|
            type t (line 1, characters 5-6)
              0 id (line 2, characters 2-4): int64 key
              1 v (line 3, characters 2-3): int
            |} )
    ; test "an uppercase type name with [@@table], and non-ASCII field names"
        (fun () ->
          expect (Src.desc names)
          @@ __POS_OF__
               {|
            type userAccount (line 1, characters 5-16) table user_account (line 2, characters 0-24)
              0 id (line 1, characters 21-23): int64 key
              1 café (line 1, characters 40-45): string
              2 draft (line 1, characters 56-61): int
              3 q (line 1, characters 69-70): int
              4 schema (line 1, characters 78-84): int
            |} ;
          expect (Src.schema names)
          @@ __POS_OF__
               {|
            userAccount table user_account, 0 relations
              id column id: int64 key
              café column café: string
              draft column draft: int
              q column q: int
              schema column schema: int
            |} )
    ; test "the names of the hostile group" (fun () ->
          expect (Src.error hostile) @@ __POS_OF__ {| no error |} )
    ; test "the numbers of the default thunks" (fun () ->
          expect (Src.desc thunks)
          @@ __POS_OF__
               {|
            type a_ (line 1, characters 5-7)
              0 id (line 1, characters 12-14): int64 key
              1 b (line 1, characters 31-32): int default 1 (line 1, characters 49-50)
            type a (line 2, characters 4-5)
              2 id (line 2, characters 10-12): int64 key
              3 _b (line 2, characters 29-31): int default 2 (line 2, characters 48-49)
            |} )
    ; test "short bare names and a dotted name" (fun () ->
          expect (Src.desc short_names)
          @@ __POS_OF__
               {|
            type t (line 1, characters 5-6)
              0 id (line 1, characters 11-13): int64 key
              1 v (line 1, characters 30-31): int
            |} )
    ; test "an attribute on its own line goes to the field before it" (fun () ->
          expect (Src.desc own_line)
          @@ __POS_OF__
               {|
            type t (line 1, characters 5-6)
              0 id (line 2, characters 2-4): int64 key
              1 name (line 4, characters 2-6): string
            |} )
    ; test "a fault of the list with fewer than two types goes to the group"
        (fun () ->
          let loc, _, tds = Src.group own_line in
          let g = Src.Desc.group ~loc tds in
          let fault models =
            { Src.Schema.model= None
            ; models
            ; fields= []
            ; relation= None
            ; table= false
            ; text= "text" }
          in
          let at models =
            match Src.Desc.place ~loc g (fault models) with
            | _ ->
                "no error"
            | exception Ppxlib.Location.Error e ->
                Src.place (Ppxlib.Location.Error.get_location e)
          in
          equal string (Src.place loc) (at ["t"]) ;
          equal string (Src.place loc) (at []) ;
          equal string (Src.place loc) (at ["t"; "u"]) )
    ; test "names that the checks accept" (fun () ->
          expect (Src.desc accepted)
          @@ __POS_OF__
               {|
            type t_ (line 1, characters 5-7)
              0 id (line 1, characters 12-14): int64 key
              1 v (line 1, characters 31-32): int
            |} )
    ; test "the expander marks each Cistern attribute as seen" (fun () ->
          expect (Src.unseen person)
          @@ __POS_OF__ {| Attribute `deriving' was silently dropped |} ;
          expect (Src.unseen long_forms)
          @@ __POS_OF__ {| Attribute `deriving' was silently dropped |} ;
          expect (Src.unseen other_namespaces)
          @@ __POS_OF__
               {|
            Attribute `deprecated' was silently dropped
            Attribute `deriving' was silently dropped
            Attribute `kye.x' was silently dropped
            Attribute `ocaml.doc' was silently dropped
            Attribute `ocaml.warning' was silently dropped
            Attribute `yojson.key' was silently dropped
            Attribute `yojson.uniqe' was silently dropped
            |} )
    ]
