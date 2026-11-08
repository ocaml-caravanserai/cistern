(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* The errors of the relation fields (phase 4f, the plan of phase 4, E12 and
   E15), each with its line and its characters. *)

open Windtrap

let row name src expected =
  test name (fun () -> equal string expected (Src.error src))

let tests =
  group "the errors of relations"
    [ row "a payload that is not a record"
        {|type t = { id : int64; [@key] u_id : int64; u : u Deferred.t [@belongs_to "x"] }
and u = { id : int64 [@key] } [@@deriving cistern]|}
        "line 1, characters 61-78: [@@deriving cistern] [@belongs_to] on the \
         field \"u\" of the type \"t\" takes the record { key = <field>; \
         references = <field> }"
    ; row "a qualified value"
        {|type t = { id : int64; [@key] u_id : int64; u : u Deferred.t [@belongs_to { key = M.u_id; references = id }] }
and u = { id : int64 [@key] } [@@deriving cistern]|}
        "line 1, characters 82-88: [@@deriving cistern] [@belongs_to] on the \
         field \"u\" of the type \"t\": the value of key must be the name of a \
         field"
    ; row "a payload with with"
        {|type t = { id : int64; [@key] u_id : int64; u : u Deferred.t [@belongs_to { r with key = u_id; references = id }] }
and u = { id : int64 [@key] } [@@deriving cistern]|}
        "line 1, characters 76-77: [@@deriving cistern] [@belongs_to] on the \
         field \"u\" of the type \"t\" takes the record { key = <field>; \
         references = <field> }, with no \"with\""
    ; row "an unknown label"
        {|type t = { id : int64; [@key] u_id : int64; u : u Deferred.t [@belongs_to { key = u_id; references = id; on = x }] }
and u = { id : int64 [@key] } [@@deriving cistern]|}
        "line 1, characters 105-107: [@@deriving cistern] [@belongs_to] on the \
         field \"u\" of the type \"t\" has an unknown label: it takes the \
         record { key = <field>; references = <field> }"
    ; row "a label two times"
        {|type t = { id : int64; [@key] u_id : int64; u : u Deferred.t [@belongs_to { key = u_id; key = u_id; references = id }] }
and u = { id : int64 [@key] } [@@deriving cistern]|}
        "line 1, characters 88-91: [@@deriving cistern] [@belongs_to] on the \
         field \"u\" of the type \"t\" has the label key two times"
    ; row "no label references"
        {|type t = { id : int64; [@key] u_id : int64; u : u Deferred.t [@belongs_to { key = u_id }] }
and u = { id : int64 [@key] } [@@deriving cistern]|}
        "line 1, characters 61-89: [@@deriving cistern] [@belongs_to] on the \
         field \"u\" of the type \"t\" has no label references: it takes the \
         record { key = <field>; references = <field> }"
    ; row "no label key"
        {|type t = { id : int64; [@key] u_id : int64; u : u Deferred.t [@belongs_to { references = id }] }
and u = { id : int64 [@key] } [@@deriving cistern]|}
        "line 1, characters 61-94: [@@deriving cistern] [@belongs_to] on the \
         field \"u\" of the type \"t\" has no label key: it takes the record { \
         key = <field>; references = <field> }"
    ; row "a value that is not a field name"
        {|type t = { id : int64; [@key] u_id : int64; u : u Deferred.t [@belongs_to { key = "u_id"; references = id }] }
and u = { id : int64 [@key] } [@@deriving cistern]|}
        "line 1, characters 82-88: [@@deriving cistern] [@belongs_to] on the \
         field \"u\" of the type \"t\": the value of key must be the name of a \
         field"
    ; row "a qualified label"
        {|type t = { id : int64; [@key] u_id : int64; u : u Deferred.t [@belongs_to { M.key = u_id; references = id }] }
and u = { id : int64 [@key] } [@@deriving cistern]|}
        "line 1, characters 76-81: [@@deriving cistern] [@belongs_to] on the \
         field \"u\" of the type \"t\" has an unknown label: it takes the \
         record { key = <field>; references = <field> }"
    ; row "[@has_many] with a payload"
        {|type t = { id : int64; [@key] us : u list Deferred.t [@has_many 1] }
and u = { id : int64 [@key] } [@@deriving cistern]|}
        "line 1, characters 53-66: [@@deriving cistern] [@has_many] on the \
         field \"us\" of the type \"t\" takes no payload"
    ; row "[@has_many] on <t> Deferred.t"
        {|type t = { id : int64; [@key] us : u Deferred.t [@has_many] }
and u = { id : int64 [@key] } [@@deriving cistern]|}
        "line 1, characters 35-47: [@@deriving cistern] [@has_many] on the \
         field \"us\" of the type \"t\": the type of the field must be <t> \
         list Deferred.t, with <t> a type of the group"
    ; row "[@has_many] on <t> list"
        {|type t = { id : int64; [@key] us : u list [@has_many] }
and u = { id : int64 [@key] } [@@deriving cistern]|}
        "line 1, characters 35-41: [@@deriving cistern] [@has_many] on the \
         field \"us\" of the type \"t\": the type of the field must be <t> \
         list Deferred.t, with <t> a type of the group"
    ; row "[@has_many] on int"
        {|type t = { id : int64; [@key] us : int [@has_many] }
and u = { id : int64 [@key] } [@@deriving cistern]|}
        "line 1, characters 35-38: [@@deriving cistern] [@has_many] on the \
         field \"us\" of the type \"t\": the type of the field must be <t> \
         list Deferred.t, with <t> a type of the group"
    ; row "[@belongs_to] on <t> list Deferred.t"
        {|type t = { id : int64; [@key] u_id : int64; u : u list Deferred.t [@belongs_to { key = u_id; references = id }] }
and u = { id : int64 [@key] } [@@deriving cistern]|}
        "line 1, characters 48-65: [@@deriving cistern] [@belongs_to] on the \
         field \"u\" of the type \"t\": the type of the field must be <t> \
         Deferred.t or <t> option Deferred.t, with <t> a type of the group"
    ; row "[@has_many] on <t> list X.Deferred.t"
        {|type t = { id : int64; [@key] us : u list X.Deferred.t [@has_many] }
and u = { id : int64 [@key] } [@@deriving cistern]|}
        "line 1, characters 35-54: [@@deriving cistern] [@has_many] on the \
         field \"us\" of the type \"t\": the type of the field must be <t> \
         list Deferred.t, with <t> a type of the group"
    ; row "[@belongs_to] on <t> Lazy.t"
        {|type t = { id : int64; [@key] u_id : int64; u : u Lazy.t [@belongs_to { key = u_id; references = id }] }
and u = { id : int64 [@key] } [@@deriving cistern]|}
        "line 1, characters 48-56: [@@deriving cistern] [@belongs_to] on the \
         field \"u\" of the type \"t\": the type of the field must be <t> \
         Deferred.t or <t> option Deferred.t, with <t> a type of the group"
    ; row "[@belongs_to] on int"
        {|type t = { id : int64; [@key] u_id : int64; u : int [@belongs_to { key = u_id; references = id }] }
and u = { id : int64 [@key] } [@@deriving cistern]|}
        "line 1, characters 48-51: [@@deriving cistern] [@belongs_to] on the \
         field \"u\" of the type \"t\": the type of the field must be <t> \
         Deferred.t or <t> option Deferred.t, with <t> a type of the group"
    ; row "a has_many to a type outside the group"
        {|type t = { id : int64; [@key] us : v list Deferred.t [@has_many] }
and u = { id : int64 [@key] } [@@deriving cistern]|}
        "line 1, characters 35-36: [@@deriving cistern] [@has_many] on the \
         field \"us\" of the type \"t\": the type \"v\" is not a type of the \
         group"
    ; row "a belongs_to to a type outside the group"
        {|type t = { id : int64; [@key] u_id : int64; u : v Deferred.t [@belongs_to { key = u_id; references = id }] }
and u = { id : int64 [@key] } [@@deriving cistern]|}
        "line 1, characters 48-49: [@@deriving cistern] [@belongs_to] on the \
         field \"u\" of the type \"t\": the type \"v\" is not a type of the \
         group"
    ; row "[@has_many] and [@belongs_to] on one field"
        {|type t = { id : int64; [@key] u_id : int64; u : u Deferred.t [@has_many] [@belongs_to { key = u_id; references = id }] }
and u = { id : int64 [@key] } [@@deriving cistern]|}
        "line 1, characters 75-85: [@@deriving cistern] the field \"u\" of the \
         type \"t\" has [@has_many] and [@belongs_to]"
    ; row "a type of the group with no relation attribute"
        {|type t = { id : int64; [@key] u : u }
and u = { id : int64 [@key] } [@@deriving cistern]|}
        "line 1, characters 34-35: [@@deriving cistern] the field \"u\" of the \
         type \"t\" has a type of the group or a Deferred.t, with no relation \
         attribute: give [@has_many] or [@belongs_to]"
    ; row "an option of a type of the group"
        {|type t = { id : int64; [@key] u : u option }
and u = { id : int64 [@key] } [@@deriving cistern]|}
        "line 1, characters 34-42: [@@deriving cistern] the field \"u\" of the \
         type \"t\" has a type of the group or a Deferred.t, with no relation \
         attribute: give [@has_many] or [@belongs_to]"
    ; row "a Deferred.t field with no relation attribute"
        {|type t = { id : int64; [@key] us : u list Deferred.t }
and u = { id : int64 [@key] } [@@deriving cistern]|}
        "line 1, characters 35-52: [@@deriving cistern] the field \"us\" of \
         the type \"t\" has a type of the group or a Deferred.t, with no \
         relation attribute: give [@has_many] or [@belongs_to]"
    ; row "a Cistern.Deferred.t field with no relation attribute"
        {|type t = { id : int64; [@key] us : u list Cistern.Deferred.t }
and u = { id : int64 [@key] } [@@deriving cistern]|}
        "line 1, characters 35-60: [@@deriving cistern] the field \"us\" of \
         the type \"t\" has a type of the group or a Deferred.t, with no \
         relation attribute: give [@has_many] or [@belongs_to]"
    ; row "the relation name all"
        {|type t = { id : int64; [@key] all : u list Deferred.t [@has_many] }
and u = { id : int64 [@key] } [@@deriving cistern]|}
        "line 1, characters 30-33: [@@deriving cistern] the name of the field \
         \"all\" of the type \"t\", a relation field, is a value of the model \
         module (all, where, update, delete, draft, create, model, get_by_*, \
         find_by_*, filter_by_*)"
    ; row "the relation name create"
        {|type t = { id : int64; [@key] create : u list Deferred.t [@has_many] }
and u = { id : int64 [@key] } [@@deriving cistern]|}
        "line 1, characters 30-36: [@@deriving cistern] the name of the field \
         \"create\" of the type \"t\", a relation field, is a value of the \
         model module (all, where, update, delete, draft, create, model, \
         get_by_*, find_by_*, filter_by_*)"
    ; row "the relation name model"
        {|type t = { id : int64; [@key] model : u list Deferred.t [@has_many] }
and u = { id : int64 [@key] } [@@deriving cistern]|}
        "line 1, characters 30-35: [@@deriving cistern] the name of the field \
         \"model\" of the type \"t\", a relation field, is a value of the \
         model module (all, where, update, delete, draft, create, model, \
         get_by_*, find_by_*, filter_by_*)"
    ; row "the relation name get_by_x"
        {|type t = { id : int64; [@key] get_by_x : u list Deferred.t [@has_many] }
and u = { id : int64 [@key] } [@@deriving cistern]|}
        "line 1, characters 30-38: [@@deriving cistern] the name of the field \
         \"get_by_x\" of the type \"t\", a relation field, is a value of the \
         model module (all, where, update, delete, draft, create, model, \
         get_by_*, find_by_*, filter_by_*)"
    ; row "the relation name find_by_x"
        {|type t = { id : int64; [@key] find_by_x : u list Deferred.t [@has_many] }
and u = { id : int64 [@key] } [@@deriving cistern]|}
        "line 1, characters 30-39: [@@deriving cistern] the name of the field \
         \"find_by_x\" of the type \"t\", a relation field, is a value of the \
         model module (all, where, update, delete, draft, create, model, \
         get_by_*, find_by_*, filter_by_*)"
    ; row "the relation name filter_by_x"
        {|type t = { id : int64; [@key] filter_by_x : u list Deferred.t [@has_many] }
and u = { id : int64 [@key] } [@@deriving cistern]|}
        "line 1, characters 30-41: [@@deriving cistern] the name of the field \
         \"filter_by_x\" of the type \"t\", a relation field, is a value of \
         the model module (all, where, update, delete, draft, create, model, \
         get_by_*, find_by_*, filter_by_*)"
    ; row "the relation name where"
        {|type t = { id : int64; [@key] where : u list Deferred.t [@has_many] }
and u = { id : int64 [@key] } [@@deriving cistern]|}
        "line 1, characters 30-35: [@@deriving cistern] the name of the field \
         \"where\" of the type \"t\", a relation field, is a value of the \
         model module (all, where, update, delete, draft, create, model, \
         get_by_*, find_by_*, filter_by_*)"
    ; row "the relation name update"
        {|type t = { id : int64; [@key] update : u list Deferred.t [@has_many] }
and u = { id : int64 [@key] } [@@deriving cistern]|}
        "line 1, characters 30-36: [@@deriving cistern] the name of the field \
         \"update\" of the type \"t\", a relation field, is a value of the \
         model module (all, where, update, delete, draft, create, model, \
         get_by_*, find_by_*, filter_by_*)"
    ; row "the relation name delete"
        {|type t = { id : int64; [@key] delete : u list Deferred.t [@has_many] }
and u = { id : int64 [@key] } [@@deriving cistern]|}
        "line 1, characters 30-36: [@@deriving cistern] the name of the field \
         \"delete\" of the type \"t\", a relation field, is a value of the \
         model module (all, where, update, delete, draft, create, model, \
         get_by_*, find_by_*, filter_by_*)"
    ; row "the relation name draft"
        {|type t = { id : int64; [@key] draft : u list Deferred.t [@has_many] }
and u = { id : int64 [@key] } [@@deriving cistern]|}
        "line 1, characters 30-35: [@@deriving cistern] the name of the field \
         \"draft\" of the type \"t\", a relation field, is a value of the \
         model module (all, where, update, delete, draft, create, model, \
         get_by_*, find_by_*, filter_by_*)"
    ; row "the relation name contains (Expr.OPS)"
        {|type t = { id : int64; [@key] contains : u list Deferred.t [@has_many] }
and u = { id : int64 [@key] } [@@deriving cistern]|}
        "line 1, characters 30-38: [@@deriving cistern] the name of the field \
         \"contains\" of the type \"t\" is a name of the filters of Q \
         (Expr.OPS)"
    ; row "a relation name with cistern__"
        {|type t = { id : int64; [@key] cistern__us : u list Deferred.t [@has_many] }
and u = { id : int64 [@key] } [@@deriving cistern]|}
        "line 1, characters 30-41: [@@deriving cistern] the name of the field \
         \"cistern__us\" of the type \"t\" starts with \"cistern__\", the \
         prefix of the generated names"
    ; row "a mutable relation field"
        {|type t = { id : int64; [@key] mutable us : u list Deferred.t [@has_many] }
and u = { id : int64 [@key] } [@@deriving cistern]|}
        "line 1, characters 38-40: [@@deriving cistern] the field \"us\" of \
         the type \"t\" is mutable: a mutable field is not supported \
         (Field.set gives a copy)"
    ; row "[@default] on a relation field"
        {|type t = { id : int64; [@key] us : u list Deferred.t [@has_many] [@default Unloaded] }
and u = { id : int64 [@key] } [@@deriving cistern]|}
        "line 1, characters 67-74: [@@deriving cistern] [@default] on the \
         field \"us\" of the type \"t\", a relation field: a relation field is \
         not a column"
    ; row "[@column] on a relation field"
        {|type t = { id : int64; [@key] us : u list Deferred.t [@has_many] [@column "x"] }
and u = { id : int64 [@key] } [@@deriving cistern]|}
        "line 1, characters 67-73: [@@deriving cistern] [@column] on the field \
         \"us\" of the type \"t\", a relation field: a relation field is not a \
         column"
    ; row "[@key] on a relation field"
        {|type t = { id : int64; [@key] us : u list Deferred.t [@has_many] [@key] }
and u = { id : int64 [@key] } [@@deriving cistern]|}
        "line 1, characters 67-70: [@@deriving cistern] [@key] on the field \
         \"us\" of the type \"t\", a relation field: a relation field is not a \
         column"
    ; row "[@unique] on a relation field"
        {|type t = { id : int64; [@key] us : u list Deferred.t [@has_many] [@unique] }
and u = { id : int64 [@key] } [@@deriving cistern]|}
        "line 1, characters 67-73: [@@deriving cistern] [@unique] on the field \
         \"us\" of the type \"t\", a relation field: a relation field is not a \
         column"
    ; row "a nullable key and <t> Deferred.t"
        {|type t = { id : int64; [@key] u_id : int64 option; u : u Deferred.t [@belongs_to { key = u_id; references = id }] }
and u = { id : int64 [@key] } [@@deriving cistern]|}
        "line 1, characters 55-67: [@@deriving cistern] the relation field \
         \"u\" of the type \"t\": the key \"u_id\" is an option, thus the type \
         is <t> option Deferred.t"
    ; row "a key that is not an option and <t> option Deferred.t"
        {|type t = { id : int64; [@key] u_id : int64; u : u option Deferred.t [@belongs_to { key = u_id; references = id }] }
and u = { id : int64 [@key] } [@@deriving cistern]|}
        "line 1, characters 48-67: [@@deriving cistern] the relation field \
         \"u\" of the type \"t\": the key \"u_id\" is not an option, thus the \
         type is <t> Deferred.t"
    ; row "rule 8: the key is not a field"
        {|type t = { id : int64; [@key] u_id : int64; u : u Deferred.t [@belongs_to { key = nope; references = id }] }
and u = { id : int64 [@key] } [@@deriving cistern]|}
        "line 1, characters 61-106: [@@deriving cistern] model \"t\", relation \
         \"u\": the key \"nope\" is not a field of the model"
    ; row "rule 8: references is not the key"
        {|type t = { id : int64; [@key] u_id : int64; u : u Deferred.t [@belongs_to { key = u_id; references = n }] }
and u = { id : int64; [@key] n : int64 } [@@deriving cistern]|}
        "line 1, characters 61-105: [@@deriving cistern] model \"t\", relation \
         \"u\": references \"n\" is not the key \"id\" of \"u\" (version 1 \
         refers only to the key)"
    ; row "rule 8: the types of the key and the reference differ"
        {|type t = { id : int64; [@key] u_id : string; u : u Deferred.t [@belongs_to { key = u_id; references = id }] }
and u = { id : int64 [@key] } [@@deriving cistern]|}
        "line 1, characters 62-107: [@@deriving cistern] model \"t\", relation \
         \"u\": the key \"u_id\" and the field \"id\" of \"u\" have different \
         types"
    ; row "rule 8: a self belongs_to on the [@key]"
        {|type t = { id : int64; [@key] p : t Deferred.t [@belongs_to { key = id; references = id }] } [@@deriving cistern]|}
        "line 1, characters 47-90: [@@deriving cistern] model \"t\", relation \
         \"p\": the Belongs_to refers to its own key (MySQL refuses this \
         foreign key)"
    ; row "rule 8: two belongs_to on one key"
        {|type t = { id : int64; [@key] u_id : int64; u : u Deferred.t; [@belongs_to { key = u_id; references = id }] v : u Deferred.t [@belongs_to { key = u_id; references = id }] }
and u = { id : int64 [@key] } [@@deriving cistern]|}
        "line 1, characters 125-170: [@@deriving cistern] model \"t\", \
         relation \"v\": the key \"u_id\" is also the key of the Belongs_to \
         \"u\""
    ; row "rule 9: a has_many with no belongs_to back"
        {|type t = { id : int64; [@key] us : u list Deferred.t [@has_many] }
and u = { id : int64 [@key] } [@@deriving cistern]|}
        "line 1, characters 30-32: [@@deriving cistern] model \"t\", relation \
         \"us\": the model \"u\" has no Belongs_to to \"t\""
    ; row "rule 9: a has_many with two belongs_to back"
        {|type t = { id : int64; [@key] us : u list Deferred.t [@has_many] }
and u = { id : int64; [@key] a : int64; b : int64; x : t Deferred.t; [@belongs_to { key = a; references = id }] y : t Deferred.t [@belongs_to { key = b; references = id }] } [@@deriving cistern]|}
        "line 1, characters 30-32: [@@deriving cistern] model \"t\", relation \
         \"us\": the model \"u\" has 2 Belongs_to to \"t\" (version 1 has no \
         via)"
    ; row "rule 11: two foreign key names at the second type"
        {|type p = { id : int64 [@key] }
and a = { id : int64; [@key] a_b : int64; r : p Deferred.t [@belongs_to { key = a_b; references = id }] } [@@table "t"]
and b = { id : int64; [@key] b : int64; r : p Deferred.t [@belongs_to { key = b; references = id }] } [@@table "t_a"] [@@deriving cistern]|}
        "line 3, characters 4-5: [@@deriving cistern] the foreign key names \
         \"t_a_b_fk\", \"t_a_b_fk\" of the columns \"t.a_b\", \"t_a.b\" are \
         equal with no regard to ASCII case (a constraint name is unique in \
         the database)"
    ; row
        "rule 11: one type two times, at the attribute of the second belongs_to"
        {|type t = { id : int64; [@key] cccccccccccccccccccccccccccccccccccccccccccccccccccccccccmwjz : int64; [@unique] ccccccccccccccccccccccccccccccccccccccccccccccccccccccccctdimd : int64; x : u Deferred.t; [@belongs_to { key = cccccccccccccccccccccccccccccccccccccccccccccccccccccccccmwjz; references = id }] y : u Deferred.t [@belongs_to { key = ccccccccccccccccccccccccccccccccccccccccccccccccccccccccctdimd; references = id }] }
and u = { id : int64 [@key] } [@@deriving cistern]|}
        "line 1, characters 321-424: [@@deriving cistern] the foreign key \
         names \
         \"t_cccccccccccccccccccccccccccccccccccccccccccccccccc_fk_4c89b599\", \
         \"t_cccccccccccccccccccccccccccccccccccccccccccccccccc_fk_4c89b599\" \
         of the columns \
         \"t.cccccccccccccccccccccccccccccccccccccccccccccccccccccccccmwjz\", \
         \"t.ccccccccccccccccccccccccccccccccccccccccccccccccccccccccctdimd\" \
         are equal with no regard to ASCII case (a constraint name is unique \
         in the database)"
    ; row "an accepted group: no error"
        {|type t = { id : int64; [@key] us : u list Deferred.t [@has_many] }
and u = { id : int64; [@key] t_id : int64; t : t Deferred.t [@belongs_to { key = t_id; references = id }] } [@@deriving cistern]|}
        "no error"
    ; test "accepted: the group of DESIGN.md" (fun () ->
          let src =
            {|type user = {
  id : int64; [@key] [@auto]
  name : string;
  todos : todo list Deferred.t; [@has_many]
}
and todo = {
  id : int64; [@key] [@auto]
  user_id : int64;
  user : user Deferred.t; [@belongs_to { key = user_id; references = id }]
  title : string; [@index]
}
[@@deriving cistern]|}
          in
          equal string
            "type user (line 1, characters 5-9)\n\
            \  0 id (line 2, characters 2-4): int64 key auto\n\
            \  1 name (line 3, characters 2-6): string\n\
            \  relation todos (line 4, characters 2-7) [line 4, characters \
             32-43]: has_many todo\n\
             type todo (line 6, characters 4-8)\n\
            \  2 id (line 7, characters 2-4): int64 key auto\n\
            \  3 user_id (line 8, characters 2-9): int64\n\
            \  4 title (line 10, characters 2-7): string index\n\
            \  relation user (line 9, characters 2-6) [line 9, characters \
             26-74]: belongs_to user key user_id (line 9, characters 47-54) \
             references id (line 9, characters 69-71)"
            (Src.desc src) ;
          equal string
            "user table user, 1 relations\n\
            \  id column id: int64 key auto\n\
            \  name column name: string\n\
            \  todos: has_many todo\n\
             todo table todo, 1 relations\n\
            \  id column id: int64 key auto\n\
            \  user_id column user_id: int64\n\
            \  title column title: string index\n\
            \  user: belongs_to user key user_id references id"
            (Src.schema src) )
    ; test "accepted: a tree with a nullable key" (fun () ->
          let src =
            {|type node = {
  id : int64; [@key] [@auto]
  parent_id : int64 option;
  parent : node option Deferred.t; [@belongs_to { references = id; key = parent_id }]
  children : node list Deferred.t [@has_many]
}
[@@deriving cistern]|}
          in
          equal string
            "type node (line 1, characters 5-9)\n\
            \  0 id (line 2, characters 2-4): int64 key auto\n\
            \  1 parent_id (line 3, characters 2-11): int64 option\n\
            \  relation parent (line 4, characters 2-8) [line 4, characters \
             35-85]: belongs_to node option key parent_id (line 4, characters \
             73-82) references id (line 4, characters 63-65)\n\
            \  relation children (line 5, characters 2-10) [line 5, characters \
             34-45]: has_many node"
            (Src.desc src) ;
          equal string
            "node table node, 2 relations\n\
            \  id column id: int64 key auto\n\
            \  parent_id column parent_id: int64 option\n\
            \  parent: belongs_to node key parent_id references id\n\
            \  children: has_many node"
            (Src.schema src) )
    ; test "accepted: Cistern.Deferred.t and the long forms" (fun () ->
          let src =
            {|type p = {
  id : int; [@key]
  cs : c list Cistern.Deferred.t [@cistern.has_many]
}
and c = {
  id : int; [@key]
  p_id : int; [@column "pid"]
  p : p Cistern.Deferred.t [@cistern.belongs_to { key = p_id; references = id }]
}
[@@deriving cistern]|}
          in
          equal string
            "type p (line 1, characters 5-6)\n\
            \  0 id (line 2, characters 2-4): int key\n\
            \  relation cs (line 3, characters 2-4) [line 3, characters \
             33-52]: has_many c\n\
             type c (line 5, characters 4-5)\n\
            \  1 id (line 6, characters 2-4): int key\n\
            \  2 p_id (line 7, characters 2-6): int column pid (line 7, \
             characters 14-29)\n\
            \  relation p (line 8, characters 2-3) [line 8, characters 27-80]: \
             belongs_to p key p_id (line 8, characters 56-60) references id \
             (line 8, characters 75-77)"
            (Src.desc src) ;
          equal string
            "p table p, 1 relations\n\
            \  id column id: int key\n\
            \  cs: has_many c\n\
             c table c, 1 relations\n\
            \  id column id: int key\n\
            \  p_id column pid: int\n\
            \  p: belongs_to p key p_id references id"
            (Src.schema src) )
    ; test "a [@unique] or [@index] key of a belongs_to: no second filter_by"
        (fun () ->
          let text src =
            let loc, rf, tds = Src.group src in
            Ppxlib.Pprintast.string_of_structure
              (Ppx_cistern.Private.expand ~loc (rf, tds))
          in
          let count sub s =
            let n = String.length sub in
            let rec go i acc =
              if i + n > String.length s then acc
              else if String.sub s i n = sub then go (i + 1) (acc + 1)
              else go (i + 1) acc
            in
            go 0 0
          in
          let group attr =
            Printf.sprintf
              {|type t = { id : int64; [@key] us : u list Deferred.t [@has_many] }
and u = { id : int64; [@key] t_id : int64; %s t : t Deferred.t [@belongs_to { key = t_id; references = id }] } [@@deriving cistern]|}
              attr
          in
          (* The key acts as [@index]: one filter_by_t_id. *)
          equal int 1 (count "let filter_by_t_id" (text (group ""))) ;
          equal int 1 (count "let filter_by_t_id" (text (group "[@index]"))) ;
          (* A [@unique] key has get_by_t_id, and no filter_by_t_id. *)
          equal int 0 (count "let filter_by_t_id" (text (group "[@unique]"))) ;
          equal int 1 (count "let get_by_t_id" (text (group "[@unique]"))) ) ]
