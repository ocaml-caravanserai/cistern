(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* Each error of ppx_cistern, with its line and its characters (the plan of
   phase 3, 4.2). The checks raise at the first fault. *)

open Windtrap

let row name src expected =
  test name (fun () -> expect (Src.error src) expected)

let tests =
  group "the errors"
    [ row "no [@key]" {|type t = { a : int; b : string } [@@deriving cistern]|}
      @@ __POS_OF__
           {| line 1, characters 5-6: [@@deriving cistern] model "t": no field has [@key] |}
    ; row "two keys"
        {|type t = { a : int; [@key] b : string [@key] } [@@deriving cistern]|}
      @@ __POS_OF__
           {| line 1, characters 27-28: [@@deriving cistern] model "t": 2 fields have [@key] (version 1 has one key) (the fields "a", "b") |}
    ; row "a key that is an option"
        {|type t = { a : int option [@key] } [@@deriving cistern]|}
      @@ __POS_OF__
           {| line 1, characters 11-12: [@@deriving cistern] model "t", field "a": the key is an option |}
    ; row "a key that is a float"
        {|type t = { a : float [@key] } [@@deriving cistern]|}
      @@ __POS_OF__
           {| line 1, characters 11-12: [@@deriving cistern] model "t", field "a": the key is a float |}
    ; row "[@auto] on a field that is not the key"
        {|type t = { id : int64; [@key] n : int64 [@auto] } [@@deriving cistern]|}
      @@ __POS_OF__
           {| line 1, characters 30-31: [@@deriving cistern] model "t", field "n": [@auto] is only on the key |}
    ; row "[@auto] on an int key"
        {|type t = { id : int [@key] [@auto] } [@@deriving cistern]|}
      @@ __POS_OF__
           {| line 1, characters 11-13: [@@deriving cistern] model "t", field "id": [@auto] is only on an int64 field |}
    ; row "[@unique] with [@index]"
        {|type t = { id : int64; [@key] n : string [@unique] [@index] } [@@deriving cistern]|}
      @@ __POS_OF__
           {| line 1, characters 30-31: [@@deriving cistern] model "t", field "n": [@unique] and [@index] together (the unique index is an index) |}
    ; row "[@unique] on the key"
        {|type t = { id : int64 [@key] [@unique] } [@@deriving cistern]|}
      @@ __POS_OF__
           {| line 1, characters 11-13: [@@deriving cistern] model "t", field "id": the key has no [@unique] or [@index] (it has an index) |}
    ; row "an alias"
        {|type t = { id : int64; [@key] n : my_int } [@@deriving cistern]|}
      @@ __POS_OF__
           {| line 1, characters 34-40: [@@deriving cistern] the type of the field "n" of the type "t" is not a type of version 1: int64, int, string, bool, float, bytes, Ptime.t, or an option of one of them (a type alias of the program is not recognized) |}
    ; row "Int64.t" {|type t = { id : Int64.t [@key] } [@@deriving cistern]|}
      @@ __POS_OF__
           {| line 1, characters 16-23: [@@deriving cistern] the type of the field "id" of the type "t" is not a type of version 1: int64, int, string, bool, float, bytes, Ptime.t, or an option of one of them (a type alias of the program is not recognized) |}
    ; row "int list"
        {|type t = { id : int64; [@key] n : int list } [@@deriving cistern]|}
      @@ __POS_OF__
           {| line 1, characters 34-42: [@@deriving cistern] the type of the field "n" of the type "t" is not a type of version 1: int64, int, string, bool, float, bytes, Ptime.t, or an option of one of them (a type alias of the program is not recognized) |}
    ; row "int option option"
        {|type t = { id : int64; [@key] n : int option option } [@@deriving cistern]|}
      @@ __POS_OF__
           {| line 1, characters 34-51: [@@deriving cistern] the type of the field "n" of the type "t" is not a type of version 1: int64, int, string, bool, float, bytes, Ptime.t, or an option of one of them (a type alias of the program is not recognized) |}
    ; row "an option with no argument"
        {|type t = { id : int64; [@key] n : option } [@@deriving cistern]|}
      @@ __POS_OF__
           {| line 1, characters 34-40: [@@deriving cistern] the type of the field "n" of the type "t" is not a type of version 1: int64, int, string, bool, float, bytes, Ptime.t, or an option of one of them (a type alias of the program is not recognized) |}
    ; row "an option with two arguments"
        {|type t = { id : int64; [@key] n : (int, string) option } [@@deriving cistern]|}
      @@ __POS_OF__
           {| line 1, characters 34-54: [@@deriving cistern] the type of the field "n" of the type "t" is not a type of version 1: int64, int, string, bool, float, bytes, Ptime.t, or an option of one of them (a type alias of the program is not recognized) |}
    ; row "a polymorphic type"
        {|type t = { id : int64; [@key] f : 'a. 'a -> 'a } [@@deriving cistern]|}
      @@ __POS_OF__
           {| line 1, characters 34-46: [@@deriving cistern] the field "f" of the type "t" has a polymorphic type |}
    ; row "a mutable field"
        {|type t = { id : int64; [@key] mutable n : int } [@@deriving cistern]|}
      @@ __POS_OF__
           {| line 1, characters 38-39: [@@deriving cistern] the field "n" of the type "t" is mutable: a mutable field is not supported (Field.set gives a copy) |}
    ; row "the field name not"
        {|type t = { id : int64; [@key] not : bool } [@@deriving cistern]|}
      @@ __POS_OF__
           {| line 1, characters 30-33: [@@deriving cistern] the name of the field "not" of the type "t" is a name of the filters of Q (Expr.OPS) |}
    ; row "the field name in_"
        {|type t = { id : int64; [@key] in_ : bool } [@@deriving cistern]|}
      @@ __POS_OF__
           {| line 1, characters 30-33: [@@deriving cistern] the name of the field "in_" of the type "t" is a name of the filters of Q (Expr.OPS) |}
    ; row "the field name starts_with_opt"
        {|type t = { id : int64; [@key] starts_with_opt : bool } [@@deriving cistern]|}
      @@ __POS_OF__
           {| line 1, characters 30-45: [@@deriving cistern] the name of the field "starts_with_opt" of the type "t" is a name of the filters of Q (Expr.OPS) |}
    ; row "the field name contains"
        {|type t = { id : int64; [@key] contains : bool } [@@deriving cistern]|}
      @@ __POS_OF__
           {| line 1, characters 30-38: [@@deriving cistern] the name of the field "contains" of the type "t" is a name of the filters of Q (Expr.OPS) |}
    ; row "the field name contains_opt"
        {|type t = { id : int64; [@key] contains_opt : bool } [@@deriving cistern]|}
      @@ __POS_OF__
           {| line 1, characters 30-42: [@@deriving cistern] the name of the field "contains_opt" of the type "t" is a name of the filters of Q (Expr.OPS) |}
    ; row "the field name starts_with"
        {|type t = { id : int64; [@key] starts_with : bool } [@@deriving cistern]|}
      @@ __POS_OF__
           {| line 1, characters 30-41: [@@deriving cistern] the name of the field "starts_with" of the type "t" is a name of the filters of Q (Expr.OPS) |}
    ; row "the field name is_null"
        {|type t = { id : int64; [@key] is_null : bool } [@@deriving cistern]|}
      @@ __POS_OF__
           {| line 1, characters 30-37: [@@deriving cistern] the name of the field "is_null" of the type "t" is a name of the filters of Q (Expr.OPS) |}
    ; row "a field name with cistern__"
        {|type t = { id : int64; [@key] cistern__x : bool } [@@deriving cistern]|}
      @@ __POS_OF__
           {| line 1, characters 30-40: [@@deriving cistern] the name of the field "cistern__x" of the type "t" starts with "cistern__", the prefix of the generated names |}
    ; row "a type name with cistern__"
        {|type cistern__t = { id : int64 [@key] } [@@deriving cistern]|}
      @@ __POS_OF__
           {| line 1, characters 5-15: [@@deriving cistern] the type name "cistern__t" starts with "cistern__", the prefix of the generated names |}
    ; row "an uppercase letter and no [@@table]"
        {|type userAccount = { id : int64 [@key] } [@@deriving cistern]|}
      @@ __POS_OF__
           {| line 1, characters 5-16: [@@deriving cistern] the type name "userAccount" has an uppercase letter, which a table name cannot have: give [@@table "useraccount"] |}
    ; row "a non-ASCII type name"
        {|type té = { id : int64 [@key] } [@@deriving cistern]|}
      @@ __POS_OF__
           {| line 1, characters 5-8: [@@deriving cistern] the type name "té" must start with an ASCII lowercase letter and have only ASCII characters |}
    ; row "a type name that starts with _"
        {|type _x = { id : int64 [@key] } [@@deriving cistern]|}
      @@ __POS_OF__
           {| line 1, characters 5-7: [@@deriving cistern] the type name "_x" must start with an ASCII lowercase letter and have only ASCII characters |}
    ; row "the type name cistern"
        {|type cistern = { id : int64 [@key] } [@@deriving cistern]|}
      @@ __POS_OF__
           {| line 1, characters 5-12: [@@deriving cistern] the type name "cistern": its module Cistern would hide the library |}
    ; row "the type name stdlib"
        {|type stdlib = { id : int64 [@key] } [@@deriving cistern]|}
      @@ __POS_OF__
           {| line 1, characters 5-11: [@@deriving cistern] the type name "stdlib": its module Stdlib would hide the stdlib |}
    ; row "the type name ptime"
        {|type ptime = { id : int64 [@key] } [@@deriving cistern]|}
      @@ __POS_OF__
           {| line 1, characters 5-10: [@@deriving cistern] the type name "ptime": its module Ptime would change Ptime.t |}
    ; row "the type name unit"
        {|type unit = { id : int64 [@key] } [@@deriving cistern]|}
      @@ __POS_OF__
           {| line 1, characters 5-9: [@@deriving cistern] the type name "unit" is the name of a type of OCaml: it would change the types of the fields of the program |}
    ; row "the type name string"
        {|type string = { id : int64 [@key] } [@@deriving cistern]|}
      @@ __POS_OF__
           {| line 1, characters 5-11: [@@deriving cistern] the type name "string" is the name of a type of OCaml: it would change the types of the fields of the program |}
    ; row "the type name int64"
        {|type int64 = { id : int [@key] } [@@deriving cistern]|}
      @@ __POS_OF__
           {| line 1, characters 5-10: [@@deriving cistern] the type name "int64" is the name of a type of OCaml: it would change the types of the fields of the program |}
    ; row "the type name int"
        {|type int = { id : int64 [@key] } [@@deriving cistern]|}
      @@ __POS_OF__
           {| line 1, characters 5-8: [@@deriving cistern] the type name "int" is the name of a type of OCaml: it would change the types of the fields of the program |}
    ; row "the type name bool"
        {|type bool = { id : int64 [@key] } [@@deriving cistern]|}
      @@ __POS_OF__
           {| line 1, characters 5-9: [@@deriving cistern] the type name "bool" is the name of a type of OCaml: it would change the types of the fields of the program |}
    ; row "the type name float"
        {|type float = { id : int64 [@key] } [@@deriving cistern]|}
      @@ __POS_OF__
           {| line 1, characters 5-10: [@@deriving cistern] the type name "float" is the name of a type of OCaml: it would change the types of the fields of the program |}
    ; row "the type name bytes"
        {|type bytes = { id : int64 [@key] } [@@deriving cistern]|}
      @@ __POS_OF__
           {| line 1, characters 5-10: [@@deriving cistern] the type name "bytes" is the name of a type of OCaml: it would change the types of the fields of the program |}
    ; row "the type name option"
        {|type option = { id : int64 [@key] } [@@deriving cistern]|}
      @@ __POS_OF__
           {| line 1, characters 5-11: [@@deriving cistern] the type name "option" is the name of a type of OCaml: it would change the types of the fields of the program |}
    ; row "the type name cistern_user"
        {|type cistern_user = { id : int64 [@key] } [@@deriving cistern]|}
      @@ __POS_OF__
           {| line 1, characters 5-17: [@@deriving cistern] the type name "cistern_user" starts with "cistern", the prefix of the generated modules |}
    ; row "the type name a__b"
        {|type a__b = { id : int64 [@key] } [@@deriving cistern]|}
      @@ __POS_OF__
           {| line 1, characters 5-9: [@@deriving cistern] the type name "a__b" has "__", the separator of the generated names |}
    ; row "the type name b__"
        {|type b__ = { id : int64 [@key] } [@@deriving cistern]|}
      @@ __POS_OF__
           {| line 1, characters 5-8: [@@deriving cistern] the type name "b__" has "__", the separator of the generated names |}
    ; row "the error of the second type of a group"
        {|type ok = { id : int64 [@key] }
and stdlib = { id : int64 [@key] } [@@deriving cistern]|}
      @@ __POS_OF__
           {| line 2, characters 4-10: [@@deriving cistern] the type name "stdlib": its module Stdlib would hide the stdlib |}
    ; row "[@@table \"User\"]"
        {|type t = { id : int64 [@key] } [@@table "User"] [@@deriving cistern]|}
      @@ __POS_OF__
           {| line 1, characters 31-47: [@@deriving cistern] model "t": the table "User" has an uppercase ASCII letter (version 1 permits only lowercase table names) |}
    ; row "a column name with a backquote"
        {|type t = { id : int64; [@key] n : int [@column "a`b"] } [@@deriving cistern]|}
      @@ __POS_OF__
           {| line 1, characters 38-53: [@@deriving cistern] model "t", field "n": the name "a`b" has a backquote |}
    ; row "two columns equal after the fold"
        {|type t = {
  id : int64; [@key]
  a : int; [@column "Name"]
  b : int [@column "name"]
} [@@deriving cistern]|}
      @@ __POS_OF__
           {| line 4, characters 10-26: [@@deriving cistern] model "t": the columns "Name", "name" are equal with no regard to ASCII case (the fields "a", "b") |}
    ; row "a table name of 65 characters from [@@table]"
        {|type t = { id : int64 [@key] }
[@@table "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"] [@@deriving cistern]|}
      @@ __POS_OF__
           {| line 2, characters 0-77: [@@deriving cistern] model "t": the name "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" has more than 64 characters |}
    ; row "a table name of 65 characters from the type name"
        {|type aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa = { id : int64 [@key] } [@@deriving cistern]|}
      @@ __POS_OF__
           {| line 1, characters 5-70: [@@deriving cistern] model "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa": the name "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" has more than 64 characters (give a shorter table name with [@@table "..."]) |}
    ; row "a column name of 65 characters from the field name"
        {|type t = { id : int64; [@key] ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc : int } [@@deriving cistern]|}
      @@ __POS_OF__
           {| line 1, characters 30-95: [@@deriving cistern] model "t", field "ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc": the name "ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc" has more than 64 characters (give a shorter column name with [@column "..."]) |}
    ; row "a column name of 65 characters from [@column]"
        {|type t = { id : int64; [@key] c : int [@column "ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc"] } [@@deriving cistern]|}
      @@ __POS_OF__
           {| line 1, characters 38-115: [@@deriving cistern] model "t", field "c": the name "ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc" has more than 64 characters |}
    ; row "a long type name with a wrong [@@table]"
        {|type aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa = { id : int64 [@key] }
[@@table "User"] [@@deriving cistern]|}
      @@ __POS_OF__
           {| line 2, characters 0-16: [@@deriving cistern] model "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa": the table "User" has an uppercase ASCII letter (version 1 permits only lowercase table names) |}
    ; row "a long field name with a wrong [@column]"
        {|type t = { id : int64; [@key] ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc : int [@column "a`b"] } [@@deriving cistern]|}
      @@ __POS_OF__
           {| line 1, characters 102-117: [@@deriving cistern] model "t", field "ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc": the name "a`b" has a backquote |}
    ; row "a key with a [@column] that is an option"
        {|type t = { id : int64 option [@key] [@column "k"]; v : int } [@@deriving cistern]|}
      @@ __POS_OF__
           {| line 1, characters 11-13: [@@deriving cistern] model "t", field "id": the key is an option |}
    ; row "two columns equal after the fold, with no [@column]"
        {|type t = {
  id : int64; [@key]
  name : int;
  nAme : int;
} [@@deriving cistern]|}
      @@ __POS_OF__
           {| line 4, characters 2-6: [@@deriving cistern] model "t": the columns "name", "nAme" are equal with no regard to ASCII case (the fields "name", "nAme") |}
    ; row "two columns equal after the fold, the second from the field name"
        {|type t = {
  id : int64; [@key]
  a : int; [@column "b"]
  b : int;
} [@@deriving cistern]|}
      @@ __POS_OF__
           {| line 4, characters 2-3: [@@deriving cistern] model "t": the columns "b", "b" are equal with no regard to ASCII case (the fields "a", "b") |}
    ; row "three columns equal after the fold"
        {|type t = {
  id : int64; [@key]
  name : int;
  nAme : int;
  naMe : int;
} [@@deriving cistern]|}
      @@ __POS_OF__
           {| line 4, characters 2-6: [@@deriving cistern] model "t": the columns "name", "nAme", "naMe" are equal with no regard to ASCII case (the fields "name", "nAme", "naMe") |}
    ; row "[@@table \"x\"] and [@@table \"X\"]"
        {|type a = { id : int64 [@key] } [@@table "x"]
and b = { id : int64 [@key] } [@@table "X"] [@@deriving cistern]|}
      @@ __POS_OF__
           {| line 2, characters 30-43: [@@deriving cistern] the tables "x", "X" of the models "a", "b" are equal with no regard to ASCII case |}
    ; row "two types with one name, the second with [@@table]"
        {|type a = { id : int64 [@key] }
and a = { id : int64 [@key] } [@@table "z"] [@@deriving cistern]|}
      @@ __POS_OF__
           {| line 2, characters 4-5: [@@deriving cistern] 2 models have the name "a" |}
    ; row "two types with one name"
        {|type a = { id : int64 [@key] }
and b = { id : int64 [@key] }
and a = { id : int64 [@key] } [@@deriving cistern]|}
      @@ __POS_OF__
           {| line 3, characters 4-5: [@@deriving cistern] 2 models have the name "a" |}
    ; row "two equal tables from [@@table]"
        {|type a = { id : int64 [@key] }
and b = { id : int64 [@key] } [@@table "a"] [@@deriving cistern]|}
      @@ __POS_OF__
           {| line 2, characters 30-43: [@@deriving cistern] the tables "a", "a" of the models "a", "b" are equal with no regard to ASCII case |}
    ; row "two equal tables, the second from the type name"
        {|type b = { id : int64 [@key] } [@@table "a"]
and a = { id : int64 [@key] } [@@deriving cistern]|}
      @@ __POS_OF__
           {| line 2, characters 4-5: [@@deriving cistern] the tables "a", "a" of the models "b", "a" are equal with no regard to ASCII case |}
    ; row "two equal tables with another type between them"
        {|type a = { id : int64 [@key] } [@@table "c"]
and b = { id : int64 [@key] }
and c = { id : int64 [@key] } [@@deriving cistern]|}
      @@ __POS_OF__
           {| line 3, characters 4-5: [@@deriving cistern] the tables "c", "c" of the models "a", "c" are equal with no regard to ASCII case |}
    ; row "a variant type in the group"
        {|type t = { id : int64 [@key] }
and v = A | B [@@deriving cistern]|}
      @@ __POS_OF__
           {| line 2, characters 0-34: [@@deriving cistern] the type "v" is not a record type |}
    ; row "an abstract type in the group"
        {|type t = { id : int64 [@key] }
and v [@@deriving cistern]|}
      @@ __POS_OF__
           {| line 2, characters 0-26: [@@deriving cistern] the type "v" is not a record type |}
    ; row "a type with a parameter"
        {|type 'a t = { id : int64 [@key] } [@@deriving cistern]|}
      @@ __POS_OF__
           {| line 1, characters 0-54: [@@deriving cistern] the type "t" has parameters |}
    ; row "a private type"
        {|type t = private { id : int64 [@key] } [@@deriving cistern]|}
      @@ __POS_OF__
           {| line 1, characters 0-59: [@@deriving cistern] the type "t" is private |}
    ; row "a type with a manifest"
        {|type t = u = { id : int64 [@key] } [@@deriving cistern]|}
      @@ __POS_OF__
           {| line 1, characters 0-55: [@@deriving cistern] the type "t" is equal to another type |}
    ; row "a wrong payload of [@column]"
        {|type t = { id : int64; [@key] n : int [@column 3] } [@@deriving cistern]|}
      @@ __POS_OF__
           {| line 1, characters 38-49: [@@deriving cistern] [@column] on the field "n" of the type "t" takes one string |}
    ; row "a wrong payload of [@@table]"
        {|type t = { id : int64 [@key] } [@@table x] [@@deriving cistern]|}
      @@ __POS_OF__
           {| line 1, characters 31-42: [@@deriving cistern] [@@table] on the type "t" takes one string |}
    ; row "[@@table] with two strings"
        {|type t = { id : int64 [@key] } [@@table "a" "b"] [@@deriving cistern]|}
      @@ __POS_OF__
           {| line 1, characters 31-48: [@@deriving cistern] [@@table] on the type "t" takes one string |}
    ; row "[@key \"x\"]"
        {|type t = { id : int64 [@key "x"] } [@@deriving cistern]|}
      @@ __POS_OF__
           {| line 1, characters 22-32: [@@deriving cistern] [@key] on the field "id" of the type "t" takes no payload |}
    ; row "[@auto 1]"
        {|type t = { id : int64 [@key] [@auto 1] } [@@deriving cistern]|}
      @@ __POS_OF__
           {| line 1, characters 29-38: [@@deriving cistern] [@auto] on the field "id" of the type "t" takes no payload |}
    ; row "[@unique x]"
        {|type t = { id : int64; [@key] n : int [@unique x] } [@@deriving cistern]|}
      @@ __POS_OF__
           {| line 1, characters 38-49: [@@deriving cistern] [@unique] on the field "n" of the type "t" takes no payload |}
    ; row "[@index ()]"
        {|type t = { id : int64; [@key] n : int [@index ()] } [@@deriving cistern]|}
      @@ __POS_OF__
           {| line 1, characters 38-49: [@@deriving cistern] [@index] on the field "n" of the type "t" takes no payload |}
    ; row "[@key: int]"
        {|type t = { id : int64 [@key: int] } [@@deriving cistern]|}
      @@ __POS_OF__
           {| line 1, characters 22-33: [@@deriving cistern] [@key] on the field "id" of the type "t" takes no payload |}
    ; row "an empty [@default]"
        {|type t = { id : int64; [@key] n : int [@default] } [@@deriving cistern]|}
      @@ __POS_OF__
           {| line 1, characters 38-48: [@@deriving cistern] [@default] on the field "n" of the type "t" has no expression |}
    ; row "a [@default] with two items"
        {|type t = { id : int64; [@key] n : int [@default 1;; 2] } [@@deriving cistern]|}
      @@ __POS_OF__
           {| line 1, characters 38-54: [@@deriving cistern] [@default] on the field "n" of the type "t" takes one expression |}
    ; row "a [@default] with a type"
        {|type t = { id : int64; [@key] n : int [@default: int] } [@@deriving cistern]|}
      @@ __POS_OF__
           {| line 1, characters 38-53: [@@deriving cistern] [@default] on the field "n" of the type "t" takes one expression |}
    ; row "[@default] on an [@auto] key"
        {|type t = { id : int64 [@key] [@auto] [@default 0L] } [@@deriving cistern]|}
      @@ __POS_OF__
           {| line 1, characters 37-50: [@@deriving cistern] the field "id" of the type "t" has [@auto]: the server gives its value, so it has no default |}
    ; row "[@default] on a key with no [@auto]"
        {|type t = { id : string [@key] [@default "x"] } [@@deriving cistern]|}
      @@ __POS_OF__
           {| line 1, characters 30-44: [@@deriving cistern] the field "id" of the type "t" is the key: each row needs its own key, so it has no default |}
    ; row "[@key] [@key]"
        {|type t = { id : int64 [@key] [@key] } [@@deriving cistern]|}
      @@ __POS_OF__
           {| line 1, characters 31-34: [@@deriving cistern] the attribute [@key] comes two times on the field "id" of the type "t" |}
    ; row "[@column \"a\"] [@column \"b\"]"
        {|type t = { id : int64; [@key] n : int [@column "a"] [@column "b"] } [@@deriving cistern]|}
      @@ __POS_OF__
           {| line 1, characters 54-60: [@@deriving cistern] the attribute [@column] comes two times on the field "n" of the type "t" |}
    ; row "[@key] [@cistern.key]"
        {|type t = { id : int64 [@key] [@cistern.key] } [@@deriving cistern]|}
      @@ __POS_OF__
           {| line 1, characters 31-42: [@@deriving cistern] the attribute [@key] comes two times on the field "id" of the type "t" |}
    ; row "[@@table] two times"
        {|type t = { id : int64 [@key] } [@@table "a"] [@@cistern.table "b"] [@@deriving cistern]|}
      @@ __POS_OF__
           {| line 1, characters 48-61: [@@deriving cistern] the attribute [@@table] comes two times on the type "t" |}
    ; row "(string [@unique])"
        {|type t = { id : int64; [@key] n : (string [@unique]) } [@@deriving cistern]|}
      @@ __POS_OF__
           {| line 1, characters 44-50: [@@deriving cistern] the attribute [@unique] is in the type of the field "n" of the type "t": put it after the type |}
    ; row "(string [@unique]) option"
        {|type t = { id : int64; [@key] n : (string [@unique]) option } [@@deriving cistern]|}
      @@ __POS_OF__
           {| line 1, characters 44-50: [@@deriving cistern] the attribute [@unique] is in the type of the field "n" of the type "t": put it after the type |}
    ; row "a field attribute on a type"
        {|type t = { id : int64 [@key] } [@@unique] [@@deriving cistern]|}
      @@ __POS_OF__
           {| line 1, characters 34-40: [@@deriving cistern] the attribute [@unique] is for a field, not for the type "t" |}
    ; row "[@@table] on a field"
        {|type t = { id : int64 [@key] [@table "x"] } [@@deriving cistern]|}
      @@ __POS_OF__
           {| line 1, characters 31-36: [@@deriving cistern] the attribute [@@table] is for a type, not for the field "id" of the type "t" |}
    ; row "[@kye]" {|type t = { id : int64 [@kye] } [@@deriving cistern]|}
      @@ __POS_OF__
           {| line 1, characters 24-27: [@@deriving cistern] unknown attribute [@kye]: did you mean [@key]? |}
    ; row "[@uniqe]"
        {|type t = { id : int64; [@key] n : int [@uniqe] } [@@deriving cistern]|}
      @@ __POS_OF__
           {| line 1, characters 40-45: [@@deriving cistern] unknown attribute [@uniqe]: did you mean [@unique]? |}
    ; row "[@colum \"x\"]"
        {|type t = { id : int64; [@key] n : int [@colum "x"] } [@@deriving cistern]|}
      @@ __POS_OF__
           {| line 1, characters 40-45: [@@deriving cistern] unknown attribute [@colum]: did you mean [@column]? |}
    ; row "[@defualt v]"
        {|type t = { id : int64; [@key] n : int [@defualt 1] } [@@deriving cistern]|}
      @@ __POS_OF__
           {| line 1, characters 40-47: [@@deriving cistern] unknown attribute [@defualt]: did you mean [@default]? |}
    ; row "[@indx]"
        {|type t = { id : int64; [@key] n : int [@indx] } [@@deriving cistern]|}
      @@ __POS_OF__
           {| line 1, characters 40-44: [@@deriving cistern] unknown attribute [@indx]: did you mean [@index]? |}
    ; row "[@@tabel]"
        {|type t = { id : int64 [@key] } [@@tabel "x"] [@@deriving cistern]|}
      @@ __POS_OF__
           {| line 1, characters 34-39: [@@deriving cistern] unknown attribute [@@tabel]: did you mean [@@table]? |}
    ; row "[@has_mny]"
        {|type t = { id : int64; [@key] n : int [@has_mny] } [@@deriving cistern]|}
      @@ __POS_OF__
           {| line 1, characters 40-47: [@@deriving cistern] unknown attribute [@has_mny]: did you mean [@has_many]? |}
    ; row "[@kye] in the type of a field"
        {|type t = { id : int64; [@key] n : (int [@kye]) } [@@deriving cistern]|}
      @@ __POS_OF__
           {| line 1, characters 41-44: [@@deriving cistern] unknown attribute [@kye]: did you mean [@key]? |}
    ; row "[@colu]"
        {|type t = { id : int64; [@key] n : int [@colu "x"] } [@@deriving cistern]|}
      @@ __POS_OF__
           {| line 1, characters 40-44: [@@deriving cistern] unknown attribute [@colu]: did you mean [@column]? |}
    ; row "[@@tab]"
        {|type t = { id : int64 [@key] } [@@tab "x"] [@@deriving cistern]|}
      @@ __POS_OF__
           {| line 1, characters 34-37: [@@deriving cistern] unknown attribute [@@tab]: did you mean [@@table]? |}
    ; row "[@atuo]"
        {|type t = { id : int64 [@key] [@atuo] } [@@deriving cistern]|}
      @@ __POS_OF__
           {| line 1, characters 31-35: [@@deriving cistern] unknown attribute [@atuo]: did you mean [@auto]? |}
    ; row "[@keys]" {|type t = { id : int64 [@keys] } [@@deriving cistern]|}
      @@ __POS_OF__
           {| line 1, characters 24-28: [@@deriving cistern] unknown attribute [@keys]: did you mean [@key]? |}
    ; row "[@ky]" {|type t = { id : int64 [@ky] } [@@deriving cistern]|}
      @@ __POS_OF__
           {| line 1, characters 24-26: [@@deriving cistern] unknown attribute [@ky]: did you mean [@key]? |}
    ; row "[@col \"x\"]"
        {|type t = { id : int64; [@key] n : int [@col "x"] } [@@deriving cistern]|}
      @@ __POS_OF__
           {| line 1, characters 40-43: [@@deriving cistern] unknown attribute [@col]: did you mean [@column]? |}
    ; row "[@unique_key]"
        {|type t = { id : int64; [@key] n : int [@unique_key] } [@@deriving cistern]|}
      @@ __POS_OF__
           {| line 1, characters 40-50: [@@deriving cistern] unknown attribute [@unique_key]: did you mean [@unique]? |}
    ; row "[@unique_index]"
        {|type t = { id : int64; [@key] n : int [@unique_index] } [@@deriving cistern]|}
      @@ __POS_OF__
           {| line 1, characters 40-52: [@@deriving cistern] unknown attribute [@unique_index]: did you mean [@unique]? |}
    ; row "[@primary_key]"
        {|type t = { id : int64 [@primary_key] } [@@deriving cistern]|}
      @@ __POS_OF__
           {| line 1, characters 24-35: [@@deriving cistern] unknown attribute [@primary_key]: did you mean [@key]? |}
    ; row "[@primary]"
        {|type t = { id : int64 [@primary] } [@@deriving cistern]|}
      @@ __POS_OF__
           {| line 1, characters 24-31: [@@deriving cistern] unknown attribute [@primary]: did you mean [@key]? |}
    ; row "[@pk]" {|type t = { id : int64 [@pk] } [@@deriving cistern]|}
      @@ __POS_OF__
           {| line 1, characters 24-26: [@@deriving cistern] unknown attribute [@pk]: did you mean [@key]? |}
    ; row "[@auto_increment]"
        {|type t = { id : int64 [@key] [@auto_increment] } [@@deriving cistern]|}
      @@ __POS_OF__
           {| line 1, characters 31-45: [@@deriving cistern] unknown attribute [@auto_increment]: did you mean [@auto]? |}
    ; row "[@autoincrement]"
        {|type t = { id : int64 [@key] [@autoincrement] } [@@deriving cistern]|}
      @@ __POS_OF__
           {| line 1, characters 31-44: [@@deriving cistern] unknown attribute [@autoincrement]: did you mean [@auto]? |}
    ; row "an attribute before a field on its line"
        {|type t = {
  name : string;
  [@key] code : int64;
} [@@deriving cistern]|}
      @@ __POS_OF__
           {| line 3, characters 4-7: [@@deriving cistern] the attribute [@key] is after the ";" of the field "name" of the type "t", thus OCaml gives it to "name", not to "code" on the same line: for "code", put it after its type, as in "code : int64 [@key];" |}
    ; row "two attributes before a field on its line"
        {|type t = {
  id : int64; [@key]
  name : string;
  [@unique] [@index] email : string;
} [@@deriving cistern]|}
      @@ __POS_OF__
           {| line 4, characters 4-10: [@@deriving cistern] the attribute [@unique] is after the ";" of the field "name" of the type "t", thus OCaml gives it to "name", not to "email" on the same line: for "email", put it after its type, as in "email : string [@unique];" |}
    ; row "an unknown [@cistern.uniqe]"
        {|type t = { id : int64; [@key] n : int [@cistern.uniqe] } [@@deriving cistern]|}
      @@ __POS_OF__
           {| line 1, characters 40-53: [@@deriving cistern] unknown attribute [@cistern.uniqe] |}
    ; row "an unknown [@@cistern.tabel]"
        {|type t = { id : int64 [@key] } [@@cistern.tabel "x"] [@@deriving cistern]|}
      @@ __POS_OF__
           {| line 1, characters 34-47: [@@deriving cistern] unknown attribute [@@cistern.tabel] |}
    ]
