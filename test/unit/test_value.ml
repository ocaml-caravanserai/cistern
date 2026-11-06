(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* Value.equal. *)

open Windtrap
open Cistern_core

let time ?ps () = Fixture.time ?ps (2026, 11, 6) (12, 34, 56)

let decimal = Fixture.decimal

let same name a b = test name (fun () -> is_true (Value.equal (a ()) (b ())))

let differ name a b = test name (fun () -> is_false (Value.equal (a ()) (b ())))

let tests =
  group "Value"
    [ group "equal"
        [ same "Null" (fun () -> Value.Null) (fun () -> Value.Null)
        ; same "Int" (fun () -> Value.Int 1L) (fun () -> Value.Int 1L)
        ; differ "Int, another value"
            (fun () -> Value.Int 1L)
            (fun () -> Value.Int 2L)
        ; same "Float" (fun () -> Value.Float 1.5) (fun () -> Value.Float 1.5)
        ; same "Float, the same NaN"
            (fun () -> Value.Float Float.nan)
            (fun () -> Value.Float Float.nan)
        ; differ "Float, 0.0 and -0.0"
            (fun () -> Value.Float 0.0)
            (fun () -> Value.Float (-0.0))
        ; same "Text" (fun () -> Value.Text "a") (fun () -> Value.Text "a")
        ; differ "Text, another value"
            (fun () -> Value.Text "a")
            (fun () -> Value.Text "b")
        ; same "Bytes" (fun () -> Value.Bytes "a") (fun () -> Value.Bytes "a")
        ; differ "Bytes, another value"
            (fun () -> Value.Bytes "a")
            (fun () -> Value.Bytes "b")
        ; differ "Text and Bytes with the same string"
            (fun () -> Value.Text "a")
            (fun () -> Value.Bytes "a")
        ; same "Datetime"
            (fun () -> Value.Datetime (time ()))
            (fun () -> Value.Datetime (time ()))
        ; differ "Datetime, 1 ps later"
            (fun () -> Value.Datetime (time ()))
            (fun () -> Value.Datetime (time ~ps:1L ()))
        ; same "Decimal" (fun () -> decimal "1.0") (fun () -> decimal "1.0")
        ; differ "Decimal, 1.0 and 1"
            (fun () -> decimal "1.0")
            (fun () -> decimal "1")
        ; differ "Int and Float"
            (fun () -> Value.Int 1L)
            (fun () -> Value.Float 1.0)
        ; differ "Null and Int" (fun () -> Value.Null) (fun () -> Value.Int 0L)
        ] ]
