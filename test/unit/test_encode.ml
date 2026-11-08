(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* Encode: the helpers of the generated code (the plan of phase 3, D8). *)

open Windtrap
open Cistern_core

let value name expected actual =
  test name (fun () -> is_true (Value.equal expected (actual ())))

let time ps = Fixture.time ~ps (2026, 11, 8) (1, 2, 3)

let tests =
  group "Encode"
    [ value "int64" (Value.Int (-5L)) (fun () -> Encode.int64 (-5L))
    ; value "int" (Value.Int 7L) (fun () -> Encode.int 7)
    ; value "text" (Value.Text "é\x00") (fun () -> Encode.text "é\x00")
    ; value "bool true" (Value.Int 1L) (fun () -> Encode.bool true)
    ; value "bool false" (Value.Int 0L) (fun () -> Encode.bool false)
    ; value "float" (Value.Float 1.5) (fun () -> Encode.float 1.5)
    ; value "bytes" (Value.Bytes "\x00\xff") (fun () ->
          Encode.bytes (Bytes.of_string "\x00\xff") )
    ; value "ptime" (Value.Datetime (time 0L)) (fun () -> Encode.ptime (time 0L))
    ; value "opt None" Value.Null (fun () -> Encode.opt Encode.int None)
    ; value "opt Some" (Value.Int 3L) (fun () -> Encode.opt Encode.int (Some 3))
    ; test "cut: to 1 µs, not rounded" (fun () ->
          is_true
            (Ptime.equal (time 123_456_000_000L)
               (Encode.cut (time 123_456_999_999L)) ) )
    ; test "cut_opt" (fun () ->
          is_true (Encode.cut_opt None = None) ;
          is_true
            (Option.equal Ptime.equal
               (Some (time 999_999_000_000L))
               (Encode.cut_opt (Some (time 999_999_999_999L))) ) )
    ; test "some" (fun () -> equal (option int) (Some 3) (Encode.some 3))
    ; test "set and updates: the labels of update, in order" (fun () ->
          let module Q = Cistern_test_models.Person.Person.Q in
          let r =
            { Cistern_test_models.Person.id= 1L
            ; email= "e"
            ; nick= None
            ; city= "c"
            ; age= None
            ; active= true }
          in
          equal bool true (Encode.set Q.city None = None) ;
          let us =
            Encode.updates
              [ Encode.set Q.city (Some "d")
              ; Encode.set Q.age None
              ; Encode.set Q.email (Some "f") ]
          in
          equal (list string) ["city"; "email"]
            (List.concat_map (fun u -> List.map fst (Update.assignments u)) us) ;
          equal bool true
            ( List.fold_left (fun r u -> Update.apply u r) r us
            = {r with city= "d"; email= "f"} ) )
    ; test "auto: the key from the server" (fun () ->
          equal int64 4L (Encode.auto (Some 4L)) )
    ; test "auto: no key raises Invalid_argument" (fun () ->
          raises
            (Invalid_argument "Cistern: of_draft of an [@auto] key with no key")
            (fun () -> Encode.auto (None : int64 option) ) ) ]
