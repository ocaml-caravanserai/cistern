(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* Stage 1 of the generated code (the plan of phase 3, 4.3, R1): for each
   hand-written group of test/model, the Pprintast text of the output of the
   expander for the type declaration of the file is the Pprintast text of the
   hand-written stage 1. The hand-written stage 1 came from the generator, thus
   R1 detects changes only. The D2 test below and the run-time rows of the unit,
   sim and integration suites give the meaning. *)

open Windtrap
open Ppxlib

let items m =
  let src =
    In_channel.with_open_bin ("../model/" ^ m ^ ".ml") In_channel.input_all
  in
  let lexbuf = Lexing.from_string src in
  Lexing.set_filename lexbuf (m ^ ".ml") ;
  Parse.implementation lexbuf

let stage1_prefixes =
  [ "Cistern__private__"
  ; "Cistern_fields__"
  ; "Cistern_drafts__"
  ; "Cistern_model__" ]

(* An item of stage 1: a default thunk, or a module with a name of stage 1. *)
let in_stage1 item =
  match item.pstr_desc with
  | Pstr_value _ ->
      true
  | Pstr_module {pmb_name= {txt= Some name; _}; _} ->
      List.exists
        (fun prefix -> String.starts_with ~prefix name)
        stage1_prefixes
  | _ ->
      false

let rec after_type = function
  | {pstr_desc= Pstr_type (rf, tds); pstr_loc} :: rest ->
      ((pstr_loc, rf, tds), rest)
  | _ :: rest ->
      after_type rest
  | [] ->
      invalid_arg "Stage1_tests: no type"

let rec take_while p = function
  | x :: rest when p x ->
      x :: take_while p rest
  | _ ->
      []

(* The two texts of the group of the file [m]: the output of the expander and
   the hand-written stage 1. *)
let texts m =
  let (loc, rf, tds), rest = after_type (items m) in
  ( Pprintast.string_of_structure (Ppx_cistern.Private.expand ~loc (rf, tds))
  , Pprintast.string_of_structure (take_while in_stage1 rest) )

(* D2: the models of the description of the type declaration of the file [m] are
   the run-time schemas of its hand-written model modules. Desc.schema reads the
   attributes of the declaration, and the rows of 3a check it, so this test does
   not depend on the lift of stage 1. *)
let equal_schema m schemas =
  test (m ^ ": the schema of the description is the run-time schema") (fun () ->
      let (loc, _, tds), _ = after_type (items m) in
      let desc = Src.Desc.schema (Src.Desc.group ~loc tds) in
      equal string (Src.models_text schemas) (Src.models_text desc) ;
      is_true (desc = schemas) )

let equal_stage1 m =
  test m (fun () ->
      let generated, written = texts m in
      is_true (String.length written > 0) ;
      equal string written generated )

(* The names and the locations in [str] of the value bindings, of the labels of
   the record types, and of the tuples (the columns of a draft encoder). *)
let places str =
  let binds = ref [] and labels = ref [] and tuples = ref [] in
  let iter =
    object
      inherit Ast_traverse.iter as super

      method! value_binding vb =
        ( match vb.pvb_pat.ppat_desc with
        | Ppat_var {txt; _} ->
            binds := (txt, Src.place vb.pvb_loc) :: !binds
        | _ ->
            () ) ;
        super#value_binding vb

      method! label_declaration ld =
        labels := (ld.pld_name.txt, Src.place ld.pld_loc) :: !labels ;
        super#label_declaration ld

      method! expression e =
        ( match e.pexp_desc with
        | Pexp_tuple [{pexp_desc= Pexp_apply _; _}; {pexp_desc= Pexp_apply _; _}]
          ->
            tuples := Src.place e.pexp_loc :: !tuples
        | _ ->
            () ) ;
        super#expression e
    end
  in
  iter#structure str ;
  (List.rev !binds, List.rev !labels, List.rev !tuples)

let named name l =
  List.filter_map (fun (n, p) -> if n = name then Some p else None) l

(* D8: a type error in the code of a field points at the field. *)
let field_locations () =
  let (loc, rf, tds), _ = after_type (items "person") in
  let fields =
    List.concat_map
      (fun (t : Src.Desc.typ) -> t.fields)
      (Src.Desc.group ~loc tds)
  in
  let binds, labels, tuples =
    places (Ppx_cistern.Private.expand ~loc (rf, tds))
  in
  List.iteri
    (fun i (f : Src.Desc.field) ->
      let at = Src.place f.loc in
      (* The lifted field and the typed field. *)
      equal (list string) [at; at] (named f.name binds) ;
      (* The local of decode. *)
      equal (list string) [at] (named (Printf.sprintf "cistern__%d" i) binds) ;
      (* The label of the draft, if any. *)
      equal (list string) (if f.auto then [] else [at]) (named f.name labels) )
    fields ;
  (* The columns of the draft encoder. *)
  equal (list string)
    (List.filter_map
       (fun (f : Src.Desc.field) ->
         if f.auto then None else Some (Src.place f.loc) )
       fields )
    tuples ;
  (* The default thunk. *)
  equal (list string)
    [Src.place (List.nth fields 5).loc]
    (named "cistern__default__person__5" binds)

let tests =
  group "stage 1"
    [ equal_schema "kinds"
        [Cistern_test_models.Kinds.Cistern_model__kinds.schema]
    ; equal_schema "auto_only"
        [Cistern_test_models.Auto_only.Cistern_model__auto_only.schema]
    ; equal_schema "person"
        [Cistern_test_models.Person.Cistern_model__person.schema]
    ; equal_stage1 "kinds"
    ; equal_stage1 "auto_only"
    ; equal_stage1 "person"
    ; test "the code of each field has the location of its field"
        field_locations
    ; test "a key that is not the first field" (fun () ->
          let loc, rf, tds =
            Src.group {|type late = { a : int; k : string [@key] }|}
          in
          let model =
            List.filter
              (function
                | {pstr_desc= Pstr_module {pmb_name= {txt= Some n; _}; _}; _} ->
                    n = "Cistern_model__late"
                | _ ->
                    false )
              (Ppx_cistern.Private.expand ~loc (rf, tds))
          in
          expect (Pprintast.string_of_structure model)
          @@ __POS_OF__
               {|
            module Cistern_model__late =
              (struct
                 type nonrec t = late
                 type key = string
                 type draft = Cistern_drafts__late.late
                 let schema = Cistern_fields__late.Late.Schema.cistern__model
                 let key = Cistern_fields__late.Late.k
                 let fields =
                   [Cistern__private__late.Field.Any Cistern_fields__late.Late.a;
                   Cistern__private__late.Field.Any Cistern_fields__late.Late.k]
                 let decode cistern__c : t=
                   let cistern__0 =
                     Cistern__private__late.Decoder.next cistern__c
                       (Cistern__private__late.Field.decode Cistern_fields__late.Late.a) in
                   let cistern__1 =
                     Cistern__private__late.Decoder.next cistern__c
                       (Cistern__private__late.Field.decode Cistern_fields__late.Late.k) in
                   { a = cistern__0; k = cistern__1 }
                 let encode_draft = Cistern_drafts__late.encode_late
                 let of_draft (cistern__d : draft) cistern__k : t=
                   { a = (cistern__d.a); k = (cistern__d.k) }
               end :
                Cistern.MODEL with type  t =  late and type  key =  string and type
                   draft =  Cistern_drafts__late.late)[@@ocaml.warning
                                                        "-23-27-30-32-34-37-40-41-42-60-69"]
            |} )
    ; test "the text of auto_only" (fun () ->
          expect (fst (texts "auto_only"))
          @@ __POS_OF__
               {|
            module Cistern__private__auto_only = Cistern.Private[@@ocaml.warning
                                                                  "-23-27-30-32-34-37-40-41-42-60-69"]
            module Cistern_fields__auto_only =
              struct
                module Auto_only =
                  struct
                    module Schema =
                      struct
                        let id =
                          {
                            Cistern__private__auto_only.Schema.name = "id";
                            column = "id";
                            scalar = Cistern__private__auto_only.Schema.Int64;
                            nullable = false;
                            key = true;
                            auto = true;
                            unique = false;
                            index = false
                          }
                        let cistern__model =
                          {
                            Cistern__private__auto_only.Schema.name = "auto_only";
                            table = "auto_only";
                            fields = [id];
                            relations = []
                          }
                      end
                    let id =
                      Cistern__private__auto_only.Field.make ~model:Schema.cistern__model
                        ~field:Schema.id ~encode:Cistern__private__auto_only.Encode.int64
                        ~decode:Cistern__private__auto_only.Decoder.int64
                        ~get:(fun (cistern__r : auto_only) -> cistern__r.id)
                        ~set:(fun (cistern__r : auto_only) cistern__v ->
                                { cistern__r with id = cistern__v })
                  end
              end[@@ocaml.warning "-23-27-30-32-34-37-40-41-42-60-69"]
            module Cistern_drafts__auto_only =
              struct
                type auto_only = Cistern__private__auto_only.Encode.empty
                let encode_auto_only () = []
              end[@@ocaml.warning "-23-27-30-32-34-37-40-41-42-60-69"]
            module Cistern_model__auto_only =
              (struct
                 type nonrec t = auto_only
                 type key = int64
                 type draft = Cistern_drafts__auto_only.auto_only
                 let schema = Cistern_fields__auto_only.Auto_only.Schema.cistern__model
                 let key = Cistern_fields__auto_only.Auto_only.id
                 let fields =
                   [Cistern__private__auto_only.Field.Any
                      Cistern_fields__auto_only.Auto_only.id]
                 let decode cistern__c : t=
                   let cistern__0 =
                     Cistern__private__auto_only.Decoder.next cistern__c
                       (Cistern__private__auto_only.Field.decode
                          Cistern_fields__auto_only.Auto_only.id) in
                   { id = cistern__0 }
                 let encode_draft = Cistern_drafts__auto_only.encode_auto_only
                 let of_draft (cistern__d : draft) cistern__k : t=
                   { id = (Cistern__private__auto_only.Encode.auto cistern__k) }
               end :
                Cistern.MODEL with type  t =  auto_only and type  key =  int64 and type
                   draft =  Cistern_drafts__auto_only.auto_only)[@@ocaml.warning
                                                                  "-23-27-30-32-34-37-40-41-42-60-69"]
            |} )
    ]
