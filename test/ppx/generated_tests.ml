(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* The generated code (the plan of phase 3, 4.3 and 4.4): for each hand-written
   group of test/model, the Pprintast text of the output of the expander for the
   type declaration of the file is the Pprintast text of the items of the file
   after the declaration. The hand-written code came from the generator, thus
   this equality detects changes only. The D2 test below and the run-time rows
   of the unit, sim and integration suites give the meaning. *)

open Windtrap
open Ppxlib

let items ?(dir = "../model/") m =
  let src = In_channel.with_open_bin (dir ^ m ^ ".ml") In_channel.input_all in
  let lexbuf = Lexing.from_string src in
  Lexing.set_filename lexbuf (m ^ ".ml") ;
  Parse.implementation lexbuf

let rec after_type = function
  | {pstr_desc= Pstr_type (rf, tds); pstr_loc} :: rest ->
      ((pstr_loc, rf, tds), rest)
  | _ :: rest ->
      after_type rest
  | [] ->
      invalid_arg "Generated_tests: no type"

(* The two texts of the group of the file [m]: the output of the expander and
   the hand-written items after the type declaration. *)
let texts m =
  let (loc, rf, tds), rest = after_type (items m) in
  ( Pprintast.string_of_structure (Ppx_cistern.Private.expand ~loc (rf, tds))
  , Pprintast.string_of_structure rest )

(* D2: the models of the description of the type declaration of the file [m] are
   the run-time schemas of its hand-written model modules ([models], the values
   [model] of stage 3). Desc.schema reads the attributes of the declaration, and
   the rows of 3a check it, so this test does not depend on the lift. *)
let equal_schema ?dir m models =
  let file = Option.value dir ~default:"../model/" ^ m ^ ".ml" in
  test (file ^ ": the schema of the description is the run-time schema")
    (fun () ->
      let (loc, _, tds), _ = after_type (items ?dir m) in
      let desc = Src.Desc.schema (Src.Desc.group ~loc tds) in
      let schemas =
        List.map (fun (module M : Cistern.MODEL) -> M.schema) models
      in
      equal string (Src.models_text schemas) (Src.models_text desc) ;
      is_true (desc = schemas) )

(* The derived models of test/model_ppx have the type declarations of the
   hand-written models of test/model (the plan of phase 3, 4.5): with no
   [@@deriving], the two Pprintast texts are equal. *)
let same_declaration m =
  test (m ^ ": test/model_ppx has the type declaration of test/model")
    (fun () ->
      let text dir =
        let (_, rf, tds), _ = after_type (items ~dir m) in
        let tds =
          List.map
            (fun (td : type_declaration) ->
              { td with
                ptype_attributes=
                  List.filter
                    (fun (a : attribute) -> a.attr_name.txt <> "deriving")
                    td.ptype_attributes } )
            tds
        in
        Pprintast.string_of_structure
          [Ast_builder.Default.pstr_type ~loc:Location.none rf tds]
      in
      equal string (text "../model/") (text "../model_ppx/") )

let equal_output m =
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
      (* The lifted field, the typed field and the field of Q. *)
      equal (list string) [at; at; at] (named f.name binds) ;
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

(* The values of stages 2 and 3 of the group of the file [m], each with the
   labels of its parameters: one line for each value. *)
let headers m =
  let (loc, rf, tds), _ = after_type (items m) in
  let lines = ref [] in
  let iter =
    object
      inherit Ast_traverse.iter as super

      method! module_binding mb =
        match mb.pmb_name.txt with
        | Some name
          when String.starts_with ~prefix:"Cistern_gen__" name
               || not (String.starts_with ~prefix:"Cistern" name) ->
            super#module_binding mb
        | _ ->
            ()

      method! value_binding vb =
        let label = function
          | Nolabel ->
              "_"
          | Labelled l ->
              "~" ^ l
          | Optional l ->
              "?" ^ l
        in
        ( match (vb.pvb_pat.ppat_desc, vb.pvb_expr.pexp_desc) with
        | Ppat_var {txt; _}, Pexp_function (params, _, _) ->
            let ps =
              List.filter_map
                (fun p ->
                  match p.pparam_desc with
                  | Pparam_val (l, _, _) ->
                      Some (label l)
                  | Pparam_newtype _ ->
                      None )
                params
            in
            lines := String.concat " " (txt :: ps) :: !lines
        | Ppat_var {txt; _}, _ ->
            lines := txt :: !lines
        | _ ->
            () ) ;
        super#value_binding vb
    end
  in
  iter#structure (Ppx_cistern.Private.expand ~loc (rf, tds)) ;
  String.concat "\n" (List.rev !lines)

let tests =
  group "the generated code"
    [ equal_schema "kinds" [Cistern_test_models.Kinds.Kinds.model]
    ; equal_schema "auto_only" [Cistern_test_models.Auto_only.Auto_only.model]
    ; equal_schema "person" [Cistern_test_models.Person.Person.model]
    ; equal_schema "user_todo"
        [ Cistern_test_models.User_todo.User.model
        ; Cistern_test_models.User_todo.Todo.model ]
    ; equal_schema "tree" [Cistern_test_models.Tree.Node.model]
    ; same_declaration "kinds"
    ; same_declaration "auto_only"
    ; same_declaration "person"
    ; (let open Cistern_test_models_ppx in
       equal_schema ~dir:"../model_ppx/" "kinds" [Kinds.Kinds.model] )
    ; (let open Cistern_test_models_ppx in
       equal_schema ~dir:"../model_ppx/" "auto_only" [Auto_only.Auto_only.model]
      )
    ; (let open Cistern_test_models_ppx in
       equal_schema ~dir:"../model_ppx/" "person" [Person.Person.model] )
    ; (let open Cistern_test_models_ppx.Hostile in
       equal_schema ~dir:"../model_ppx/" "hostile"
         [T.model; S.model; F.model; D.model; M.model; U.model] )
    ; (let open Cistern_test_models_ppx.Two_groups in
       equal_schema ~dir:"../model_ppx/" "two_groups" [Note.model] )
    ; (let open Ppx_driver_check.Hostile in
       equal_schema ~dir:"driver/" "hostile"
         [T.model; S.model; F.model; D.model; M.model; U.model] )
    ; (let open Ppx_driver_check.Groups in
       equal_schema ~dir:"driver/" "groups" [Person.model; Tag.model] )
    ; equal_output "kinds"
    ; equal_output "auto_only"
    ; equal_output "person"
    ; equal_output "user_todo"
    ; test "user_todo: the code of each relation has the location of its field"
        (fun () ->
          let (loc, rf, tds), _ = after_type (items "user_todo") in
          let binds, _, _ =
            places (Ppx_cistern.Private.expand ~loc (rf, tds))
          in
          List.iter
            (fun (t : Src.Desc.typ) ->
              List.iter
                (fun (r : Src.Desc.relation) ->
                  let at = Src.place r.loc in
                  (* The setter, the scope and the value of Rel. *)
                  equal (list string) [at; at; at] (named r.name binds) )
                t.relations )
            (Src.Desc.group ~loc tds) )
    ; test "user_todo: the values of stages 2 and 3 with their labels"
        (fun () ->
          equal string
            "cistern__default__user__5 _\n\
             cistern__default__user__9 _\n\
             id\n\
             name\n\
             email\n\
             nick\n\
             age\n\
             active\n\
             get_by_id _\n\
             find_by_id _\n\
             get_by_email _\n\
             find_by_email _\n\
             get_by_nick _\n\
             find_by_nick _\n\
             all\n\
             where _\n\
             update ?name ?email ?nick ?age ?active _\n\
             delete _\n\
             draft ~name ~email ?nick ?age ?active _\n\
             id\n\
             user_id\n\
             title\n\
             done_\n\
             note\n\
             get_by_id _\n\
             find_by_id _\n\
             filter_by_title _\n\
             filter_by_user_id _\n\
             all\n\
             where _\n\
             update ?user_id ?title ?done_ ?note _\n\
             delete _\n\
             draft ?user_id ~title ?done_ ?note _\n\
             model\n\
             create ~name ~email ?nick ?age ?active ?todos _\n\
             todos _\n\
             todos\n\
             model\n\
             create ?user_id ~title ?done_ ?note _\n\
             user _\n\
             user"
            (headers "user_todo") )
    ; equal_output "tree"
    ; test "the values of stages 2 and 3 of person and kinds" (fun () ->
          expect (headers "person")
          @@ __POS_OF__
               {|
            cistern__default__person__5 _
            id
            email
            nick
            city
            age
            active
            get_by_id _
            find_by_id _
            get_by_email _
            find_by_email _
            get_by_nick _
            find_by_nick _
            filter_by_city _
            all
            where _
            update ?email ?nick ?city ?age ?active _
            delete _
            draft ~email ?nick ~city ?age ?active _
            model
            create ~email ?nick ~city ?age ?active _
            |} ;
          expect (headers "kinds")
          @@ __POS_OF__
               {|
            id
            i
            i_opt
            s
            s_opt
            b
            b_opt
            f
            f_opt
            y
            y_opt
            t
            t_opt
            get_by_id _
            find_by_id _
            all
            where _
            update ?i ?i_opt ?s ?s_opt ?b ?b_opt ?f ?f_opt ?y ?y_opt ?t ?t_opt _
            delete _
            draft ~id ~i ?i_opt ~s ?s_opt ~b ?b_opt ~f ?f_opt ~y ?y_opt ~t ?t_opt _
            model
            create ~id ~i ?i_opt ~s ?s_opt ~b ?b_opt ~f ?f_opt ~y ?y_opt ~t ?t_opt _
            |} )
    ; test "a group of several types: each type has its own values" (fun () ->
          let open Ppx_driver_check.Hostile in
          let sql s =
            Stdlib.List.hd (Cistern.Stmt.to_sql Cistern_core.Dialect.Mysql s)
          in
          let all q = sql (Cistern.Query.to_list q) in
          let from table s =
            let k = String.length table in
            let rec at i =
              i + k <= String.length s
              && (String.sub s i k = table || at (i + 1))
            in
            at 0
          in
          let check table texts =
            Stdlib.List.iter
              (fun s ->
                if not (from ("`" ^ table ^ "`") s) then
                  fail (Printf.sprintf "no table %s in %s" table s) )
              texts
          in
          let s = {draft= 1L; schema= "a"; s_name= "b"; name= None; model= 2} in
          check "s"
            [ all S.all
            ; all (S.where S.Q.(model ==. 2))
            ; sql (S.get_by_draft 1L)
            ; sql (S.delete s) ] ;
          let f = {update= "u"; key= 1; all= None} in
          check "f" [all F.all; sql (F.get_by_update "u"); sql (F.delete f)] ;
          let d = {where= 1L; delete= 2} in
          check "d" [all D.all; sql (D.get_by_where 1L); sql (D.delete d)] ;
          let m = {fields= 1L; decode= "x"} in
          check "m" [all M.all; sql (M.get_by_fields 1L); sql (M.delete m)] ;
          check "u" [all U.all; sql (U.get_by_cistern 1L)] ;
          (* The defaults of the two thunks (D10). *)
          equal int 1 (A_.draft ~id:1L ()).b ;
          equal int 2 (A.draft ~key_a:1L ())._b )
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
            module Cistern_gen__auto_only =
              struct
                module Auto_only =
                  struct
                    type nonrec t = auto_only
                    module Q =
                      struct
                        let id = Cistern_fields__auto_only.Auto_only.id
                        include Cistern__private__auto_only.Expr.Ops
                      end
                    let get_by_id cistern__k =
                      Cistern__private__auto_only.Model.get (module
                        Cistern_model__auto_only) cistern__k
                    let find_by_id cistern__k =
                      Cistern__private__auto_only.Model.find (module
                        Cistern_model__auto_only) cistern__k
                    let all =
                      Cistern__private__auto_only.Model.all (module
                        Cistern_model__auto_only)
                    let where cistern__e =
                      Cistern__private__auto_only.Model.where (module
                        Cistern_model__auto_only) cistern__e
                    let update cistern__r =
                      Cistern__private__auto_only.Model.update (module
                        Cistern_model__auto_only)
                        (Cistern__private__auto_only.Encode.updates []) cistern__r
                    let delete cistern__r =
                      Cistern__private__auto_only.Model.delete (module
                        Cistern_model__auto_only) cistern__r
                    let draft () : Cistern_drafts__auto_only.auto_only= ()
                  end
              end[@@ocaml.warning "-23-27-30-32-34-37-40-41-42-60-69"]
            module Auto_only =
              struct
                include Cistern_gen__auto_only.Auto_only
                module Drafts = Cistern_drafts__auto_only
                type draft = Drafts.auto_only
                let model : (module Cistern.MODEL) = (module Cistern_model__auto_only)
                let create () =
                  Cistern__private__auto_only.Model.create (module
                    Cistern_model__auto_only) (Cistern_gen__auto_only.Auto_only.draft ())
              end[@@ocaml.warning "-23-27-30-32-34-37-40-41-42-60-69"]
            |} )
    ]
