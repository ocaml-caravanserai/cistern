(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* A source in a string, through the expander of ppx_cistern as a function. *)

open Ppxlib
module Desc = Ppx_cistern.Private.Desc
module Schema = Cistern_schema.Schema

(* The first group of type declarations in [src]. *)
let group src =
  let lexbuf = Lexing.from_string src in
  Lexing.set_filename lexbuf "src.ml" ;
  let items = Parse.implementation lexbuf in
  match
    List.find_map
      (fun item ->
        match item.pstr_desc with
        | Pstr_type (rf, tds) ->
            Some (item.pstr_loc, rf, tds)
        | _ ->
            None )
      items
  with
  | Some g ->
      g
  | None ->
      invalid_arg "Src.group: no type"

(* The attributes of [src] that the expander does not mark as seen, as the
   driver of ppxlib with -check finds them, in sorted order. *)
let unseen src =
  let lexbuf = Lexing.from_string src in
  let items = Parse.implementation lexbuf in
  Attribute.reset_checks () ;
  Attribute.collect#structure items ;
  List.iter
    (fun item ->
      match item.pstr_desc with
      | Pstr_type (rf, tds) ->
          ignore (Ppx_cistern.Private.expand ~loc:item.pstr_loc (rf, tds))
      | _ ->
          () )
    items ;
  String.concat "\n"
    (List.sort compare
       (List.map Location.Error.message (Attribute.collect_unseen_errors ())) )

let place (loc : Location.t) =
  let s = loc.loc_start and e = loc.loc_end in
  Printf.sprintf "line %d, characters %d-%d" s.pos_lnum (s.pos_cnum - s.pos_bol)
    (e.pos_cnum - s.pos_bol)

(* The error of the expander for [src] with its place, or "no error". *)
let error src =
  let loc, rf, tds = group src in
  match Ppx_cistern.Private.expand ~loc (rf, tds) with
  | [] ->
      "no error"
  | _ :: _ ->
      "code"
  | exception Location.Error e ->
      Printf.sprintf "%s: %s"
        (place (Location.Error.get_location e))
        (Location.Error.message e)

let scalar : Schema.scalar -> string = function
  | Int64 ->
      "int64"
  | Int ->
      "int"
  | String ->
      "string"
  | Bool ->
      "bool"
  | Float ->
      "float"
  | Bytes ->
      "bytes"
  | Ptime ->
      "Ptime.t"

let flags l = String.concat "" (List.filter_map Fun.id l)

let flag b s = if b then Some (" " ^ s) else None

(* The description of the group in [src], one line for each type and field. *)
let desc src =
  let loc, _, tds = group src in
  let field (f : Desc.field) =
    Printf.sprintf "  %d %s (%s): %s%s%s%s%s" f.number f.name (place f.loc)
      (scalar f.scalar)
      (if f.nullable then " option" else "")
      (flags
         [ flag f.key "key"
         ; flag f.auto "auto"
         ; flag f.unique "unique"
         ; flag f.index "index" ] )
      ( match f.column with
      | Some (c, loc) ->
          Printf.sprintf " column %s (%s)" c (place loc)
      | None ->
          "" )
      ( match f.default with
      | Some e ->
          Printf.sprintf " default %s (%s)"
            (Pprintast.string_of_expression e)
            (place e.pexp_loc)
      | None ->
          "" )
  in
  let typ (t : Desc.typ) =
    Printf.sprintf "type %s (%s)%s\n%s" t.name (place t.loc)
      ( match t.table with
      | Some (x, loc) ->
          Printf.sprintf " table %s (%s)" x (place loc)
      | None ->
          "" )
      (String.concat "\n" (List.map field t.fields))
  in
  String.concat "\n" (List.map typ (Desc.group ~loc tds))

(* The models of the group in [src]. *)
let schema src =
  let loc, _, tds = group src in
  let field (f : Schema.field) =
    Printf.sprintf "  %s column %s: %s%s%s" f.name f.column (scalar f.scalar)
      (if f.nullable then " option" else "")
      (flags
         [ flag f.key "key"
         ; flag f.auto "auto"
         ; flag f.unique "unique"
         ; flag f.index "index" ] )
  in
  let model (m : Schema.model) =
    Printf.sprintf "%s table %s, %d relations\n%s" m.name m.table
      (List.length m.relations)
      (String.concat "\n" (List.map field m.fields))
  in
  String.concat "\n" (List.map model (Desc.schema (Desc.group ~loc tds)))
