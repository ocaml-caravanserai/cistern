(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* errors.tsv has each error number of the table of Error.of_server and of
   Error.interrupts, and no other number. make db-errors then compares its names
   with perror of the server packages.

   The probes: with an empty message, a number that is not in the table gives
   the SQLSTATE fallback: Constraint_violation Other_constraint for 23000, and
   Driver for HY000. A number of the table gives its own result with at least
   one of the two (for example 1052 and 1290 give Driver, and not the fallback
   of 23000). *)

open Windtrap
open Cistern_core

let dialects = [(Dialect.Mysql, "mysql"); (Mariadb, "mariadb")]

(* Two probes, so that a row of the table that gives the result of one fallback
   is still found: the fallback of 23000 is Constraint_violation
   Other_constraint, and the fallback of HY000 is Driver. *)
let in_table d code =
  let probe sqlstate = Error.of_server d ~code ~sqlstate ~message:"" in
  ( match probe "23000" with
    | Constraint_violation {kind= Other_constraint; _} ->
        false
    | _ ->
        true )
  || match probe "HY000" with Driver _ -> false | _ -> true

(* The pairs "<dialect> <number>" of the code, in order. *)
let expected =
  List.concat_map
    (fun (d, tag) ->
      List.filter_map
        (fun code ->
          if in_table d code || Error.interrupts d code then
            Some (Printf.sprintf "%s %d" tag code)
          else None )
        (List.init 65536 Fun.id) )
    dialects

(* The rows of the file, with no comment and no empty line. *)
let rows =
  In_channel.with_open_text "errors.tsv" In_channel.input_lines
  |> List.filter (fun l -> l <> "" && l.[0] <> '#')

(* The pair of a row, if the row has a dialect, a number and a name that is not
   empty. *)
let pair l =
  match String.split_on_char '\t' l with
  | [d; n; name] when name <> "" ->
      Some (d ^ " " ^ n)
  | _ ->
      None

(* The pairs of the file, in its order. *)
let file = List.filter_map pair rows

let tests =
  group "errors.tsv"
    [ test "each row has a dialect, a number and a name" (fun () ->
          equal (list string) []
            (List.filter (fun l -> Option.is_none (pair l)) rows) )
    ; test "the file has the numbers of the table, and no other" (fun () ->
          let missing = List.filter (fun p -> not (List.mem p file)) expected in
          let extra = List.filter (fun p -> not (List.mem p expected)) file in
          equal (list string) [] missing ;
          equal (list string) [] extra ;
          equal (list string) expected file ) ]

let () = exit (Windtrap.run "error_codes" [tests])
