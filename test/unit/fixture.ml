(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* Builders and printers that the unit tests share. *)

open Cistern_core

(* The time (y, mo, d) (h, mi, s) in UTC, plus [ps] picoseconds. *)
let time ?(ps = 0L) (y, mo, d) (h, mi, s) =
  let t = Option.get (Ptime.of_date_time ((y, mo, d), ((h, mi, s), 0))) in
  Option.get (Ptime.add_span t (Option.get (Ptime.Span.of_d_ps (0, ps))))

(* A Value.Decimal. Call it only inside a test. *)
let decimal s = Value.Decimal (Option.get (Decimal.of_string s))

(* A printer of values for the reports of the tests. *)
let pp_value ppf (v : Value.t) =
  match v with
  | Null ->
      Format.pp_print_string ppf "Null"
  | Int i ->
      Format.fprintf ppf "Int %Ld" i
  | Float f ->
      Format.fprintf ppf "Float %h" f
  | Text s ->
      Format.fprintf ppf "Text %S" s
  | Bytes s ->
      Format.fprintf ppf "Bytes %S" s
  | Datetime t ->
      Format.fprintf ppf "Datetime %a" (Ptime.pp_rfc3339 ~frac_s:12 ()) t
  | Decimal d ->
      Format.fprintf ppf "Decimal %s" (Decimal.to_string d)

let value = Windtrap.Testable.make ~pp:pp_value ~equal:Value.equal
