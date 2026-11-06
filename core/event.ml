(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

type outcome =
  | Done of {rows: int}
  | Failed of Error.t
  | Cancelled of {sent: bool}

type t =
  | Query of {sql: string; duration: Weft.Span.t; outcome: outcome}
  | Connect of {address: string; duration: Weft.Span.t; error: Error.t option}

let pp_outcome ppf = function
  | Done {rows} ->
      Format.fprintf ppf "rows %d" rows
  | Failed e ->
      Format.fprintf ppf "failed %a" Error.pp_summary e
  | Cancelled {sent= true} ->
      Format.pp_print_string ppf "cancelled sent"
  | Cancelled {sent= false} ->
      Format.pp_print_string ppf "cancelled not sent"

let pp ppf = function
  | Query {sql; duration; outcome} ->
      Format.fprintf ppf "query %a %a %S" Weft.Span.pp duration pp_outcome
        outcome sql
  | Connect {address; duration; error= None} ->
      Format.fprintf ppf "connect %a %s ok" Weft.Span.pp duration address
  | Connect {address; duration; error= Some e} ->
      Format.fprintf ppf "connect %a %s failed %a" Weft.Span.pp duration address
        Error.pp_summary e
