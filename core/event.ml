(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

type outcome =
  | Done of {rows: int}
  | Failed of Error.t
  | Cancelled of {sent: bool}

type kill_outcome = [`Killed | `Gone]

type kill_dropped =
  [ `Queue_full
  | `Duplicate
  | `Other_server
  | `Failed
  | `Not_owner
  | `Off
  | `Drain_timeout
  | `Closed ]

type kill_off =
  [ `Identity_failed
  | `Proxy
  | `Privileged
  | `Role
  | `Grantee_unknown
  | `Not_owner ]

type t =
  | Query of {sql: string; duration: Weft.Span.t; outcome: outcome}
  | Connect of {address: string; duration: Weft.Span.t; error: Error.t option}
  | Kill of {connection_id: int64; duration: Weft.Span.t; outcome: kill_outcome}
  | Kill_dropped of {connection_id: int64; reason: kill_dropped}
  | Kill_off of {reason: kill_off}

let kill_outcome_text = function `Killed -> "killed" | `Gone -> "gone"

let kill_dropped_text = function
  | `Queue_full ->
      "queue_full"
  | `Duplicate ->
      "duplicate"
  | `Other_server ->
      "other_server"
  | `Failed ->
      "failed"
  | `Not_owner ->
      "not_owner"
  | `Off ->
      "off"
  | `Drain_timeout ->
      "drain_timeout"
  | `Closed ->
      "closed"

let kill_off_text = function
  | `Identity_failed ->
      "identity_failed"
  | `Proxy ->
      "proxy"
  | `Privileged ->
      "privileged"
  | `Role ->
      "role"
  | `Grantee_unknown ->
      "grantee_unknown"
  | `Not_owner ->
      "not_owner"

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
  | Kill {connection_id; duration; outcome} ->
      Format.fprintf ppf "kill %a %Ld %s" Weft.Span.pp duration connection_id
        (kill_outcome_text outcome)
  | Kill_dropped {connection_id; reason} ->
      Format.fprintf ppf "kill dropped %Ld %s" connection_id
        (kill_dropped_text reason)
  | Kill_off {reason} ->
      Format.fprintf ppf "kill off %s" (kill_off_text reason)
