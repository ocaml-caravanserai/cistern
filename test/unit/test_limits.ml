(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* The numbers of Cistern_core.Limits against the page doc/limits.mld (the plan
   of phase 5, F5). Each row of the page with a name in brackets in the first
   column has a constant here, and each constant has a row. *)

open Windtrap
module L = Cistern_core.Limits

let span s =
  let ns = Weft.Span.to_ns s in
  let min = 60_000_000_000L and sec = 1_000_000_000L and ms = 1_000_000L in
  if ns >= min && Int64.rem ns min = 0L then
    Printf.sprintf "%Ld min" (Int64.div ns min)
  else if ns >= sec && Int64.rem ns sec = 0L then
    Printf.sprintf "%Ld s" (Int64.div ns sec)
  else Printf.sprintf "%Ld ms" (Int64.div ns ms)

let constants =
  [ ("pool_size", string_of_int L.pool_size)
  ; ("checkout_timeout", span L.checkout_timeout)
  ; ("exec_timeout", span L.exec_timeout)
  ; ("transaction_timeout", span L.transaction_timeout)
  ; ("margin", span L.margin)
  ; ("end_min", span L.end_min)
  ; ("end_max", span L.end_max)
  ; ("commit_guard", span L.commit_guard)
  ; ("max_lifetime", span L.max_lifetime)
  ; ("jitter", Printf.sprintf "%.0f %%" (L.jitter *. 100.))
  ; ("idle_max", span L.idle_max)
  ; ("idle_offset", span L.idle_offset)
  ; ("idle_min", span L.idle_min)
  ; ("connect_timeout", span L.connect_timeout)
  ; ("concurrent_connects", string_of_int L.concurrent_connects)
  ; ("backoff_first", span L.backoff_first)
  ; ("backoff_max", span L.backoff_max)
  ; ("kill_limit", span L.kill_limit)
  ; ("killing_age", span L.killing_age)
  ; ("drain_limit", span L.drain_limit)
  ; ("close_limit", span L.close_limit)
  ; ("in_keys", string_of_int L.in_keys)
  ; ("identifier_length", string_of_int L.identifier_length) ]

(* The rows "| [name] | value | ... |" of the page, in their order. *)
let page_rows () =
  let ic = open_in_bin "../../doc/limits.mld" in
  let text = really_input_string ic (in_channel_length ic) in
  close_in ic ;
  String.split_on_char '\n' text
  |> List.filter_map (fun line ->
      match List.map String.trim (String.split_on_char '|' line) with
      | "" :: name :: value :: _
        when String.length name > 2
             && name.[0] = '['
             && name.[String.length name - 1] = ']' ->
          Some (String.sub name 1 (String.length name - 2), value)
      | _ ->
          None )

let pair = list (pair string string)

let tests =
  group "limits"
    [ test "each constant equals its row of doc/limits.mld" (fun () ->
          let sort = List.sort compare in
          equal pair (sort constants) (sort (page_rows ())) )
    ; test "the span format" (fun () ->
          equal (list string)
            ["30 min"; "90 s"; "50 ms"; "1500 ms"]
            (List.map span Weft.Span.[s 1800; s 90; ms 50; ms 1500]) )
    ; test "names refuse one character more than identifier_length" (fun () ->
          let n k = String.make k 'a' in
          equal bool true
            (Result.is_ok
               (Cistern_names.valid_identifier (n L.identifier_length)) ) ;
          equal bool false
            (Result.is_ok
               (Cistern_names.valid_identifier (n (L.identifier_length + 1))) ) )
    ; test "a long index name has identifier_length characters" (fun () ->
          (* Cistern_names cannot read Limits: this row ties its cut. *)
          let long = String.make 100 'a' in
          equal int L.identifier_length
            (String.length
               (Cistern_names.index_name ~table:long ~column:long
                  Cistern_names.Idx ) ) )
    ; test "the idle limit" (fun () ->
          let module P = Cistern.Private.Pool_state in
          equal pair
            [("100", span L.idle_max); ("20", "15 s"); ("1", span L.idle_min)]
            (List.map
               (fun w -> (string_of_int w, span (P.idle_limit w)))
               [100; 20; 1] ) ) ]
