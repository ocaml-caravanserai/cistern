(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* Pool_state: the rows that the stateful test reaches rarely. *)

open Windtrap
module P = Cistern.Private.Pool_state
module Span = Weft.Span

let ms = Span.ms

(* A span in milliseconds. *)
let ms_text t = Printf.sprintf "%Ldms" (Int64.div (Span.to_ns t) 1_000_000L)

let span_text = function Some t -> ms_text t | None -> "none"

let mysql = Cistern_core.Dialect.Mysql

let failure = Cistern_core.Error.Connect "refused"

(* A pool of [size] with [n] waiters at time 0. *)
let with_waiters ?(max_lifetime = Span.s 10) size n =
  let rec go s k =
    if k = 0 then s
    else
      let s, _, _ = P.checkout s Span.zero k in
      go s (k - 1)
  in
  go (P.create ~size ~max_lifetime) n

(* What the opener knows: its doorbell (each wake rings it, a tick clears it)
   and the timer of its last tick. After each operation, the opener must know
   the timer of the state. *)
type opener =
  {st: (string, int) P.t; bell: bool; armed: Span.t option; trace: string list}

let start size =
  { st= P.create ~size ~max_lifetime:(Span.s 10)
  ; bell= false
  ; armed= None
  ; trace= [] }

let knows o now =
  match P.next_timer o.st now with
  | None ->
      true
  | Some t -> (
      o.bell
      || match o.armed with Some a -> Span.compare a t <= 0 | None -> false )

(* One step: an operation at [now] in ms, then the check. *)
let step o now name f =
  let st, wake = f o.st (ms now) in
  let o = {o with st; bell= o.bell || wake; trace= name :: o.trace} in
  is_true
    ~msg:
      ( "the opener knows the timer after "
      ^ String.concat "; " (List.rev o.trace) )
    (knows o (ms now)) ;
  o

let checkout o now w =
  step o now "checkout" (fun s now ->
      let s, _, wake = P.checkout s now w in
      (s, wake) )

let tick o now =
  let st, t = P.tick o.st (ms now) in
  {st; bell= false; armed= t.next_timer; trace= "tick" :: o.trace}

let failed o now =
  step o now "open_failed" (fun s now -> P.open_failed s now failure)

let tests =
  group "Pool_state"
    [ test "S0: a failed connect arms a timer that the opener knows" (fun () ->
          let o = checkout (start 1) 0 0 in
          let o = tick o 0 in
          ignore (failed o 0) )
    ; test "S1: a checkout in the backoff after a cancel" (fun () ->
          let o = start 2 in
          let st, r, _ = P.checkout o.st Span.zero 1 in
          let id = match r with P.Wait id -> id | _ -> fail "no wait" in
          let o = tick {o with st} 0 in
          let o = failed o 1 in
          (* cancel_wait gives no wake: the waiter leaves, the demand is 0. *)
          let st, _, _ = P.cancel_wait o.st (ms 2) id in
          let o = tick {o with st} 3 in
          ignore (checkout o 50 2) )
    ; test "S2: two connects fail while the opener waits" (fun () ->
          let o = checkout (start 4) 0 1 in
          let o = checkout o 0 2 in
          let o = checkout o 0 3 in
          let o = tick o 0 in
          equal int 2 (P.opening o.st) ;
          let o = failed o 10 in
          ignore (failed o 20) )
    ; cases "a jitter out of the range is cut"
        ~id:(fun (n, _, _) -> n)
        [ ("nan", Float.nan, 10_000)
        ; ("infinity", Float.infinity, 11_500)
        ; ("1", 1., 11_500)
        ; ("neg_infinity", Float.neg_infinity, 8_500)
        ; ("-1", -1., 8_500) ]
        (fun (_, u, life_ms) ->
          let s = with_waiters 1 1 in
          let s, _ = P.tick s Span.zero in
          let _, r, _ =
            P.opened s (Span.s 1) "c0" ~wait_timeout:100 ~dialect:mysql ~u
          in
          match r with
          | P.Opened_give (_, e) ->
              equal string
                (span_text (Some (ms (1000 + life_ms))))
                (span_text e.expires)
          | _ ->
              fail "no give" )
    ; cases "create refuses a bad size or life" ~id:fst
        [ ( "size 0"
          , fun () -> ignore (P.create ~size:0 ~max_lifetime:(Span.s 1)) )
        ; ( "size 1001"
          , fun () -> ignore (P.create ~size:1001 ~max_lifetime:(Span.s 1)) )
        ; ( "a life of 0"
          , fun () -> ignore (P.create ~size:1 ~max_lifetime:Span.zero) )
        ; ( "a negative life"
          , fun () -> ignore (P.create ~size:1 ~max_lifetime:(Span.ns (-1))) )
        ]
        (fun (_, f) ->
          raises_match (function Invalid_argument _ -> true | _ -> false) f )
    ; test "create accepts the bounds" (fun () ->
          equal int 1 (P.size (P.create ~size:1 ~max_lifetime:(Span.ns 1))) ;
          equal int 1000
            (P.size (P.create ~size:1000 ~max_lifetime:(Span.ns 1))) )
    ; test "the backoff: 100 ms, doubled, at most 5 s" (fun () ->
          let s = with_waiters 1 1 in
          let rec fail s k acc =
            if k = 0 then List.rev acc
            else
              let s, t = P.tick s (Span.s 100) in
              ignore t ;
              let s, _ = P.open_failed s Span.zero failure in
              (* Each failure at time 0: the end is the maximum. *)
              fail s (k - 1) (span_text (P.backoff_end s) :: acc)
          in
          equal (list string)
            [ "100ms"
            ; "200ms"
            ; "400ms"
            ; "800ms"
            ; "1600ms"
            ; "3200ms"
            ; "5000ms"
            ; "5000ms" ]
            (List.map (fun x -> x) (fail s 8 [])) )
    ; test "a failure earlier in time keeps the later end of the backoff"
        (fun () ->
          (* Two fibers read the clock in one order and report in the other
             order: the end of the backoff is a maximum. *)
          let s = with_waiters 2 2 in
          let s, _ = P.tick s Span.zero in
          let s, _ = P.open_failed s (Span.s 10) failure in
          equal string "10100ms" (span_text (P.backoff_end s)) ;
          let s, _ = P.open_failed s (ms 9850) failure in
          (* 9850 ms + 200 ms is earlier than 10100 ms. *)
          equal string "10100ms" (span_text (P.backoff_end s)) )
    ; test "no timer when no slot is free" (fun () ->
          (* Size 1, two waiters. A connect fails at 0 (backoff to 100 ms). At
             200 ms a connect starts, and it takes the only slot. *)
          let s = with_waiters 1 2 in
          let s, _ = P.tick s Span.zero in
          let s, _ = P.open_failed s Span.zero failure in
          let s, t = P.tick s (ms 200) in
          equal int 1 t.connects ;
          equal int 0 (P.free s) ;
          equal int 1 (P.demand s) ;
          (* Back in time, inside the backoff: no timer, no slot. *)
          equal string "none" (span_text (P.next_timer s (ms 50))) )
    ; test "no timer while two connects run" (fun () ->
          let s = with_waiters 4 3 in
          let s, t = P.tick s Span.zero in
          equal int 2 t.connects ;
          let s, _ = P.open_failed s Span.zero failure in
          let s, t = P.tick s (ms 200) in
          equal int 1 t.connects ;
          equal int 2 (P.opening s) ;
          (* Back in time, inside the backoff: no timer, because two connects
             run. *)
          equal string "none" (span_text (P.next_timer s (ms 50))) )
    ; test "an entry with no end of life" (fun () ->
          let s = with_waiters ~max_lifetime:(Span.of_ns Int64.max_int) 1 1 in
          let s, _ = P.tick s (Span.s 1) in
          let s, r, _ =
            P.opened s (Span.s 1) "c0" ~wait_timeout:100 ~dialect:mysql ~u:0.15
          in
          let e =
            match r with P.Opened_give (_, e) -> e | _ -> fail "no give"
          in
          is_true (e.expires = None) ;
          let s, _, _ = P.checkin s (Span.s 2) e ~reusable:true ~dirty:false in
          let _, r, _ = P.checkout s (Span.s 3) 0 in
          is_true (match r with P.Conn _ -> true | _ -> false) )
    ; cases "the idle limit"
        ~id:(fun (w, _) -> string_of_int w)
        [ (1, "1000ms")
        ; (6, "1000ms")
        ; (7, "2000ms")
        ; (35, "30000ms")
        ; (100, "30000ms") ]
        (fun (w, expected) ->
          let s = with_waiters 1 1 in
          let s, _ = P.tick s Span.zero in
          let s, r, _ =
            P.opened s Span.zero "c0" ~wait_timeout:w ~dialect:mysql ~u:0.
          in
          ignore s ;
          match r with
          | P.Opened_give (_, e) ->
              equal string expected (ms_text e.idle_limit)
          | _ ->
              fail "no give" )
    ; test "the dialect of the last connect" (fun () ->
          let s = with_waiters 1 0 in
          equal bool true (P.last_dialect s = None) ;
          let s, _, _ = P.checkout s Span.zero 0 in
          let s, _ = P.tick s Span.zero in
          let s, _, _ =
            P.opened s Span.zero "c0" ~wait_timeout:100
              ~dialect:Cistern_core.Dialect.Mariadb ~u:0.
          in
          is_true (P.last_dialect s = Some Cistern_core.Dialect.Mariadb) ) ]
