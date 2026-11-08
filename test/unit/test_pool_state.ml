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

(* A pool of 1 whose entry is in use by the waiter 0, opened at time 0. *)
let one_in_use () =
  let o = checkout (start 1) 0 0 in
  let o = tick o 0 in
  let st, r, wake =
    P.opened o.st Span.zero "c0" ~wait_timeout:100 ~dialect:mysql ~u:0.
  in
  let e = match r with P.Opened_give (_, e) -> e | _ -> fail "no give" in
  ({o with st; bell= o.bell || wake}, e)

let killing o now e =
  let tok = ref None in
  let o =
    step o now "killing" (fun s now ->
        let s, k, wake = P.killing s now e in
        tok := Some k ;
        (s, wake) )
  in
  (o, Option.get !tok)

let slots_text st =
  String.concat ";"
    (List.map
       (fun (k, a) -> Printf.sprintf "%d@%s" (P.token_id k) (ms_text a))
       (P.killing_slots st) )

let killing_tests =
  [ test "the stall of review C: an expired Killing slot and a later checkout"
      (fun () ->
        (* A pool of 1. At 1 s the entry becomes a Killing slot (age limit 31
           s), with no waiter: no wake and no timer. At 41 s no operation has
           removed the slot. A checkout waits, and it must wake the opener,
           because the slot is free at its time. *)
        let o, e = one_in_use () in
        let o = tick o 0 in
        let o, tok = killing o 1000 e in
        equal int 0 (P.token_id tok) ;
        equal bool false o.bell ;
        equal string "none" (span_text (P.next_timer o.st (ms 1000))) ;
        equal string "0@31000ms" (slots_text o.st) ;
        equal int 0 (P.free o.st) ;
        let st, r, wake = P.checkout o.st (ms 41000) 7 in
        is_true (match r with P.Wait _ -> true | _ -> false) ;
        equal bool true wake ;
        equal string "" (slots_text st) ;
        equal int 1 (P.free st) ;
        let _, t = P.tick st (ms 41000) in
        equal int 1 t.connects )
  ; test "the timer at the age limit of a Killing slot" (fun () ->
        (* The waiter comes first: the timer is the age limit, and a tick at it
           starts the connect. *)
        let o, e = one_in_use () in
        let o = tick o 0 in
        let o, _ = killing o 1000 e in
        let o = checkout o 2000 8 in
        equal bool true o.bell ;
        equal string "31000ms" (span_text (P.next_timer o.st (ms 2000))) ;
        let o = tick o 2000 in
        equal string "31000ms" (span_text o.armed) ;
        let _, t = P.tick o.st (ms 30999) in
        equal int 0 t.connects ;
        let _, t = P.tick o.st (ms 31000) in
        equal int 1 t.connects )
  ; test
      "an age limit before the end of the backoff gives the end of the backoff"
      (fun () ->
        (* Size 2, two waiters, two connects. The first gives its entry to a
           waiter, and the entry becomes a Killing slot at 950 ms (age limit
           30950 ms). The second connect fails at 30900 ms: the backoff ends at
           31000 ms. At the age limit a slot is free, but the backoff stops the
           connect. *)
        let s = with_waiters 2 2 in
        let s, t = P.tick s Span.zero in
        equal int 2 t.connects ;
        let s, r, _ =
          P.opened s Span.zero "c0" ~wait_timeout:100 ~dialect:mysql ~u:0.
        in
        let e =
          match r with P.Opened_give (_, e) -> e | _ -> fail "no give"
        in
        let s, _, _ = P.killing s (ms 950) e in
        let s, wake = P.open_failed s (ms 30900) failure in
        equal bool true wake ;
        equal string "31000ms" (span_text (P.backoff_end s)) ;
        equal string "31000ms" (span_text (P.next_timer s (ms 30900))) ;
        let _, t = P.tick s (ms 30950) in
        equal int 0 t.connects ;
        let _, t = P.tick s (ms 31000) in
        equal int 1 t.connects )
  ; test "no free slot: the end of the backoff after the earliest age limit"
      (fun () ->
        (* A state with no free slot, the end of the backoff at 100 ms, and an
           age limit at 50 ms. No operation assumes that the time grows, thus
           the killing at -29950 ms is permitted. At 50 ms a slot is free, but
           the backoff stops the connect until 100 ms (review C of 1.5a, M1: the
           closed form takes the later of the two). *)
        let s = P.create ~size:2 ~max_lifetime:(Span.s 10) in
        let s, _, _ = P.checkout s Span.zero 1 in
        let s, _ = P.tick s Span.zero in
        let s, r, _ =
          P.opened s Span.zero "a" ~wait_timeout:100 ~dialect:mysql ~u:0.
        in
        let a =
          match r with P.Opened_give (_, e) -> e | _ -> fail "no give"
        in
        let s, _, _ = P.checkout s Span.zero 2 in
        let s, _ = P.tick s Span.zero in
        let s, _ = P.open_failed s Span.zero failure in
        let s, t = P.tick s (ms 100) in
        equal int 1 t.connects ;
        let s, _, _ = P.killing s (ms (-29950)) a in
        let s, _, _ = P.checkout s (ms (-29900)) 3 in
        equal int 0 (P.free s) ;
        equal string "100ms" (span_text (P.next_timer s (ms 10))) ;
        let _, t = P.tick s (ms 50) in
        equal int 0 t.connects ;
        let _, t = P.tick s (ms 100) in
        equal int 1 t.connects )
  ; test "killed: one time, and not after the age limit" (fun () ->
        let o, e = one_in_use () in
        let s, tok, _ = P.killing o.st (ms 1000) e in
        let s2, freed, _ = P.killed s (ms 2000) tok in
        equal bool true freed ;
        equal int 1 (P.free s2) ;
        let _, freed, _ = P.killed s2 (ms 3000) tok in
        equal bool false freed ;
        (* At the age limit the slot is already free. *)
        let s3, freed, _ = P.killed s (ms 31000) tok in
        equal bool false freed ;
        equal string "" (slots_text s3) ;
        (* 1 ns before it, killed frees it. *)
        let _, freed, _ =
          P.killed s (Span.of_ns (Int64.pred (Span.to_ns (ms 31000)))) tok
        in
        equal bool true freed )
  ; test "the tokens are unique, and killed frees only its slot" (fun () ->
        let s = with_waiters 2 2 in
        let s, _ = P.tick s Span.zero in
        let open_one s =
          let s, r, _ =
            P.opened s Span.zero "c" ~wait_timeout:100 ~dialect:mysql ~u:0.
          in
          (s, match r with P.Opened_give (_, e) -> e | _ -> fail "no give")
        in
        let s, a = open_one s in
        let s, b = open_one s in
        let s, ta, _ = P.killing s (ms 1000) a in
        let s, tb, _ = P.killing s (ms 2000) b in
        equal string "0@31000ms;1@32000ms" (slots_text s) ;
        let s, _, _ = P.killed s (ms 3000) tb in
        equal string "0@31000ms" (slots_text s) ;
        ignore ta )
  ; test "close_all keeps the Killing slots" (fun () ->
        let o, e = one_in_use () in
        let s, tok, _ = P.killing o.st (ms 1000) e in
        let s, _, _ = P.close_all s (ms 2000) in
        equal string "0@31000ms" (slots_text s) ;
        let s, freed, wake = P.killed s (ms 3000) tok in
        equal bool true freed ;
        equal bool false wake ;
        equal int 1 (P.free s) )
  ; test "an age limit too large for a span" (fun () ->
        let o, e = one_in_use () in
        let late = Span.of_ns (Int64.sub Int64.max_int 1_000_000_000L) in
        let s, _, _ = P.killing o.st late e in
        equal (list int64) [Int64.max_int]
          (List.map (fun (_, a) -> Span.to_ns a) (P.killing_slots s)) ) ]

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
          let st, _, _, wake = P.cancel_wait o.st (ms 2) id in
          equal bool false wake ;
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
          is_true (P.last_dialect s = Some Cistern_core.Dialect.Mariadb) )
    ; group "the Killing slots" killing_tests ]
