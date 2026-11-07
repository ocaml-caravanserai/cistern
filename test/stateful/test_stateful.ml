(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* Pool_state against the reference of model.ml (DESIGN.md, "Pool" and "Tests").
   A value of the test is one state with its time, and the entries and waiters
   that the test holds, as a shell would hold them. Each operation makes a new
   value. After each call, the invariant compares the outcome and the observable
   state of the two sides, and checks the invariants of the pool on the system
   side.

   The system side also models what the opener knows: its doorbell (each wake
   rings it, and a tick clears it) and the timer that its last tick armed. An
   invariant requires that the opener knows each connect to start, each entry to
   close and each timer of the state.

   The seeds. The default run (dune runtest and the mutation gate) uses one
   fixed root seed, thus a fixed list of 1000 case seeds that meet every cover
   demand. A random seed could fail a cover demand with no bug, and the gate
   needs the same cases for each mutant. With CISTERN_SLOW set (make sim-slow,
   the nightly job), the run uses a random root seed, and the test with the tag
   slow runs too, with more cases. If a change of the generators makes a cover
   demand fail with the fixed seed, choose another seed and examine it in the
   slow run first. *)

open Windtrap
module P = Cistern.Private.Pool_state
module Span = Weft.Span
module M = Model

let ns = Span.of_ns

(* The system side. *)
type s =
  { st: (string, int) P.t
  ; now: Span.t
  ; held: (int * string P.entry) list  (** The entries in use, by id. *)
  ; wids: (int * P.waiter_id) list  (** Each waiter id that a checkout gave. *)
  ; closing_ids: int list  (** The entries that tick gave to close. *)
  ; opened_ids: int list  (** Each entry that opened made. *)
  ; gone: int list  (** The entries that went out of the state. *)
  ; bell: bool  (** The doorbell of the opener. *)
  ; armed: Span.t option  (** The timer of the last tick. *)
  ; next_conn: int
  ; next_label: int
  ; out: string  (** The outcome of the last operation. *) }

(* The reference side. *)
type r = {m: M.t; label: int; rout: string; steps: int}

let error_text = function
  | Some e ->
      Format.asprintf "%a" Cistern_core.Error.pp_summary e
  | None ->
      "-"

let span_text = function
  | Some t ->
      Int64.to_string (Span.to_ns t)
  | None ->
      "-"

let check_entry_fresh (s : s) (e : string P.entry) =
  (* No hand-out of an entry past its life or its idle limit. *)
  ( match e.expires with
  | Some x ->
      is_true ~msg:"a fresh entry" (Span.compare s.now x < 0)
  | None ->
      () ) ;
  is_true ~msg:"within its idle limit"
    ( Int64.compare
        (Int64.sub (Span.to_ns s.now) (Span.to_ns e.last_used))
        (Span.to_ns e.idle_limit)
    < 0 )

(* The life of the test pools: 2 s, so that entries expire often. *)
let life = 2

(* The jitter, with no formula of the model: the life of an entry is 0.85 to
   1.15 of max_lifetime. *)
let check_life (e : string P.entry) =
  match e.expires with
  | Some x ->
      let l = Int64.sub (Span.to_ns x) (Span.to_ns e.opened) in
      let max_lifetime = Span.to_ns (Span.s life) in
      is_true ~msg:"the life of an entry is in the range of the jitter"
        ( Int64.compare (Int64.mul 100L l) (Int64.mul 85L max_lifetime) >= 0
        && Int64.compare (Int64.mul 100L l) (Int64.mul 115L max_lifetime) <= 0
        )
  | None ->
      fail "an entry with no end of life"

let ids_of es = List.map (fun (e : _ P.entry) -> e.id) es

(* The invariants of the pool, on the system side. *)
let invariants (s : s) =
  let st = s.st in
  let idle = P.idle st in
  equal ~msg:"in_use is the held entries" int (List.length s.held) (P.in_use st) ;
  is_true ~msg:"the slot bound"
    (List.length idle + P.in_use st + P.opening st + P.closing st <= P.size st) ;
  equal ~msg:"free" int
    (P.size st - (List.length idle + P.in_use st + P.opening st + P.closing st))
    (P.free st) ;
  equal ~msg:"closing is to close and in close" int
    (List.length (P.to_close st) + List.length s.closing_ids)
    (P.closing st) ;
  (* Conservation: each entry that opened made is in exactly one place: idle,
     one holder, to close, in close, or out of the state. *)
  let places =
    ids_of idle @ List.map fst s.held
    @ ids_of (P.to_close st)
    @ s.closing_ids @ s.gone
  in
  equal ~msg:"each entry in exactly one place" (list int)
    (List.sort compare s.opened_ids)
    (List.sort compare places) ;
  List.iter check_life idle ;
  List.iter (fun (_, e) -> check_life e) s.held ;
  if P.waiting st > 0 then
    is_true ~msg:"no idle entry while a fiber waits" (idle = []) ;
  (* Progress. *)
  let tick_now = (snd (P.tick st s.now)).connects in
  if
    (not (P.is_closed st))
    && P.waiting st > P.opening st
    && P.free st > 0
    && P.opening st < 2
  then
    is_true ~msg:"progress: a connect starts now or at the timer"
      (tick_now > 0 || P.next_timer st s.now <> None) ;
  (* The opener knows its work: its doorbell rang, or it armed a timer that is
     not later. *)
  let knows x =
    s.bell
    || match s.armed with Some a -> Span.compare a x <= 0 | None -> false
  in
  if tick_now > 0 || (snd (P.tick st s.now)).to_close <> [] then
    is_true ~msg:"the opener knows the work of a tick now" (knows s.now) ;
  (* The timer: the opener knows it, it is later than now, no connect before it,
     a connect at it. *)
  match P.next_timer st s.now with
  | None ->
      ()
  | Some t ->
      is_true ~msg:"the opener knows the timer" (knows t) ;
      is_true ~msg:"the timer is later than now" (Span.compare t s.now > 0) ;
      equal ~msg:"no connect before the timer" int 0 tick_now ;
      equal ~msg:"no connect 1 ns before the timer" int 0
        (snd (P.tick st (ns (Int64.pred (Span.to_ns t))))).connects ;
      is_true ~msg:"a connect at the timer" ((snd (P.tick st t)).connects > 0)

(* The two sides agree. *)
let agree (r : r) (s : s) =
  let m = r.m in
  equal ~msg:"the outcome" string r.rout s.out ;
  equal ~msg:"the time" int64 m.now (Span.to_ns s.now) ;
  equal ~msg:"idle" (list int)
    (List.map (fun (e : M.entry) -> e.id) m.idle)
    (List.map (fun (e : _ P.entry) -> e.id) (P.idle s.st)) ;
  equal ~msg:"in use" int (List.length m.used) (P.in_use s.st) ;
  equal ~msg:"opening" int m.opening (P.opening s.st) ;
  equal ~msg:"closing" int
    (List.length m.to_close + List.length m.in_close)
    (P.closing s.st) ;
  equal ~msg:"waiting" int (List.length m.waiters) (P.waiting s.st) ;
  equal ~msg:"the error" string
    (Option.value ~default:"-" m.error)
    (error_text (P.error s.st)) ;
  equal ~msg:"the backoff" string
    (match m.backoff_end with Some t -> Int64.to_string t | None -> "-")
    (span_text (P.backoff_end s.st)) ;
  equal ~msg:"the timer" string
    (match M.next_timer m with Some t -> Int64.to_string t | None -> "-")
    (span_text (P.next_timer s.st s.now)) ;
  equal ~msg:"wake" bool (M.wake m)
    ( (snd (P.tick s.st s.now)).connects > 0
    || (snd (P.tick s.st s.now)).to_close <> []
    || P.next_timer s.st s.now <> None ) ;
  List.iter
    (fun (e : M.entry) ->
      match List.assoc_opt e.id s.held with
      | Some se ->
          equal ~msg:"the life of an entry" (option int64) e.expires
            (Option.map Span.to_ns se.expires) ;
          equal ~msg:"the idle limit" int64 e.idle_limit
            (Span.to_ns se.idle_limit) ;
          equal ~msg:"the dirty mark" bool e.dirty se.dirty
      | None ->
          fail "an entry in use on one side only" )
    m.used ;
  invariants s

let pool = abstract "p" ~invariant:(fun r s -> agree !r !s)

let create size =
  ( {m= M.create ~size ~life:(M.sec life); label= 0; rout= "created"; steps= 0}
  , { st= P.create ~size ~max_lifetime:(Span.s life)
    ; now= Span.zero
    ; held= []
    ; wids= []
    ; closing_ids= []
    ; opened_ids= []
    ; gone= []
    ; bell= false
    ; armed= None
    ; next_conn= 0
    ; next_label= 0
    ; out= "created" } )

let wake_text b = if b then " wake" else ""

(* The operations of the system side. Each gives the outcome text of the
   model. *)

let s_checkout (s : s) =
  let w = s.next_label in
  let st, r, wake = P.checkout s.st s.now w in
  let s = {s with st; next_label= w + 1; bell= s.bell || wake} in
  match r with
  | P.Conn e ->
      check_entry_fresh s e ;
      { s with
        held= (e.id, e) :: s.held
      ; out= Printf.sprintf "conn %d%s" e.id (wake_text wake) }
  | P.Wait id ->
      { s with
        wids= (P.waiter_id id, id) :: s.wids
      ; out= Printf.sprintf "wait %d%s" (P.waiter_id id) (wake_text wake) }
  | P.Closed ->
      {s with out= "closed" ^ wake_text wake}

let r_checkout (r : r) =
  let m, out = M.checkout r.m r.label in
  let moved = List.length m.to_close - List.length r.m.to_close in
  cover "an old idle entry moves at checkout" (moved > 0) ;
  cover "an old entry under a fresh one moves at checkout"
    ( moved > 0
    &&
    match r.m.idle with
    | top :: _ ->
        List.exists (fun (e : M.entry) -> e.id = top.id) m.used
    | [] ->
        false ) ;
  cover "two old entries move at one checkout" (moved > 1) ;
  cover "a checkout on a closed pool" r.m.closed ;
  let out = out ^ wake_text (M.wake m) in
  {r with m; label= r.label + 1; rout= out}

let give_text w (e : _ P.entry) wake =
  Printf.sprintf "give %d %d%s" w e.id (wake_text wake)

let s_checkin id reusable dirty (s : s) =
  let e = List.assoc id s.held in
  let held = List.remove_assoc id s.held in
  let st, r, wake = P.checkin s.st s.now e ~reusable ~dirty in
  let s = {s with st; held; bell= s.bell || wake} in
  match r with
  | P.Give (w, e) ->
      check_entry_fresh s e ;
      {s with held= (e.id, e) :: s.held; out= give_text w e wake}
  | P.Idle ->
      {s with out= "idle" ^ wake_text wake}
  | P.Closing ->
      {s with out= "closing" ^ wake_text wake}
  | P.Freed ->
      {s with gone= id :: s.gone; out= "freed" ^ wake_text wake}
  | P.Abort c ->
      {s with gone= id :: s.gone; out= "abort " ^ c ^ wake_text wake}

let r_checkin id reusable dirty (r : r) =
  let m, out = M.checkin r.m id ~reusable ~dirty in
  cover "a checkin gives to a waiter" (String.starts_with ~prefix:"give" out) ;
  cover "a checkin of an entry past its life" (out = "closing") ;
  cover "a checkin on a closed pool" (String.starts_with ~prefix:"abort" out) ;
  {r with m; rout= out ^ wake_text (M.wake m)}

let s_cancel id (s : s) =
  let st, found, err = P.cancel_wait s.st s.now (List.assoc id s.wids) in
  {s with st; out= Printf.sprintf "found %b error %s" found (error_text err)}

let r_cancel id (r : r) =
  let m, out = M.cancel_wait r.m id in
  cover "a cancel of a waiter that got an entry" (List.mem id r.m.given) ;
  cover "a cancel of a waiter in the queue" (List.mem_assoc id r.m.waiters) ;
  {r with m; rout= out}

let s_tick (s : s) =
  let st, t = P.tick s.st s.now in
  { s with
    st
  ; bell= false
  ; armed= t.next_timer
  ; closing_ids=
      s.closing_ids @ List.map (fun (e : _ P.entry) -> e.id) t.to_close
  ; out=
      Printf.sprintf "connects %d close [%s] timer %s" t.connects
        (String.concat ";"
           (List.map (fun (e : _ P.entry) -> string_of_int e.id) t.to_close) )
        (span_text t.next_timer) }

let r_tick (r : r) =
  let m, out = M.tick r.m in
  {r with m; rout= out}

let opened_text wake = function
  | P.Opened_give (w, (e : _ P.entry)) ->
      Printf.sprintf "give %d %d%s" w e.id (wake_text wake)
  | P.Opened_idle ->
      "idle" ^ wake_text wake
  | P.Opened_close c ->
      "close " ^ c ^ wake_text wake

let s_opened (wt, u) (s : s) =
  let c = Printf.sprintf "c%d" s.next_conn in
  let st, r, wake =
    P.opened s.st s.now c ~wait_timeout:wt ~dialect:Cistern_core.Dialect.Mysql
      ~u
  in
  let s = {s with st; next_conn= s.next_conn + 1; bell= s.bell || wake} in
  match r with
  | P.Opened_give (_, e) ->
      { s with
        held= (e.id, e) :: s.held
      ; opened_ids= e.id :: s.opened_ids
      ; out= opened_text wake r }
  | P.Opened_idle ->
      let e = List.hd (P.idle st) in
      {s with opened_ids= e.id :: s.opened_ids; out= opened_text wake r}
  | P.Opened_close _ ->
      {s with out= opened_text wake r}

let r_opened (wt, u) (r : r) =
  let m, out = M.opened r.m ~wait_timeout:wt ~u in
  cover "opened gives to a waiter" (String.starts_with ~prefix:"give" out) ;
  cover "opened with the idle limit of 30 s" (wt >= 35) ;
  cover "opened with a jitter out of the range"
    (Float.is_nan u || Float.abs u > 0.15) ;
  cover "opened on a closed pool" r.m.closed ;
  let out = out ^ wake_text (M.wake m) in
  {r with m; rout= out}

let failure = Cistern_core.Error.Connect "refused"

let s_failed (s : s) =
  let st, wake = P.open_failed s.st s.now failure in
  {s with st; bell= s.bell || wake; out= "failed" ^ wake_text wake}

let r_failed (r : r) =
  let m, out = M.open_failed r.m "Connect" in
  cover "a second failure in a row" (r.m.error <> None) ;
  {r with m; rout= out ^ wake_text (M.wake m)}

let s_aborted (s : s) =
  let st, wake = P.open_aborted s.st s.now in
  {s with st; bell= s.bell || wake; out= "aborted" ^ wake_text wake}

let r_aborted (r : r) =
  let m, out = M.open_aborted r.m in
  {r with m; rout= out ^ wake_text (M.wake m)}

(* closed_done of an id: an entry in close, or an id that no entry has. *)
let unknown_id = 1_000_000

let s_done id (s : s) =
  let st, wake = P.closed_done s.st s.now id in
  let found = List.mem id s.closing_ids in
  { s with
    st
  ; bell= s.bell || wake
  ; closing_ids= List.filter (( <> ) id) s.closing_ids
  ; gone= (if found then id :: s.gone else s.gone)
  ; out= Printf.sprintf "done %d%s" id (wake_text wake) }

let r_done id (r : r) =
  cover "closed_done of an entry in close" (id <> unknown_id) ;
  cover "closed_done of an unknown id" (id = unknown_id) ;
  let m, out = M.closed_done r.m id in
  {r with m; rout= Printf.sprintf "%s %d%s" out id (wake_text (M.wake m))}

let s_close_all (s : s) =
  let st, waiters, entries = P.close_all s.st s.now in
  { s with
    st
  ; closing_ids= []
  ; gone= ids_of entries @ s.gone
  ; out=
      Printf.sprintf "waiters [%s] close [%s]"
        (String.concat ";" (List.map string_of_int waiters))
        (String.concat ";"
           (List.map (fun (e : _ P.entry) -> string_of_int e.id) entries) ) }

let r_close_all (r : r) =
  let m, out = M.close_all r.m in
  cover "close_all with entries in close" (r.m.in_close <> []) ;
  {r with m; rout= out}

(* A run of failed connects: up to 9 times, the time goes to the end of the
   backoff, a tick starts connects, and each of them fails. Thus the step of the
   backoff reaches its maximum of 5 s. *)
let fail_run_text outs = String.concat " / " (List.rev outs)

let r_fail_run (r : r) =
  let rec go (r : r) k outs =
    let m = r.m in
    let now =
      match m.backoff_end with Some e when e > m.now -> e | _ -> m.now
    in
    let r = {r with m= {m with now}} in
    let before = r.m.opening in
    let r = r_tick r in
    let n = r.m.opening - before in
    let outs = r.rout :: outs in
    if n = 0 || k = 1 then (r, outs)
    else
      let r, outs =
        List.fold_left
          (fun (r, outs) _ ->
            let r = r_failed r in
            (r, r.rout :: outs) )
          (r, outs) (List.init n Fun.id)
      in
      go r (k - 1) outs
  in
  let r, outs = go r 9 [] in
  cover "the backoff step at its maximum" (r.m.step = M.sec 5) ;
  {r with rout= fail_run_text outs}

let s_fail_run (s : s) =
  let rec go (s : s) k outs =
    let now =
      match P.backoff_end s.st with
      | Some e when Span.compare e s.now > 0 ->
          e
      | _ ->
          s.now
    in
    let s = {s with now} in
    let before = P.opening s.st in
    let s = s_tick s in
    let n = P.opening s.st - before in
    let outs = s.out :: outs in
    if n = 0 || k = 1 then (s, outs)
    else
      let s, outs =
        List.fold_left
          (fun (s, outs) _ ->
            let s = s_failed s in
            (s, s.out :: outs) )
          (s, outs) (List.init n Fun.id)
      in
      go s (k - 1) outs
  in
  let s, outs = go s 9 [] in
  {s with out= fail_run_text outs}

(* The advances of the time. Each side finds its target in its own state; a
   difference shows in the next comparison. *)
type advance =
  | Forward of int  (** ms *)
  | Back of int  (** µs, a small step back *)
  | To_timer
  | To_life of int  (** ns from the end of the life of the oldest entry *)
  | To_idle of int  (** ns from the idle limit of the oldest idle entry *)
  | To_top_life of int
      (** ns from the end of the life of the top of the idle stack *)
  | To_top_idle of int  (** ns from the idle limit of the top of the stack *)
  | To_backoff of int  (** ns from the end of the backoff *)

let advance_text = function
  | Forward n ->
      Printf.sprintf "forward %dms" n
  | Back n ->
      Printf.sprintf "back %dus" n
  | To_timer ->
      "to the timer"
  | To_life d ->
      Printf.sprintf "to the life %+dns" d
  | To_idle d ->
      Printf.sprintf "to the idle limit %+dns" d
  | To_top_life d ->
      Printf.sprintf "to the life of the top %+dns" d
  | To_top_idle d ->
      Printf.sprintf "to the idle limit of the top %+dns" d
  | To_backoff d ->
      Printf.sprintf "to the backoff %+dns" d

let advance_gen =
  Gen.with_pp
    (fun ppf a -> Format.pp_print_string ppf (advance_text a))
    (Gen.one_of
       [ Gen.map (fun n -> Forward n) (Gen.int_range 0 3000)
       ; Gen.map (fun n -> Forward n) (Gen.int_range 5000 40000)
       ; Gen.map (fun n -> Back n) (Gen.int_range 1 1000)
       ; Gen.constant To_timer
       ; Gen.map (fun d -> To_life d) (Gen.int_range (-1) 1)
       ; Gen.map (fun d -> To_idle d) (Gen.int_range (-1) 1)
       ; Gen.map (fun d -> To_top_life d) (Gen.int_range (-1) 1)
       ; Gen.map (fun d -> To_top_idle d) (Gen.int_range (-1) 1)
       ; Gen.map (fun d -> To_backoff d) (Gen.int_range (-1) 1) ] )

let oldest_m (es : M.entry list) =
  match List.sort (fun (a : M.entry) b -> compare a.id b.id) es with
  | e :: _ ->
      Some e
  | [] ->
      None

let r_advance a (r : r) =
  let m = r.m in
  cover "an advance to the timer" (a = To_timer && M.next_timer m <> None) ;
  let target =
    match a with
    | Forward n ->
        Some (Int64.add m.now (M.ms n))
    | Back n ->
        Some (Int64.sub m.now (Int64.mul (Int64.of_int n) 1000L))
    | To_timer ->
        M.next_timer m
    | To_life d ->
        Option.bind
          (oldest_m (m.idle @ m.used))
          (fun e -> Option.map (fun x -> Int64.add x (Int64.of_int d)) e.expires)
    | To_idle d ->
        Option.map
          (fun (e : M.entry) ->
            Int64.add (Int64.add e.last_used e.idle_limit) (Int64.of_int d) )
          (oldest_m m.idle)
    | To_top_life d ->
        Option.bind (List.nth_opt m.idle 0) (fun (e : M.entry) ->
            Option.map (fun x -> Int64.add x (Int64.of_int d)) e.expires )
    | To_top_idle d ->
        Option.map
          (fun (e : M.entry) ->
            Int64.add (Int64.add e.last_used e.idle_limit) (Int64.of_int d) )
          (List.nth_opt m.idle 0)
    | To_backoff d ->
        Option.map (fun t -> Int64.add t (Int64.of_int d)) m.backoff_end
  in
  let now = Option.value ~default:m.now target in
  {r with m= {m with now}; rout= "now " ^ Int64.to_string now}

let oldest_s (es : string P.entry list) =
  match List.sort (fun (a : _ P.entry) b -> compare a.id b.id) es with
  | e :: _ ->
      Some e
  | [] ->
      None

let s_advance a (s : s) =
  let add t d = ns (Int64.add (Span.to_ns t) (Int64.of_int d)) in
  let target =
    match a with
    | Forward n ->
        Some (add s.now (n * 1_000_000))
    | Back n ->
        Some (add s.now (-n * 1000))
    | To_timer ->
        P.next_timer s.st s.now
    | To_life d ->
        Option.bind
          (oldest_s (P.idle s.st @ List.map snd s.held))
          (fun e -> Option.map (fun x -> add x d) e.expires)
    | To_idle d ->
        Option.map
          (fun (e : _ P.entry) ->
            add
              (ns
                 (Int64.add (Span.to_ns e.last_used) (Span.to_ns e.idle_limit)) )
              d )
          (oldest_s (P.idle s.st))
    | To_top_life d ->
        Option.bind
          (List.nth_opt (P.idle s.st) 0)
          (fun (e : _ P.entry) -> Option.map (fun x -> add x d) e.expires)
    | To_top_idle d ->
        Option.map
          (fun (e : _ P.entry) ->
            add
              (ns
                 (Int64.add (Span.to_ns e.last_used) (Span.to_ns e.idle_limit)) )
              d )
          (List.nth_opt (P.idle s.st) 0)
    | To_backoff d ->
        Option.map (fun t -> add t d) (P.backoff_end s.st)
  in
  let now = Option.value ~default:s.now target in
  {s with now; out= "now " ^ Int64.to_string (Span.to_ns now)}

(* A new pool with [k] idle entries, so that a short case reaches the deep paths
   (old entries at checkout, the polite closes). [k] times: a checkout waits, a
   tick starts a connect, and opened gives the entry to the waiter. Then each
   entry goes back to the idle stack, 300 ms after the one before. The
   wait_timeout alternates between 40 s and 3 s (idle limits of 30 s and 1
   s). *)
let warm_wt i = if i mod 2 = 0 then 40 else 3

let warm_r (size, k) =
  let r = fst (create size) in
  let rec take (r : r) i =
    if i = k then r
    else
      let r = r_checkout r in
      let r = r_tick r in
      take (r_opened (warm_wt i, 0.) r) (i + 1)
  in
  let r = take r 0 in
  let ids = List.rev_map (fun (e : M.entry) -> e.id) r.m.used in
  let r =
    List.fold_left
      (fun r id -> r_advance (Forward 300) (r_checkin id true false r))
      r ids
  in
  {r with rout= "created"}

let warm_s (size, k) =
  let s = snd (create size) in
  let rec take (s : s) i =
    if i = k then s
    else
      let s = s_checkout s in
      let s = s_tick s in
      take (s_opened (warm_wt i, 0.) s) (i + 1)
  in
  let s = take s 0 in
  let ids = List.rev_map fst s.held in
  let s =
    List.fold_left
      (fun s id -> s_advance (Forward 300) (s_checkin id true false s))
      s ids
  in
  {s with out= "created"}

(* The elements: the entries in use, the waiter ids (in the queue, or given),
   and the entries in close, from the reference side. *)
let held =
  among int pool (fun r -> List.map (fun (e : M.entry) -> e.id) !r.m.used)

let waiter = among int pool (fun r -> List.init !r.m.next_waiter Fun.id)

let in_close =
  among int pool (fun r -> List.map (fun (e : M.entry) -> e.id) !r.m.in_close)

(* The size, and the idle entries at the start (0 to the size). *)
let size_gen =
  Gen.with_pp
    (fun ppf (n, k) -> Format.fprintf ppf "(size %d, idle %d)" n k)
    (Gen.bind (Gen.int_range 1 4) (fun n ->
         Gen.map (fun k -> (n, k)) (Gen.int_range 0 n) ) )

(* The wait_timeout: up to 12 s (an idle limit of 1 to 7 s), or 30 to 45 s (the
   maximum of 30 s applies from 35 s). The jitter: mostly from -0.2 to 0.2, and
   rarely a value that the operation must cut. *)
let opened_gen =
  Gen.with_pp
    (fun ppf (w, u) -> Format.fprintf ppf "(wait_timeout %d, u %g)" w u)
    (Gen.pair
       (Gen.one_of [Gen.int_range 1 12; Gen.int_range 30 45])
       (Gen.frequency
          [ ( 9
            , Gen.map (fun i -> float_of_int i /. 100.) (Gen.int_range (-20) 20)
            )
          ; ( 1
            , Gen.of_list
                [Float.nan; Float.infinity; Float.neg_infinity; 1.; -1.] ) ] ) )

let flag = Gen.with_pp Format.pp_print_bool Gen.bool

(* An operation on the two sides: each side changes its value in place and gives
   the outcome text, which the test compares. *)
let on_r f r =
  r := f !r ;
  r := {!r with steps= !r.steps + 1} ;
  !r.rout

let on_s f s =
  s := f !s ;
  !s.out

let op name ?pre sig_ rf sf = command name ?pre sig_ rf sf

let commands =
  let checkout =
    op "checkout" (pool ^-> returns string) (on_r r_checkout) (on_s s_checkout)
  and checkin =
    op "checkin"
      (pool ^-> held ^-> flag @-> flag @-> returns string)
      (fun r id reusable dirty -> on_r (r_checkin id reusable dirty) r)
      (fun s id reusable dirty -> on_s (s_checkin id reusable dirty) s)
  and tick = op "tick" (pool ^-> returns string) (on_r r_tick) (on_s s_tick)
  and opened =
    op "opened"
      ~pre:(fun r _ -> !r.m.opening > 0)
      (pool ^-> opened_gen @-> returns string)
      (fun r x -> on_r (r_opened x) r)
      (fun s x -> on_s (s_opened x) s)
  and closed_done =
    op "closed_done"
      (pool ^-> in_close ^-> returns string)
      (fun r id -> on_r (r_done id) r)
      (fun s id -> on_s (s_done id) s)
  and advance =
    op "advance"
      (pool ^-> advance_gen @-> returns string)
      (fun r a -> on_r (r_advance a) r)
      (fun s a -> on_s (s_advance a) s)
  in
  (* A command listed more than one time is drawn more often. close_all ends the
     work of a pool, thus it is listed once, and only after 15 steps of the
     case: the cases then reach the deep paths before the end. *)
  [ command "create"
      (size_gen @-> makes pool)
      (fun x -> ref (warm_r x))
      (fun x -> ref (warm_s x)) ]
  @ List.concat_map
      (fun c -> [c; c; c])
      [checkout; checkin; tick; opened; closed_done; advance]
  @ [ op "cancel_wait"
        (pool ^-> waiter ^-> returns string)
        (fun r id -> on_r (r_cancel id) r)
        (fun s id -> on_s (s_cancel id) s)
    ; op "open_failed"
        ~pre:(fun r -> !r.m.opening > 0)
        (pool ^-> returns string)
        (on_r r_failed) (on_s s_failed)
    ; op "open_aborted"
        ~pre:(fun r -> !r.m.opening > 0)
        (pool ^-> returns string)
        (on_r r_aborted) (on_s s_aborted)
    ; op "closed_done of an unknown id"
        (pool ^-> returns string)
        (on_r (r_done unknown_id))
        (on_s (s_done unknown_id))
    ; op "a run of failed connects"
        ~pre:(fun r ->
          (not !r.m.closed)
          && M.demand !r.m > 0
          && M.free !r.m > 0
          && !r.m.opening < 2 )
        (pool ^-> returns string)
        (on_r r_fail_run) (on_s s_fail_run)
    ; op "close_all"
        ~pre:(fun r -> !r.steps >= 15)
        (pool ^-> returns string)
        (on_r r_close_all) (on_s s_close_all) ]

(* The fixed root seed of the default run (see the comment at the top). *)
let seed = "s1:5ca1ab1e0c15ed00"

(* Without CISTERN_SLOW, the slow test is not in the list (a tag filter would
   make the run partial, and the mutation gate would not keep its verdicts). *)
let () =
  let default =
    stateful ~count:1000 ~steps:100 "Pool_state behaves like the model" commands
  in
  let argv, tests =
    match Sys.getenv_opt "CISTERN_SLOW" with
    | Some _ ->
        ( Sys.argv
        , [ default
          ; tag "slow"
              (stateful ~count:10000 ~steps:100
                 "Pool_state behaves like the model, many random cases" commands )
          ] )
    | None ->
        (* The flags of the command line come after, thus they win. *)
        ( Array.concat
            [ [|Sys.argv.(0); "--seed"; seed|]
            ; Array.sub Sys.argv 1 (Array.length Sys.argv - 1) ]
        , [default] )
  in
  exit (Windtrap.run ~argv "stateful" tests)
