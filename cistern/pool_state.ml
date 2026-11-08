(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

module Span = Weft.Span
module Error = Cistern_core.Error
module Dialect = Cistern_core.Dialect

type time = Span.t

type 'c entry =
  { id: int
  ; conn: 'c
  ; opened: time
  ; expires: time option
  ; idle_limit: time
  ; last_used: time
  ; dirty: bool }

type waiter_id = int

let waiter_id id = id

type token = int

let token_id tok = tok

(* A queue as two lists: the front, and the back in reverse order. *)
type 'a queue = {front: 'a list; back: 'a list}

let empty_queue = {front= []; back= []}

let push q x = {q with back= x :: q.back}

let pop q =
  match q.front with
  | x :: front ->
      Some (x, {q with front})
  | [] -> (
    match List.rev q.back with
    | x :: front ->
        Some (x, {front; back= []})
    | [] ->
        None )

let queue_length q = List.length q.front + List.length q.back

let queue_to_list q = q.front @ List.rev q.back

type ('c, 'w) t =
  { size: int
  ; max_lifetime: Span.t
  ; idle: 'c entry list
  ; in_use: int
  ; opening: int
  ; to_close: 'c entry list
  ; in_close: 'c entry list
  ; waiters: (waiter_id * 'w) queue
  ; error: Error.t option
  ; backoff_end: time option
  ; backoff_step: Span.t
  ; closed: bool
  ; last_dialect: Dialect.t option
  ; killing: (token * time) list
        (* The Killing slots with their age limits, the oldest first. *)
  ; next_entry: int
  ; next_waiter: int
  ; next_token: int }

let first_step () = Cistern_core.Limits.backoff_first

let max_step () = Cistern_core.Limits.backoff_max

let max_connects = Cistern_core.Limits.concurrent_connects

let age_limit () = Cistern_core.Limits.killing_age

let create ~size ~max_lifetime =
  if size < 1 || size > 1000 then
    invalid_arg "Pool_state.create: the size must be 1 to 1000" ;
  if Span.compare max_lifetime Span.zero <= 0 then
    invalid_arg "Pool_state.create: max_lifetime must be positive" ;
  { size
  ; max_lifetime
  ; idle= []
  ; in_use= 0
  ; opening= 0
  ; to_close= []
  ; in_close= []
  ; waiters= empty_queue
  ; error= None
  ; backoff_end= None
  ; backoff_step= first_step ()
  ; closed= false
  ; last_dialect= None
  ; killing= []
  ; next_entry= 0
  ; next_waiter= 0
  ; next_token= 0 }

(* The arithmetic of spans, with no overflow: None if the sum is too large. *)
let add a b =
  let x = Span.to_ns a and y = Span.to_ns b in
  let r = Int64.add x y in
  if Int64.logand (Int64.logxor x r) (Int64.logxor y r) < 0L then None
  else Some (Span.of_ns r)

let sub a b = Span.of_ns (Int64.sub (Span.to_ns a) (Span.to_ns b))

let ( >=. ) a b = Span.compare a b >= 0

let ( <. ) a b = Span.compare a b < 0

let max_span a b = if a >=. b then a else b

let min_span a b = if a <. b then a else b

let free s =
  s.size
  - ( List.length s.idle + s.in_use + s.opening + List.length s.to_close
    + List.length s.in_close + List.length s.killing )

(* The Killing slots past their age limit at [now] are free. Each operation
   first removes them, thus no operation sees such a slot as used. *)
let expire s now =
  {s with killing= List.filter (fun (_, a) -> now <. a) s.killing}

let demand s = Int.max 0 (queue_length s.waiters - s.opening)

let in_backoff s now =
  match s.backoff_end with Some e -> now <. e | None -> false

(* The connects that tick would start at [now]. *)
let starts s now =
  if s.closed || in_backoff s now then 0
  else
    Int.max 0 (Int.min (max_connects - s.opening) (Int.min (free s) (demand s)))

(* The timer in a closed form, in O(k) for k Killing slots, because it runs
   under the mutex of the pool after each operation (review C of 1.5a, M1). The
   time does not change [closed], the demand or the running connects. A tick at
   [t] starts a connect when [t] is not in the backoff and a slot is free at
   [t]. The free slots only grow with the time: a Killing slot frees its slot at
   its age limit. Thus the timer is the end of the backoff when a slot is free
   now, else the later of the end of the backoff and the earliest age limit. A
   tick now that starts a connect gives no timer: that tick gives the timer of
   its new state. The model of the stateful test finds the timer with a search,
   as a check of this form. *)
let next_timer s now =
  let s = expire s now in
  if s.closed || demand s = 0 || s.opening >= max_connects then None
  else if free s > 0 then if in_backoff s now then s.backoff_end else None
  else
    let earliest =
      List.fold_left
        (fun m (_, a) -> match m with Some b when b <. a -> m | _ -> Some a)
        None s.killing
    in
    match (earliest, s.backoff_end) with
    | Some a, Some e ->
        Some (max_span a e)
    | a, _ ->
        a

(* The opener has work now, or a timer to arm. The opener reads its timer only
   in a tick, thus an operation that gives a timer must wake it. A wake while a
   timer is armed costs one tick, and it is not a spin: next_timer gives only a
   time later than now. *)
let wake s now =
  starts s now > 0 || s.to_close <> [] || next_timer s now <> None

(* An idle entry that passed its life or its idle limit at [now]. *)
let too_old e now =
  (match e.expires with Some x -> now >=. x | None -> false)
  || sub now e.last_used >=. e.idle_limit

type ('c, 'w) checkout = Conn of 'c entry | Wait of waiter_id | Closed

let checkout s now w =
  let s = expire s now in
  if s.closed then (s, Closed, false)
  else
    (* All the old entries move, also those under a fresh entry. *)
    let old, fresh = List.partition (fun e -> too_old e now) s.idle in
    let s = {s with idle= fresh; to_close= s.to_close @ old} in
    let s, r =
      match fresh with
      | e :: idle ->
          ({s with idle; in_use= s.in_use + 1}, Conn e)
      | [] ->
          let id = s.next_waiter in
          ( {s with waiters= push s.waiters (id, w); next_waiter= id + 1}
          , Wait id )
    in
    (s, r, wake s now)

let cancel_wait s now id =
  let s = expire s now in
  let all = queue_to_list s.waiters in
  let rest = List.filter (fun (i, _) -> i <> id) all in
  let found = List.length rest < List.length all in
  let s = {s with waiters= {front= rest; back= []}} in
  (s, found, s.error, wake s now)

type ('c, 'w) handed =
  | Give of 'w * 'c entry
  | Idle
  | Closing
  | Freed
  | Abort of 'c

let checkin s now e ~reusable ~dirty =
  let s = expire s now in
  let s' = {s with in_use= s.in_use - 1} in
  let s, r =
    if not reusable then (s', Freed)
    else if s.closed then (s', Abort e.conn)
    else
      let e = {e with last_used= now; dirty} in
      match e.expires with
      | Some x when now >=. x ->
          ({s' with to_close= s'.to_close @ [e]}, Closing)
      | _ -> (
        match pop s.waiters with
        | Some ((_, w), waiters) ->
            ({s with waiters}, Give (w, e))
        | None ->
            ({s' with idle= e :: s'.idle}, Idle) )
  in
  (s, r, wake s now)

type ('c, 'w) tick =
  {connects: int; to_close: 'c entry list; next_timer: time option}

let tick s now =
  let s = expire s now in
  let n = starts s now in
  let s' =
    { s with
      opening= s.opening + n
    ; to_close= []
    ; in_close= s.in_close @ s.to_close }
  in
  (s', {connects= n; to_close= s.to_close; next_timer= next_timer s' now})

type ('c, 'w) opened =
  | Opened_give of 'w * 'c entry
  | Opened_idle
  | Opened_close of 'c

(* The idle limit of a server whose @@wait_timeout is [w] seconds. *)
let idle_limit w =
  let module L = Cistern_core.Limits in
  max_span L.idle_min
    (min_span L.idle_max
       (Span.of_ns
          (Int64.sub (Span.to_ns (Span.s w)) (Span.to_ns L.idle_offset)) ) )

let opened s now c ~wait_timeout ~dialect ~u =
  let s = expire s now in
  let s = {s with opening= s.opening - 1} in
  if s.closed then (s, Opened_close c, false)
  else
    (* NaN gives 0, the middle of the range: Float.max of NaN is NaN. *)
    let u =
      let j = Cistern_core.Limits.jitter in
      if Float.is_nan u then 0. else Float.min j (Float.max (-.j) u)
    in
    (* The jitter is at most 15 % of the life, thus its product has no
       overflow. *)
    let ns = Span.to_ns s.max_lifetime in
    let life =
      add s.max_lifetime (Span.of_ns (Int64.of_float (Int64.to_float ns *. u)))
    in
    let e =
      { id= s.next_entry
      ; conn= c
      ; opened= now
      ; expires= Option.bind life (add now)
      ; idle_limit= idle_limit wait_timeout
      ; last_used= now
      ; dirty= false }
    in
    let s =
      { s with
        next_entry= s.next_entry + 1
      ; error= None
      ; backoff_end= None
      ; backoff_step= first_step ()
      ; last_dialect= Some dialect }
    in
    let s, r =
      match pop s.waiters with
      | Some ((_, w), waiters) ->
          ({s with waiters; in_use= s.in_use + 1}, Opened_give (w, e))
      | None ->
          ({s with idle= e :: s.idle}, Opened_idle)
    in
    (s, r, wake s now)

let open_failed s now err =
  let s = expire s now in
  let end_ =
    match add now s.backoff_step with
    | None ->
        s.backoff_end
    | Some t -> (
      match s.backoff_end with Some e -> Some (max_span e t) | None -> Some t )
  in
  let s =
    { s with
      opening= s.opening - 1
    ; error= Some err
    ; backoff_end= end_
    ; backoff_step=
        min_span (max_step ())
          (Span.of_ns (Int64.mul 2L (Span.to_ns s.backoff_step))) }
  in
  (s, wake s now)

let open_aborted s now =
  let s = expire s now in
  let s = {s with opening= s.opening - 1} in
  (s, wake s now)

let closed_done s now id =
  let s = expire s now in
  let s = {s with in_close= List.filter (fun e -> e.id <> id) s.in_close} in
  (s, wake s now)

let close_all s now =
  let s = expire s now in
  let waiters = List.map snd (queue_to_list s.waiters) in
  let entries = s.idle @ s.to_close @ s.in_close in
  ( { s with
      closed= true
    ; idle= []
    ; to_close= []
    ; in_close= []
    ; waiters= empty_queue }
  , waiters
  , entries )

(* The age limit of a slot that [killing] keeps at [now]. A time too large for a
   span gives the largest span. *)
let age_end now =
  match add now (age_limit ()) with
  | Some t ->
      t
  | None ->
      Span.of_ns Int64.max_int

let killing s now (_ : 'c entry) =
  let s = expire s now in
  let tok = s.next_token in
  let s =
    { s with
      in_use= s.in_use - 1
    ; killing= s.killing @ [(tok, age_end now)]
    ; next_token= tok + 1 }
  in
  (s, tok, wake s now)

let killed s now tok =
  let s = expire s now in
  let rest = List.filter (fun (t, _) -> t <> tok) s.killing in
  let freed = List.length rest < List.length s.killing in
  let s = {s with killing= rest} in
  (s, freed, wake s now)

let size s = s.size

let is_closed s = s.closed

let idle s = s.idle

let in_use s = s.in_use

let opening s = s.opening

let to_close (s : (_, _) t) = s.to_close

let closing (s : (_, _) t) = List.length s.to_close + List.length s.in_close

let waiting s = queue_length s.waiters

let error s = s.error

let last_dialect s = s.last_dialect

let backoff_end s = s.backoff_end

let killing_slots s = s.killing
