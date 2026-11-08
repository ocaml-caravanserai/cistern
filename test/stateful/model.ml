(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* The reference of the stateful test of Pool_state: the rules of DESIGN.md
   "Pool" and of the mli, written again with plain lists and integer
   nanoseconds, with no code of the library. Each slot is in exactly one list.

   The derived values use a method that is not the method of the code, so that a
   wrong rule is not in the two sides: the connects of a tick are the largest
   count that each limit permits (an enumeration), the timer is the smallest
   candidate time at which a tick starts a connect (a search), wake is "a tick
   now has work or arms a timer", and the checkout filters the stack. The free
   slots at a time [t] count only the Killing slots whose age limit is later
   than [t] (a filter), thus the derived values do not depend on the removal of
   the expired slots. Each operation removes them, as the mli says, so that the
   lists of the two sides agree. *)

type entry =
  { id: int
  ; conn: string
  ; opened: int64
  ; expires: int64 option
  ; idle_limit: int64
  ; last_used: int64
  ; dirty: bool }

type t =
  { size: int
  ; life: int64  (** max_lifetime in ns. *)
  ; now: int64
  ; idle: entry list  (** The next checkout first. *)
  ; used: entry list  (** Held by a fiber, or given to a waiter. *)
  ; opening: int
  ; to_close: entry list  (** The oldest first. *)
  ; in_close: entry list
  ; waiters: (int * int) list  (** (waiter id, value), the oldest first. *)
  ; given: int list  (** Waiter ids that got an entry. *)
  ; error: string option
  ; backoff_end: int64 option
  ; step: int64
  ; closed: bool
  ; killing: (int * int64) list
        (** The Killing slots: (token, age limit), the oldest first. *)
  ; issued: int list  (** Each token that killing gave. *)
  ; freed: int list  (** The tokens that killed freed. *)
  ; next_entry: int
  ; next_waiter: int
  ; next_conn: int
  ; next_token: int }

let ms n = Int64.mul (Int64.of_int n) 1_000_000L

let sec n = Int64.mul (Int64.of_int n) 1_000_000_000L

let create ~size ~life =
  { size
  ; life
  ; now= 0L
  ; idle= []
  ; used= []
  ; opening= 0
  ; to_close= []
  ; in_close= []
  ; waiters= []
  ; given= []
  ; error= None
  ; backoff_end= None
  ; step= ms 100
  ; closed= false
  ; killing= []
  ; issued= []
  ; freed= []
  ; next_entry= 0
  ; next_waiter= 0
  ; next_conn= 0
  ; next_token= 0 }

(* The age limit of a Killing slot: 30 s. *)
let age = sec 30

(* The Killing slots that hold their slot at [t]. *)
let live_at m t = List.filter (fun (_, a) -> a > t) m.killing

(* The free slots at [t]. *)
let free_at m t =
  m.size - List.length m.idle - List.length m.used - m.opening
  - List.length m.to_close - List.length m.in_close
  - List.length (live_at m t)

let free m = free_at m m.now

(* The rule of each operation: first remove the expired Killing slots. *)
let expire m = {m with killing= live_at m m.now}

let demand m = max 0 (List.length m.waiters - m.opening)

let backoff_at m t = match m.backoff_end with Some e -> t < e | None -> false

(* The connects of a tick at [t]: the largest count that each limit permits. *)
let starts_at m t =
  let permitted n =
    (not m.closed)
    && (not (backoff_at m t))
    && m.opening + n <= 2
    && n <= free_at m t
    && n <= demand m
  in
  List.fold_left
    (fun best n -> if permitted n then n else best)
    0
    (List.init (m.size + 1) Fun.id)

(* The smallest time later than now at which a tick, with no other operation,
   starts a connect. The connects change with the time only at the end of the
   backoff and at the age limits, thus these are the candidates. *)
let next_timer m =
  if starts_at m m.now > 0 then None
  else
    List.fold_left
      (fun best t ->
        if t > m.now && starts_at m t > 0 then
          match best with Some b when b <= t -> best | _ -> Some t
        else best )
      None
      (Option.to_list m.backoff_end @ List.map snd m.killing)

(* The work of a tick at now: the connects, the entries to close, and the timer
   of the state after the tick. *)
let tick_parts m =
  let m = expire m in
  let n = starts_at m m.now in
  let closing = m.to_close in
  let m =
    {m with opening= m.opening + n; to_close= []; in_close= m.in_close @ closing}
  in
  (m, n, closing, next_timer m)

(* The opener must tick: a tick now has work, or arms a timer. *)
let wake m =
  let _, n, closing, timer = tick_parts m in
  n > 0 || closing <> [] || timer <> None

let expired e t = match e.expires with Some x -> t >= x | None -> false

let idle_out e t = Int64.sub t e.last_used >= e.idle_limit

(* The outcome of each operation, as text: the stateful test compares it. *)

let checkout m w =
  let m = expire m in
  if m.closed then (m, "closed")
  else
    (* The rule of the mli: each old entry moves, the oldest place first in the
       stack order; then the top fresh entry goes out. *)
    let old e = expired e m.now || idle_out e m.now in
    let moved = List.filter old m.idle in
    let kept = List.filter (fun e -> not (old e)) m.idle in
    let m = {m with to_close= m.to_close @ moved} in
    match kept with
    | e :: rest ->
        ({m with idle= rest; used= e :: m.used}, Printf.sprintf "conn %d" e.id)
    | [] ->
        let id = m.next_waiter in
        ( {m with idle= []; waiters= m.waiters @ [(id, w)]; next_waiter= id + 1}
        , Printf.sprintf "wait %d" id )

let cancel_wait m id =
  let m = expire m in
  let found = List.mem_assoc id m.waiters in
  ( {m with waiters= List.filter (fun (i, _) -> i <> id) m.waiters}
  , Printf.sprintf "found %b error %s" found (Option.value ~default:"-" m.error)
  )

let checkin m id ~reusable ~dirty =
  let m = expire m in
  let e = List.find (fun e -> e.id = id) m.used in
  let used = List.filter (fun e -> e.id <> id) m.used in
  if not reusable then ({m with used}, "freed")
  else if m.closed then ({m with used}, "abort " ^ e.conn)
  else
    let e = {e with last_used= m.now; dirty} in
    if expired e m.now then
      ({m with used; to_close= m.to_close @ [e]}, "closing")
    else
      match m.waiters with
      | (wid, w) :: waiters ->
          ( {m with used= e :: used; waiters; given= wid :: m.given}
          , Printf.sprintf "give %d %d" w e.id )
      | [] ->
          ({m with used; idle= e :: m.idle}, "idle")

let tick m =
  let m, n, closing, _ = tick_parts m in
  ( m
  , Printf.sprintf "connects %d close [%s] timer %s" n
      (String.concat ";" (List.map (fun e -> string_of_int e.id) closing))
      (match next_timer m with Some t -> Int64.to_string t | None -> "-") )

let idle_limit w = max (sec 1) (min (sec 30) (sec (max 0 (w - 5))))

let opened m ~wait_timeout ~u =
  let m = expire m in
  let m = {m with opening= m.opening - 1} in
  let conn = Printf.sprintf "c%d" m.next_conn in
  let m = {m with next_conn= m.next_conn + 1} in
  if m.closed then (m, "close " ^ conn)
  else
    let u =
      if Float.is_nan u then 0.
      else if u > 0.15 then 0.15
      else if u < -0.15 then -0.15
      else u
    in
    let life = Int64.add m.life (Int64.of_float (Int64.to_float m.life *. u)) in
    let e =
      { id= m.next_entry
      ; conn
      ; opened= m.now
      ; expires= Some (Int64.add m.now life)
      ; idle_limit= idle_limit wait_timeout
      ; last_used= m.now
      ; dirty= false }
    in
    let m =
      { m with
        next_entry= m.next_entry + 1
      ; error= None
      ; backoff_end= None
      ; step= ms 100 }
    in
    match m.waiters with
    | (wid, w) :: waiters ->
        ( {m with used= e :: m.used; waiters; given= wid :: m.given}
        , Printf.sprintf "give %d %d" w e.id )
    | [] ->
        ({m with idle= e :: m.idle}, "idle")

let open_failed m err =
  let m = expire m in
  let t = Int64.add m.now m.step in
  let backoff_end =
    match m.backoff_end with Some e -> Some (max e t) | None -> Some t
  in
  ( { m with
      opening= m.opening - 1
    ; error= Some err
    ; backoff_end
    ; step= min (sec 5) (Int64.mul 2L m.step) }
  , "failed" )

let open_aborted m =
  let m = expire m in
  ({m with opening= m.opening - 1}, "aborted")

let closed_done m id =
  let m = expire m in
  ({m with in_close= List.filter (fun e -> e.id <> id) m.in_close}, "done")

let close_all m =
  let m = expire m in
  let entries = m.idle @ m.to_close @ m.in_close in
  let out =
    Printf.sprintf "waiters [%s] close [%s]"
      (String.concat ";" (List.map (fun (_, w) -> string_of_int w) m.waiters))
      (String.concat ";" (List.map (fun e -> string_of_int e.id) entries))
  in
  ({m with closed= true; idle= []; to_close= []; in_close= []; waiters= []}, out)

(* The entry [id] in use goes out (the shell aborted it), and its slot stays
   until the age limit or killed. *)
let killing m id =
  let m = expire m in
  let tok = m.next_token in
  ( { m with
      used= List.filter (fun e -> e.id <> id) m.used
    ; killing= m.killing @ [(tok, Int64.add m.now age)]
    ; issued= tok :: m.issued
    ; next_token= tok + 1 }
  , Printf.sprintf "token %d" tok )

let killed m tok =
  let m = expire m in
  let found = List.mem_assoc tok m.killing in
  ( { m with
      killing= List.filter (fun (k, _) -> k <> tok) m.killing
    ; freed= (if found then tok :: m.freed else m.freed) }
  , Printf.sprintf "killed %d %b" tok found )
