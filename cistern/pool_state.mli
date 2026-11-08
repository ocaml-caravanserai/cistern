(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(** The pure state of the pool (DESIGN.md, "Pool"). No operation does an effect
    or I/O, and no operation raises. The weft shell of the pool runs each
    operation under its mutex, and then does the effects that the result names.

    A time is a [Weft.Span.t] from the origin of the pool: the shell reads
    [Weft.Time.now Mono] and gives the span since the time when the pool
    started. No operation assumes that the time grows from one operation to the
    next: two fibers can read the clock in one order and run their operations in
    the other order.

    The slots: the idle entries (a stack, the last checkin first), the entries
    in use (by a fiber, or given to a waiter), the running connects, and the
    entries that wait for a polite close by the opener ([closing], in two parts:
    to close, and in close), and the [Killing] slots. Each keeps its slot. Thus
    [idle + in_use + opening + closing + killing <= size] always.

    A [Killing] slot is the slot of a connection that the shell aborted while
    its statement can still run on the server (DESIGN.md, "Cancel"). The slot
    stays until the killer sends [KILL] ({!killed}), or until its age limit of
    30 s. Each operation first removes the [Killing] slots whose age limit is at
    or before its [now]. Thus each operation, {!val-next_timer} and [wake] see
    such a slot as free, also when no operation ran since its age limit. *)

type time = Weft.Span.t

(** An open connection of the pool. The fields are for reading: only the
    operations make entries. *)
type 'c entry = private
  { id: int  (** Unique in the state. *)
  ; conn: 'c
  ; opened: time
  ; expires: time option
        (** The end of its life: the open time plus [max_lifetime] with the
            jitter. [None] if that time is too large for a span. *)
  ; idle_limit: time
        (** The smaller of 30 s and [@@wait_timeout] − 5 s, at least 1 s. *)
  ; last_used: time  (** The time of its last checkin, or of its open. *)
  ; dirty: bool
        (** A raw statement ran on it: the next checkout resets the session. *)
  }

(** The state. ['w] is the value of a waiter (the shell keeps its [Ivar] there).
*)
type ('c, 'w) t

val create : size:int -> max_lifetime:Weft.Span.t -> ('c, 'w) t
(** [create ~size ~max_lifetime] is an empty open pool.

    It raises [Invalid_argument] if [size] is not 1 to 1000, or [max_lifetime]
    is not positive. *)

(** {1 The operations}

    Each operation takes the time [now] and gives the new state, its result, and
    [wake]: true if the opener has work now (a connect to start, or an entry to
    close), or a timer to arm ({!val-next_timer} gives a time). The opener reads
    its timer only in {!val-tick}, thus the shell then rings the doorbell of the
    opener. *)

type waiter_id

val waiter_id : waiter_id -> int

(** The token of a [Killing] slot. Each {!killing} gives a new token. *)
type token

val token_id : token -> int

type ('c, 'w) checkout =
  | Conn of 'c entry  (** An idle entry within its life and its idle limit. *)
  | Wait of waiter_id  (** The waiter went to the tail of the queue. *)
  | Closed  (** The pool is closed. *)

val checkout : ('c, 'w) t -> time -> 'w -> ('c, 'w) t * ('c, 'w) checkout * bool
(** [checkout s now w]: all the idle entries that passed their life or their
    idle limit move to the entries to close (the opener closes them), also those
    under a fresh entry of the stack. Then the fresh entry at the top of the
    stack goes out, or the waiter goes to the queue. *)

val cancel_wait :
     ('c, 'w) t
  -> time
  -> waiter_id
  -> ('c, 'w) t * bool * Cistern_core.Error.t option * bool
(** [cancel_wait s now id] removes the waiter. It gives [true] if the waiter was
    in the queue, the stored connect error, and [wake]: the removal of an
    expired [Killing] slot can let another waiter get a connect. *)

type ('c, 'w) handed =
  | Give of 'w * 'c entry  (** To the oldest waiter: the entry stays in use. *)
  | Idle  (** On the idle stack. *)
  | Closing  (** It passed its life: to the entries to close. *)
  | Freed  (** Not reusable: the shell aborted it, and its slot is free. *)
  | Abort of 'c  (** The pool is closed: the shell aborts it now. *)

val checkin :
     ('c, 'w) t
  -> time
  -> 'c entry
  -> reusable:bool
  -> dirty:bool
  -> ('c, 'w) t * ('c, 'w) handed * bool
(** [checkin s now e ~reusable ~dirty] gives back an entry in use. The shell
    aborts a connection that is not reusable before the operation, so that the
    slot is free only when the socket is closed. [dirty] is the new dirty mark.
    An entry that the shell gave to a waiter that abandoned comes back with
    [checkin] too. *)

type ('c, 'w) tick =
  { connects: int  (** The connects to start now. They count in [opening]. *)
  ; to_close: 'c entry list
        (** The entries for a polite close. They move to "in close", and keep
            their slots until {!closed_done}. *)
  ; next_timer: time option  (** {!val-next_timer} of the new state. *) }

val tick : ('c, 'w) t -> time -> ('c, 'w) t * ('c, 'w) tick
(** [tick s now] is the work of the opener. *)

val next_timer : ('c, 'w) t -> time -> time option
(** [next_timer s now] is the earliest [t] later than [now] at which [tick],
    with no other operation, starts a connect. The candidates are the end of the
    backoff of the connects and the age limits of the [Killing] slots. Thus an
    age limit before the end of the backoff gives the end of the backoff. [None]
    if no candidate starts a connect, or if [tick] at [now] starts a connect
    (that tick gives the timer of its new state). The opener waits for this time
    or for its doorbell. *)

type ('c, 'w) opened =
  | Opened_give of 'w * 'c entry  (** To the oldest waiter. *)
  | Opened_idle  (** On the idle stack. *)
  | Opened_close of 'c  (** The pool is closed: the shell aborts it. *)

val opened :
     ('c, 'w) t
  -> time
  -> 'c
  -> wait_timeout:int
  -> dialect:Cistern_core.Dialect.t
  -> u:float
  -> ('c, 'w) t * ('c, 'w) opened * bool
(** [opened s now c ~wait_timeout ~dialect ~u]: a connect succeeded. It clears
    the stored error and the backoff. [wait_timeout] is [@@wait_timeout] in
    seconds. [u] is the jitter of the life, from −0.15 to 0.15. The shell draws
    it under its mutex. A value outside the range is cut to the range, and NaN
    gives 0 (the middle of the range). *)

val open_failed :
  ('c, 'w) t -> time -> Cistern_core.Error.t -> ('c, 'w) t * bool
(** [open_failed s now e]: a connect failed. The error is stored, and no connect
    starts before the end of the backoff: 100 ms after the first failure, then
    200 ms, 400 ms and so on, at most 5 s. *)

val open_aborted : ('c, 'w) t -> time -> ('c, 'w) t * bool
(** [open_aborted s now]: a connect was cancelled. No error, no backoff. *)

val closed_done : ('c, 'w) t -> time -> int -> ('c, 'w) t * bool
(** [closed_done s now id]: the opener closed the entry [id] that {!val-tick}
    gave to close. Its slot is free. Another [id] changes nothing. *)

val close_all : ('c, 'w) t -> time -> ('c, 'w) t * 'w list * 'c entry list
(** [close_all s now] closes the pool. The [Killing] slots stay. It gives the
    waiters (the shell fails them with [Connection_lost]) and the idle entries,
    the entries to close and the entries in close (the shell closes them; a
    second close of an entry in close does nothing). The entries in use and the
    running connects come back later: {!checkin} and {!val-opened} then give
    [Abort] and [Opened_close]. *)

val killing : ('c, 'w) t -> time -> 'c entry -> ('c, 'w) t * token * bool
(** [killing s now e]: the shell aborted the entry [e] in use, and its statement
    can still run on the server. The slot stays as a [Killing] slot with a new
    token, until {!killed} of the token or until [now + 30 s] (the age limit).
    This also holds on a closed pool: the drain frees the slot. *)

val killed : ('c, 'w) t -> time -> token -> ('c, 'w) t * bool * bool
(** [killed s now tok] frees the [Killing] slot of [tok]. It gives [true] if the
    slot was there, and [wake]. A token whose slot passed its age limit, or a
    token that {!killed} already freed, changes nothing. *)

val idle_limit : int -> Weft.Span.t
(** [idle_limit w] is the idle limit of a connection whose [@@wait_timeout] is
    [w] seconds: the smaller of 30 s and [w] − 5 s, at least 1 s. *)

(** {1 Reading} *)

val size : ('c, 'w) t -> int

val is_closed : ('c, 'w) t -> bool

val idle : ('c, 'w) t -> 'c entry list
(** The idle stack, the next checkout first. *)

val in_use : ('c, 'w) t -> int

val opening : ('c, 'w) t -> int

val to_close : ('c, 'w) t -> 'c entry list
(** The entries to close that no {!val-tick} took, the oldest first. *)

val closing : ('c, 'w) t -> int
(** The entries to close and in close. *)

val waiting : ('c, 'w) t -> int

val error : ('c, 'w) t -> Cistern_core.Error.t option

val last_dialect : ('c, 'w) t -> Cistern_core.Dialect.t option
(** The dialect of the last connection opened. *)

val backoff_end : ('c, 'w) t -> time option

val killing_slots : ('c, 'w) t -> (token * time) list
(** The [Killing] slots with their age limits, the oldest first. A slot past its
    age limit stays in this list until the next operation. *)

val free : ('c, 'w) t -> int
(** [size − (idle + in_use + opening + closing + killing)], with each slot of
    {!killing_slots}. *)

val demand : ('c, 'w) t -> int
(** The waiters that no running connect serves: [waiting − opening], at least 0.
*)
