(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

open Weft

let pool_size = 10

let checkout_timeout = Span.s 30

let exec_timeout = Span.s 30

let transaction_timeout = Span.s 30

let margin = Span.ms 50

let end_min = Span.s 5

let end_max = Span.s 30

let commit_guard = Span.s 1

let max_lifetime = Span.s 1800

let jitter = 0.15

let idle_max = Span.s 30

let idle_offset = Span.s 5

let idle_min = Span.s 1

let connect_timeout = Span.s 10

let concurrent_connects = 2

let backoff_first = Span.ms 100

let backoff_max = Span.s 5

let kill_limit = Span.s 2

let killing_age = Span.s 30

let drain_limit = Span.s 15

let close_limit = Span.s 2

let in_keys = 1000

let identifier_length = 64
