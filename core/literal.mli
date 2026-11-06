(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(** SQL literals for the text protocol (DESIGN.md, "Literals"). No value from a
    user is ever between quotes: text and bytes are hex digits, and a timestamp
    is made from numbers. The literals do not depend on [NO_BACKSLASH_ESCAPES]
    or on another session setting. *)

val add : Buffer.t -> Value.t -> (unit, string) result
(** [add b v] appends the literal of [v] to [b]:

    - [Null]: [NULL].
    - [Int i]: the decimal digits, with a sign. [Int64.min_int] gives
      [-9223372036854775808].
    - [Float f]: the form of DESIGN.md, "Literals", for example [1.5e0], [1e-1]
      and [-0e0].
    - [Text s]: [_utf8mb4 X'<hex>'], with uppercase hex digits.
    - [Bytes s]: [X'<hex>'].
    - [Datetime t]: ['YYYY-MM-DD HH:MM:SS.ffffff'] in UTC, truncated (not
      rounded) to 1 µs.
    - [Decimal d]: the text of [d].

    [Error msg] for a NaN or an infinity, for a [Text] that is not valid UTF-8,
    for a [Datetime] before the year 1000 (the documented range of [DATETIME] is
    the years 1000 to 9999), and for a [Decimal] with more than 30 digits after
    the point (MySQL cuts a longer literal). Then [b] has no change. [msg] never
    contains the value. The caller makes
    [Invalid_value { code = None; message = msg }] from it.

    The literal has no space before or after it. The caller puts a space before
    and after each literal, so that the literal does not join the SQL text next
    to it. A literal can start with [-], and a digit always follows this [-].
    Thus [1-] before the literal [-1] gives [1--1], which the server reads as
    arithmetic: [--] starts a comment only when white space follows it. But SQL
    text that ends with [-- ] (two dashes and a space) starts a comment, and the
    server ignores the literal after it. The caller must not put a literal after
    such text. *)
