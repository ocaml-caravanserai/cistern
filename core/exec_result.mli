(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(** The result of a statement with no rows (DESIGN.md, "Core user types"). *)

(** The counts of the last OK packet. The server sends them as unsigned 64-bit
    values. A value above [Int64.max_int] is a negative [int64] with the same 64
    bits. *)
type t =
  { affected_rows: int64
        (** The rows that the statement changed. The client sets
            [CLIENT_FOUND_ROWS], thus an [UPDATE] gives the rows that it found,
            also if it did not change them. *)
  ; last_insert_id: int64
        (** The first [AUTO_INCREMENT] value of the last [INSERT], or 0. *) }
