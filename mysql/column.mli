(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(** The values of a text result set as [Value.t] (DESIGN.md, "Commands and
    packets"). The type code, the [UNSIGNED] flag (0x20) and the character set
    of the column definition choose the value:

    - SQL [NULL] (in any column) and the type 6 ([NULL]) give [Null].
    - 1, 2, 3, 9, 13 ([YEAR]) and a signed 8 ([BIGINT]) give [Int]. [TINYINT(1)]
      is an [Int].
    - An unsigned 8 gives [Int] up to [Int64.max_int], else [Decimal].
    - 4 ([FLOAT]) and 5 ([DOUBLE]) give [Float]. The server sends a [FLOAT] with
      about 6 significant digits: use [DOUBLE] for exact values.
    - 0 and 246 ([DECIMAL]) give [Decimal].
    - 7 ([TIMESTAMP]) and 12 ([DATETIME]) give [Datetime]. A zero date fails.
    - 10 ([DATE]), 11 ([TIME]) and 14 give [Text].
    - 16 ([BIT]) and 255 ([GEOMETRY]) give [Bytes].
    - 245 ([JSON]) gives [Text], for each character set.
    - Each other type: [Bytes] with the character set 63 (binary), else [Text].
      Thus an unknown type never fails because of its code.

    [Text] must be valid UTF-8. *)

val row :
     Message.column array
  -> string option array
  -> (Cistern_core.Value.t array, string) result
(** [row columns values] decodes one row. [Error msg] names the column (from 0)
    and the expected type, and never the text of the value. *)
