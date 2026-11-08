(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(** Raw SQL from fragments (DESIGN.md, "Core user types" and "Literals").

    A raw statement joins fixed SQL text of the program and literals of values.
    It has no placeholders, thus it needs no lexer. Phase 1.1 runs it with
    [Cistern_mysql.Connection.query] and [exec]. Phase 1.2 adds [Raw.query] and
    [Raw.exec] with [Stmt.t]. *)

type t

val sql : string -> t
(** [sql s] is the SQL text [s] as it is. [s] must be fixed text of the program,
    never text from a user. *)

val v : Value.t -> t
(** [v x] is the literal of [x] ({!Cistern_core.Literal.add}), with a space
    before and after it. Thus [sql "LIMIT" ++ v (Int 5L)] gives ["LIMIT 5 "]. *)

val ( ++ ) : t -> t -> t
(** [a ++ b] is [a], then [b]. It takes constant time. *)

val to_string : t -> (string, string) result
(** [to_string r] is the SQL text of [r]. It is [Error msg] if a value has no
    literal (see {!Cistern_core.Literal.add}). The first such value gives [msg],
    which never contains the value. The caller makes
    [Invalid_value {code = None; message = msg}] from it. *)

val redacted : t -> string
(** [redacted r] is the text of [r] with [" ? "] in place of each literal: the
    SQL of an event, which holds no value (DESIGN.md, "Shared state and the
    query hook"). It never fails. A literal that a program writes into a {!sql}
    fragment is not redacted: [sql] takes only program text. *)

val first_word : t -> string option
(** [first_word r] is the first word of the text of [r]: after leading white
    space (space, tab, CR, LF), the ASCII letters up to the first other
    character or the first value, as written. It is [None] if a value, another
    character or the end comes before the first letter: for example for
    [v x ++ sql "SELECT"], ["(SELECT 1)"] and ["/* c */ COMMIT"]. Other white
    space bytes (['\x0b'], ['\x0c']) are other characters: ["\x0bCOMMIT"] gives
    [None]. The [Tx] of phase 1.3 refuses some first words, and [None]
    (DESIGN.md, "Transactions"). *)

(** The result of a raw query: the names of the columns, and the rows in the
    order of the server. Each row has one value for each column. *)
type result = {columns: string array; rows: Value.t array list}
