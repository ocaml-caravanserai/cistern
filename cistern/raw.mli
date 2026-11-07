(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(** Raw SQL from fragments (DESIGN.md, "Core user types"): the fragments of
    {!Cistern_core.Raw}, and the statements of [Db.exec]. *)

include module type of struct
  include Cistern_core.Raw
end

val query : t -> result Stmt.t
(** [query r] gives the first result set of [r] (no columns and no rows for a
    statement with none). It is a write: a raw statement can change data, thus a
    reply that does not come gives [Outcome_unknown]. It marks its connection
    dirty, so that the next checkout resets the session: three round trips
    ([COM_RESET_CONNECTION], the [SET] of Cistern, [COM_INIT_DB]) before the
    next statement on that connection. A raw [BEGIN] has no effect: the checkin
    aborts a connection with an open transaction ([Db.transaction] comes in
    phase 1.3). *)

val exec : t -> Cistern_core.Exec_result.t Stmt.t
(** [exec r] gives the counts of the last OK packet of [r]. As {!query}, it is a
    write and marks its connection dirty. *)
