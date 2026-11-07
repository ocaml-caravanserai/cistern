(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

module Private = struct
  type kind = Read | Write

  type meta =
    { redacted: Cistern_core.Dialect.t -> string list
    ; kind: kind
    ; raw: Cistern_core.Raw.t option
    ; sql: Cistern_core.Dialect.t -> string list
    ; text: (string, string) result option }

  type 'a t =
    | Stmt :
        { meta: meta
        ; run: Exec.ctx -> ('r, Cistern_core.Error.t) result
        ; rows: 'r -> int
        ; finish: 'r -> 'a }
        -> 'a t

  let make meta ~run ~rows ~finish = Stmt {meta; run; rows; finish}

  let meta (Stmt s) = s.meta

  type 'a ran = {rows: int; finish: unit -> 'a}

  let run (Stmt s) ctx =
    Result.map
      (fun r -> {rows= s.rows r; finish= (fun () -> s.finish r)})
      (s.run ctx)
end

type 'a t = 'a Private.t

let map f (Private.Stmt s) =
  Private.Stmt {s with finish= (fun r -> f (s.finish r))}

let to_sql d s = (Private.meta s).sql d
