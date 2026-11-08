(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

include Cistern_core.Raw

(* A raw statement: its text is made once. The pool gives [Invalid_value] for a
   value with no literal before the checkout, from [meta.text]. *)
let statement r ~run ~rows =
  let text = to_string r in
  let meta : Stmt.Private.meta =
    { redacted= (fun _ -> [redacted r])
    ; kind= Write
    ; raw= Some r
    ; sql=
        (fun _ ->
          [ ( match text with
            | Ok s ->
                s
            | Error m ->
                "<Invalid_value: " ^ m ^ ">" ) ] )
    ; text= Some text
    ; for_update= false
    ; atomic= false
    ; multi= false }
  in
  Stmt.Private.make meta
    ~run:(fun ctx ->
      match text with
      | Ok s ->
          run ctx (sql s)
      | Error message ->
          Error (Cistern_core.Error.Invalid_value {code= None; message}) )
    ~rows ~finish:Fun.id

let query r =
  statement r
    ~run:(fun ctx s -> Exec.query ctx s)
    ~rows:(fun (x : result) -> List.length x.rows)

let exec r =
  statement r
    ~run:(fun ctx s -> Exec.exec ctx s)
    ~rows:(fun (x : Cistern_core.Exec_result.t) -> Int64.to_int x.affected_rows)
