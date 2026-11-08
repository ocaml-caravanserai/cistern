(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

open Ast

let ident s = "`" ^ String.concat "``" (String.split_on_char '`' s) ^ "`"

let col (c : col) = ident c.table ^ "." ^ ident c.column

let cmp = function
  | Eq ->
      "="
  | Ne ->
      "<>"
  | Lt ->
      "<"
  | Gt ->
      ">"
  | Le ->
      "<="
  | Ge ->
      ">="

(* The largest LIMIT of the two servers: an offset with no limit. *)
let no_limit = "18446744073709551615"

let print ~lit dialect stmt =
  let b = Buffer.create 256 in
  let add = Buffer.add_string b in
  let list sep f l =
    List.iteri
      (fun i x ->
        if i > 0 then add sep ;
        f x )
      l
  in
  let rec expr = function
    | Cmp (op, c, l) ->
        add (col c) ;
        add " " ;
        add (cmp op) ;
        add " " ;
        add (lit l)
    | Null_safe_eq (c, l) ->
        add (col c) ;
        add " <=> " ;
        add (lit l)
    | Is_null c ->
        add (col c) ;
        add " IS NULL"
    | Is_not_null c ->
        add (col c) ;
        add " IS NOT NULL"
    | In (c, ls) ->
        add (col c) ;
        add " IN (" ;
        list ", " (fun l -> add (lit l)) ls ;
        add ")"
    | Like (c, l) ->
        add (col c) ;
        add " LIKE " ;
        add (lit l) ;
        add " ESCAPE '!'"
    | And es ->
        add "(" ; list " AND " expr es ; add ")"
    | Or es ->
        add "(" ; list " OR " expr es ; add ")"
    | Not (And _ as e) | Not (Or _ as e) ->
        add "NOT " ; expr e
    | Not e ->
        add "NOT (" ; expr e ; add ")"
    | Bool v ->
        add (if v then "TRUE" else "FALSE")
  in
  let where = function None -> () | Some e -> add " WHERE " ; expr e in
  let order = function
    | [] ->
        ()
    | os ->
        add " ORDER BY " ;
        list ", "
          (fun o ->
            add (col o.by) ;
            add (if o.desc then " DESC" else " ASC") )
          os
  in
  let limit l offset =
    match (l, offset) with
    | None, None ->
        ()
    | Some n, None ->
        add (" LIMIT " ^ string_of_int n)
    | l, Some m ->
        add " LIMIT " ;
        add (match l with Some n -> string_of_int n | None -> no_limit) ;
        add (" OFFSET " ^ string_of_int m)
  in
  let select s =
    add "SELECT " ;
    ( match s.columns with
    | Columns cs ->
        list ", " (fun c -> add (col c)) cs
    | One ->
        add "1" ) ;
    add " FROM " ;
    add (ident s.table) ;
    where s.where ;
    order s.order ;
    limit s.limit s.offset ;
    match s.lock with
    | No_lock ->
        ()
    | For_update ->
        add " FOR UPDATE"
    | For_share ->
        add " " ;
        add (Cistern_core.Dialect.for_share dialect)
  in
  ( match stmt with
  | Select s ->
      select s
  | Count {table; where= w} ->
      add "SELECT COUNT(*) FROM " ;
      add (ident table) ;
      where w
  | Count_derived s ->
      add "SELECT COUNT(*) FROM (" ;
      select s ;
      add ") AS " ;
      add (ident s.table)
  | Exists s ->
      add "SELECT EXISTS (" ; select s ; add ")"
  | Insert {table; columns; values} ->
      add "INSERT INTO " ;
      add (ident table) ;
      add " (" ;
      list ", " (fun c -> add (ident c)) columns ;
      add ") VALUES (" ;
      list ", " (fun l -> add (lit l)) values ;
      add ")"
  | Update {table; set; where= w; order= o; limit= l} ->
      add "UPDATE " ;
      add (ident table) ;
      add " SET " ;
      list ", "
        (fun (c, l) ->
          add (ident c) ;
          add " = " ;
          add (lit l) )
        set ;
      where w ;
      order o ;
      limit l None
  | Delete {table; where= w; order= o; limit= l} ->
      add "DELETE FROM " ;
      add (ident table) ;
      where w ;
      order o ;
      limit l None ) ;
  Buffer.contents b

let to_sql dialect stmt = print ~lit:Fun.id dialect stmt

let redacted dialect stmt = print ~lit:(fun _ -> "?") dialect stmt
