(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* gen.exe README.md EXAMPLE.ml...: the expected README.md on stdout (the README
   rule, test/readme/dune). It exits with 1 and a message on stderr for a defect
   of the README or of the regions. *)

let die fmt =
  Printf.ksprintf
    (fun m ->
      prerr_endline ("readme: " ^ m) ;
      exit 1 )
    fmt

let lines path = In_channel.with_open_bin path In_channel.input_lines

let begin_prefix = "(* README begin: "

let end_line = "(* README end *)"

(* The common indentation of the lines that are not empty. *)
let dedent l =
  let indent s =
    let n = String.length s in
    let rec go i = if i < n && s.[i] = ' ' then go (i + 1) else i in
    go 0
  in
  let m =
    List.fold_left
      (fun m s -> if String.trim s = "" then m else min m (indent s))
      max_int l
  in
  List.map
    (fun s ->
      if String.trim s = "" then "" else String.sub s m (String.length s - m) )
    l

(* The regions of one file: (name, lines). *)
let regions path =
  let rec go acc = function
    | [] ->
        List.rev acc
    | line :: rest -> (
        let t = String.trim line in
        if t = end_line then die "%s: %s with no begin" path end_line
        else
          match String.starts_with ~prefix:begin_prefix t with
          | false ->
              go acc rest
          | true ->
              let name =
                String.sub t
                  (String.length begin_prefix)
                  (String.length t - String.length begin_prefix)
              in
              if not (String.ends_with ~suffix:" *)" name) then
                die "%s: a bad begin line: %s" path t ;
              let name = String.sub name 0 (String.length name - 3) in
              let rec body b = function
                | [] ->
                    die "%s: the region %s has no end" path name
                | l :: r when String.trim l = end_line ->
                    (List.rev b, r)
                | l :: r
                  when String.starts_with ~prefix:begin_prefix (String.trim l)
                  ->
                    ignore r ;
                    die "%s: a begin in the region %s" path name
                | l :: r ->
                    body (l :: b) r
              in
              let b, rest = body [] rest in
              go ((name, dedent b) :: acc) rest )
  in
  go [] (lines path)

let marker_prefix = "<!-- example: "

let no_check = "<!-- no-check -->"

(* The languages of a fence that need no check. A fence with another language is
   refused, so that an unknown form of OCaml cannot escape the rule. *)
let free = [""; "sh"; "nix"; "dune"; "text"]

let ocaml_names = ["ocaml"; "ml"]

let leading s =
  let n = String.length s in
  let rec go i = if i < n && s.[i] = ' ' then go (i + 1) else i in
  go 0

(* [fence line]: [Some (indent, char, length, language)] for an opening or a
   closing fence: three or more backquotes or tildes after spaces. The language
   is the first word of the rest, in lowercase. *)
let fence line =
  let n = String.length line in
  (* The prefix of a container: spaces, the [>] of a blockquote, and the markers
     of a list item ([-], [*], [+], [1.], [1)]) before a space. The index of a
     fence is the column after this prefix. *)
  let rec prefix i =
    if i >= n then i
    else
      match line.[i] with
      | ' ' | '>' ->
          prefix (i + 1)
      | ('-' | '*' | '+') when i + 1 < n && line.[i + 1] = ' ' ->
          prefix (i + 2)
      | '0' .. '9' ->
          let j = ref i in
          while !j < n && line.[!j] >= '0' && line.[!j] <= '9' do
            incr j
          done ;
          if
            !j + 1 < n
            && (line.[!j] = '.' || line.[!j] = ')')
            && line.[!j + 1] = ' '
          then prefix (!j + 2)
          else i
      | _ ->
          i
  in
  let i = prefix 0 in
  if i < n && (line.[i] = '`' || line.[i] = '~') then (
    let c = line.[i] in
    let j = ref i in
    while !j < n && line.[!j] = c do
      incr j
    done ;
    let len = !j - i in
    if len < 3 then None
    else
      let rest = String.trim (String.sub line !j (n - !j)) in
      let word =
        match String.index_opt rest ' ' with
        | Some k ->
            String.sub rest 0 k
        | None ->
            rest
      in
      Some (i, c, len, String.lowercase_ascii word) )
  else None

let is_close (_, c, len, _) line =
  match fence line with
  | Some (_, c', len', "") ->
      c = c' && len' >= len
  | _ ->
      false

let lower = String.lowercase_ascii

let () =
  match Array.to_list Sys.argv with
  | _ :: readme :: examples ->
      let all = List.concat_map regions examples in
      let tbl = Hashtbl.create 16 in
      List.iter
        (fun (n, b) ->
          if Hashtbl.mem tbl n then die "the region %s is two times" n ;
          Hashtbl.add tbl n b )
        all ;
      let used = Hashtbl.create 16 in
      let out = Buffer.create 4096 in
      let put s = Buffer.add_string out s ; Buffer.add_char out '\n' in
      (* [prev]: the last line that is not empty before the line. *)
      let rec go prev lineno = function
        | [] ->
            ()
        | line :: rest when fence line <> None ->
            let ((indent, c, len, lang) as f) = Option.get (fence line) in
            (* The text of the block, to its closing fence. *)
            let rec block b n = function
              | [] ->
                  die "README.md:%d: a block with no end" lineno
              | l :: r when is_close f l ->
                  (List.rev b, l, n + 1, r)
              | l :: r ->
                  block (l :: b) (n + 1) r
            in
            let body, close, n, rest = block [] (lineno + 1) rest in
            let p = String.trim prev in
            let marker =
              if
                String.starts_with ~prefix:marker_prefix p
                && String.ends_with ~suffix:" -->" p
              then
                Some
                  (String.sub p
                     (String.length marker_prefix)
                     (String.length p - String.length marker_prefix - 4) )
              else None
            in
            put line ;
            ( match marker with
            | Some name -> (
                if not (indent = 0 && c = '`' && len = 3 && line = "```ocaml")
                then
                  die
                    "README.md:%d: a marked block must open with ```ocaml at \
                     column 0"
                    lineno ;
                match Hashtbl.find_opt tbl name with
                | None ->
                    die "README.md:%d: no region %s in the examples" lineno name
                | Some b ->
                    if Hashtbl.mem used name then
                      die "README.md:%d: the region %s is used two times" lineno
                        name ;
                    Hashtbl.add used name () ;
                    List.iter put b )
            | None ->
                if p <> no_check then (
                  if indent <> 0 then
                    die
                      "README.md:%d: a fence that is not at column 0 (in a \
                       list item or a blockquote): put it at column 0, or %s"
                      lineno no_check ;
                  if List.mem lang ocaml_names then
                    die
                      "README.md:%d: an OCaml block with no marker (add <!-- \
                       example: <name> --> or %s)"
                      lineno no_check ;
                  if not (List.mem lang free) then
                    die
                      "README.md:%d: a block in the language %S: use one of \
                       sh, nix, dune or text, or %s"
                      lineno lang no_check ) ;
                List.iter put body ) ;
            put close ; go close n rest
        | line :: _
          when String.length line > 0
               && (line.[0] = '\t' || leading line >= 4)
               && String.trim line <> "" ->
            die
              "README.md:%d: an indented code block: use a fence with a marker"
              lineno
        | line :: _
          when let l = lower line in
               let rec has i =
                 i + 4 <= String.length l
                 && (String.sub l i 4 = "<pre" || has (i + 1))
               in
               has 0 ->
            die "README.md:%d: a <pre> block: use a fence with a marker" lineno
        | line :: rest ->
            put line ;
            go (if String.trim line = "" then prev else line) (lineno + 1) rest
      in
      let text = lines readme in
      if List.exists (String.ends_with ~suffix:"\r") text then
        die "README.md has CRLF line ends: use LF" ;
      go "" 1 text ;
      Hashtbl.iter
        (fun n _ ->
          if not (Hashtbl.mem used n) then
            die "the region %s is in no block of README.md" n )
        tbl ;
      print_string (Buffer.contents out)
  | _ ->
      die "usage: gen.exe README.md EXAMPLE.ml..."
