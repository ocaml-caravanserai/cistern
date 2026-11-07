(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* Resultset: each kind of reply, a result of MySQL 8.4 (recorded), an ERR after
   the columns and after rows, 300 rows (the sequence id wraps), and each error
   with its stage. *)

open Windtrap
module R = Cistern_mysql.Resultset

let unhex h =
  String.init
    (String.length h / 2)
    (fun i -> Char.chr (int_of_string ("0x" ^ String.sub h (2 * i) 2)))

let frame seq payload =
  let n = String.length payload in
  String.init 3 (fun i -> Char.chr ((n lsr (8 * i)) land 0xFF))
  ^ String.make 1 (Char.chr (seq land 0xFF))
  ^ payload

(* Frames with the ids from [seq]. *)
let frames seq payloads =
  String.concat "" (List.mapi (fun i p -> frame (seq + i) p) payloads)

let reader input =
  Cistern_mysql.Packet.reader (Weft_flow.Flow.of_string input (Buffer.create 1))

let col name =
  "\x03def\x00\x00\x00"
  ^ String.make 1 (Char.chr (String.length name))
  ^ name ^ "\x00\x0c\x3f\x00\x14\x00\x00\x00\x08\x81\x00\x00\x00\x00"

let terminator = "\xfe\x00\x00\x02\x00\x00\x00"

let show = function
  | Ok (reply, next) ->
      let r =
        match reply with
        | R.Ok_packet o ->
            Printf.sprintf "ok %Ld" o.affected_rows
        | Err_packet e ->
            Printf.sprintf "err %d" e.code
        | Local_infile f ->
            "infile " ^ f
        | Rows {columns; rows; ok} ->
            Printf.sprintf "rows [%s] %s status %d"
              (String.concat ","
                 (Array.to_list
                    (Array.map
                       (fun (c : Cistern_mysql.Message.column) -> c.name)
                       columns ) ) )
              (String.concat ";"
                 (List.map
                    (fun row ->
                      String.concat ","
                        (Array.to_list
                           (Array.map (Option.value ~default:"NULL") row) ) )
                    rows ) )
              ok.status
      in
      Printf.sprintf "%s next %d" r next
  | Error {R.in_result; cause} ->
      Printf.sprintf "error %b %s" in_result
        ( match cause with
        | Malformed m ->
            m
        | Packet (Sequence {expected; got}) ->
            Printf.sprintf "sequence %d %d" expected got
        | Packet (Unsolicited _) ->
            "unsolicited"
        | Packet Too_long ->
            "too long" )

let read ?(seq = 1) input = show (R.read (reader input) ~seq)

(* The reply of MySQL 8.4.11 to SELECT 1 AS a, NULL AS b, 'x' AS c, CAST(2.5 AS
   DECIMAL(3,1)) AS d. *)
let mysql =
  frames 1
    (List.map unhex
       [ "04"
       ; "036465660000000161000c3f0002000000088100000000"
       ; "036465660000000162000c3f0000000000068000000000"
       ; "036465660000000163000c2d0004000000fd01001f0000"
       ; "036465660000000164000c3f0005000000f68100010000"
       ; "0131fb017803322e35"
       ; "fe000002000000" ] )

let tests =
  group "Resultset"
    [ cases "replies" ~id:fst
        [ (frame 1 "\x00\x03\x00\x02\x00\x00\x00", "ok 3 next 2")
        ; (frame 1 "\xff\x26\x04#23000dup", "err 1062 next 2")
        ; (frame 1 "\xfb/etc/passwd", "infile /etc/passwd next 2")
        ; (mysql, "rows [a,b,c,d] 1,NULL,x,2.5 status 2 next 8")
        ; (frames 1 ["\x01"; col "a"; terminator], "rows [a]  status 2 next 4")
        ; ( frames 1 ["\x01"; col "a"; "\x011"; "\x012"; terminator]
          , "rows [a] 1;2 status 2 next 6" )
        ; ( frames 1 ["\x01"; col "a"; "\xff\xa2\x06#22003out of range"]
          , "err 1698 next 4" )
        ; ( frames 1
              ["\x01"; col "a"; "\x011"; "\x012"; "\xff\xc8\x0b#HY000timeout"]
          , "err 3016 next 6" )
        ; ( frames 1 ["\x02"; col "a"; col "b"; "\x011\x012"; terminator]
          , "rows [a,b] 1,2 status 2 next 6" ) ]
        (fun (input, expected) -> equal string expected (read input))
    ; test "300 rows: the sequence id wraps" (fun () ->
          let rows = List.init 300 (fun _ -> "\x01x") in
          let input = frames 1 ((["\x01"; col "a"] @ rows) @ [terminator]) in
          match R.read (reader input) ~seq:1 with
          | Ok (Rows {rows; _}, next) ->
              equal int 300 (List.length rows) ;
              equal int ((1 + 303) land 0xFF) next
          | _ ->
              fail "no rows" )
    ; test "the first packet starts at seq" (fun () ->
          equal string "ok 0 next 1"
            (read ~seq:0 (frame 0 "\x00\x00\x00\x00\x00\x00\x00")) ;
          equal string "ok 0 next 0"
            (read ~seq:255 (frame 255 "\x00\x00\x00\x00\x00\x00\x00")) )
    ; cases "errors" ~id:fst
        [ (frame 2 "\x00", "error false sequence 1 2")
        ; (frame 1 "", "error false the column count is not valid")
        ; ( frames 1 ["\x01"; col "a"; ""]
          , "error true row: the packet is too short" )
        ; (frame 0 "\xff\xbf\x0f#HY000idle", "error false unsolicited")
        ; ("\xff\xff\xff\x01", "error false too long")
        ; (frame 1 "\x00\x01", "error false OK: the packet is too short")
        ; (frame 1 "\xff\x01", "error false ERR: the packet is too short")
        ; (frame 1 "\x01\x00", "error false the column count is not valid")
        ; (frame 1 "\xfc\x00\x00", "error false the column count is not valid")
        ; ( frame 1 "\xfd\x00\x00\x01"
          , "error false the column count is not valid" )
        ; (frame 1 "\xfc\x01", "error false the column count is not valid")
        ; ( frame 1 "\xfe\x00\x00\x00\x00\x00\x00\x00\x80"
          , "error false the column count is not valid" )
        ; ( frames 1 ["\x01"; "\x00"]
          , "error true column definition: the packet is too short" )
        ; ( frames 1 ["\x01"; col "a"; "\x01x\x01y"]
          , "error true row: the row has more bytes than its values" )
        ; ( frames 1 ["\x01"; col "a"; "\xfe\x00"]
          , "error true terminator: the packet is too short" )
        ; ( frames 1 ["\x01"; col "a"; "\xff\x01"]
          , "error true ERR: the packet is too short" )
        ; (frames 1 ["\x01"] ^ frame 9 (col "a"), "error true sequence 2 9")
        ; ( frames 1 ["\x01"; col "a"] ^ frame 9 "\x01x"
          , "error true sequence 3 9" )
        ; ( frames 1 ["\x01"; col "a"] ^ frame 0 "\xff\xbf\x0f#HY000idle"
          , "error true sequence 3 0" )
        ; ( frames 1 ["\x01"; col "a"] ^ frame 3 "\x00\x00\x00\x02\x00\x00\x00"
          , "error true row: the row has more bytes than its values" ) ]
        (fun (input, expected) -> equal string expected (read input))
    ; test "the largest column count" (fun () ->
          equal string "error false the column count is not valid"
            (read (frame 1 "\xfd\x00\x00\x01")) ;
          (* 65535 columns pass the count, then the stream ends. *)
          raises End_of_file (fun () ->
              R.read (reader (frame 1 "\xfc\xff\xff")) ~seq:1 ) ) ]
