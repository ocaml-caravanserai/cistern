(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* Connection.query, exec, init_db, reset and status against the fake server:
   the values of each column type, the results of more than one part, the size
   check, and the outcome rules (DESIGN.md, "Order of the error rules"). The
   plan of phase 1.1, sections 3.5 to 3.8 and 4.6. *)

open Windtrap
open Weft
open Cistern_core
module C = Cistern_mysql.Config
module Conn = Cistern_mysql.Connection
module E = Cistern_core.Error
module F = Fake_server
module T = Types_fixture

let tcp () =
  Result.get_ok
    (C.make ~tls:C.No_tls
       ~address:(C.Tcp {host= "10.0.0.1"; port= 3306})
       ~user:"u" ~password:"pw" ~database:"db" () )

let error_text = function
  | E.Connect m ->
      "Connect " ^ m
  | E.Connection_lost ->
      "Connection_lost"
  | E.Decode m ->
      "Decode " ^ m
  | E.Invalid_value {message; _} as e ->
      Format.asprintf "%a: %s" E.pp_summary e message
  | e ->
      Format.asprintf "%a" E.pp_summary e

let value_text (v : Value.t) =
  match v with
  | Null ->
      "Null"
  | Int i ->
      Printf.sprintf "Int %Ld" i
  | Float f ->
      Printf.sprintf "Float %h" f
  | Text s ->
      Printf.sprintf "Text %S" s
  | Bytes s ->
      Printf.sprintf "Bytes %S" s
  | Datetime t ->
      "Datetime " ^ Ptime.to_rfc3339 ~frac_s:6 t
  | Decimal d ->
      "Decimal " ^ Decimal.to_string d

let result_text = function
  | Ok (r : Raw.result) ->
      String.concat "," (Array.to_list r.columns)
      ^ " | "
      ^ String.concat " / "
          (List.map
             (fun row ->
               String.concat ", " (Array.to_list (Array.map value_text row)) )
             r.rows )
  | Error e ->
      error_text e

let exec_text = function
  | Ok (r : Exec_result.t) ->
      Printf.sprintf "%Ld %Ld" r.affected_rows r.last_insert_id
  | Error e ->
      error_text e

let unit_text = function Ok () -> "ok" | Error e -> error_text e

let sql s = Raw.sql s

let q s = "\x03" ^ s

(* A connection after the handshake of [F.handshake ?values], then [steps] of
   the server and [client] on the connection. The client then closes the
   connection, and has no open fd. *)
let with_connection ?values steps client =
  F.run_raw
    (F.handshake ?values () @ steps)
    (fun sim ->
      match Conn.connect (tcp ()) with
      | Error e ->
          "connect: " ^ error_text e
      | Ok c ->
          let r = client c in
          Conn.close c ;
          equal ~msg:"the fds of the client" int 0 (F.client_fds sim) ;
          r )

(* The packets that the client sent after the handshake (the three packets of
   the handshake response and the two round trips). *)
let after_handshake (result, received, _) =
  (result, List.filteri (fun i _ -> i >= 3) received)

let packets = list (pair int string)

let quit = (0, "\x01")

let ping = (0, "\x0e")

let rs ?(terminator = F.terminator) columns rows =
  String.make 1 (Char.chr (List.length columns))
  :: (columns @ rows @ [terminator])

let more_results = 0x0a

let types_expected ~zero =
  "id,ti,u,d,b,y,dt,tm,dtt,ts,j,g,en,st,f,vb,bl,tx,ch,sm,mi,it,bi,vc,db,n,q,z,jo,s \
   | Int 1, Int 1, Decimal 18446744073709551615, Decimal \
   -0.500000000000000000000000000000, Bytes \"\\002\\005\", Int 2024, Text \
   \"2024-02-29\", Text \"-838:59:59.000000\", Datetime \
   2024-01-02T03:04:05.500000-00:00, Datetime \
   2024-01-02T03:04:05.000000-00:00, Text \"{\\\"a\\\": [1, 2]}\", Bytes \
   \"\\000\\000\\000\\000\\001\\001\\000\\000\\000\\000\\000\\000\\000\\000\\000\\240?\\000\\000\\000\\000\\000\\000\\000@\", \
   Text \"b\", Text \"x,y\", Float 0x1.cp+1, Bytes \"\\000\\255\", Bytes \
   \"\\001\", Text \"h\\195\\169\", Text \"ab\", Int -32768, Int 8388607, Int \
   -2147483648, Int -9223372036854775808, Text \"v\\195\\169\", Float \
   -0x1.4f8b588e368f1p-19, Null, Decimal 0.3333, " ^ zero
  ^ ", Text \"{\\\"k\\\": 1}\", Int -1"

(* The definition of an INT column (type 3) with the binary set. *)
let int_column name =
  "\x03def\x00\x00\x00" ^ F.lstr name
  ^ "\x00\x0c\x3f\x00\x0b\x00\x00\x00\x03\x00\x00\x00\x00\x00"

let tests =
  group "Raw on one connection"
    [ cases "the column types of the pinned servers"
        ~id:(fun (n, _, _) -> n)
        [ ("MySQL 8.4", T.mysql84, "Float -0x0p+0")
        ; ("MariaDB 11.8", T.mariadb118, "Float 0x0p+0") ]
        (fun (_, (fx : T.t), zero) ->
          let result, received =
            after_handshake
              (with_connection
                 [ F.Recv
                 ; F.Sends (1, (fx.count :: fx.columns) @ [fx.row; F.terminator])
                 ]
                 (fun c -> result_text (Conn.query c (sql "SELECT x"))) )
          in
          equal string (types_expected ~zero) result ;
          equal packets [(0, q "SELECT x"); quit] received )
    ; test "a statement with no result set, and status" (fun () ->
          let result, received =
            after_handshake
              (with_connection
                 [ F.Recv
                 ; F.Send (1, F.ok_with ~status:3 ())
                 ; F.Recv
                 ; F.Send (1, F.ok_with ~affected:3L ~insert:7L ~status:2 ()) ]
                 (fun c ->
                   (* The status of the OK of round trip 2. *)
                   let s0 = Conn.status c in
                   let a = result_text (Conn.query c (sql "BEGIN")) in
                   let s1 = Conn.status c in
                   let b = exec_text (Conn.exec c (sql "INSERT")) in
                   Printf.sprintf "%d; %s; %d; %s; %d" s0 a s1 b (Conn.status c) ) )
          in
          equal string "2;  | ; 3; 3 7; 2" result ;
          equal packets [(0, q "BEGIN"); (0, q "INSERT"); quit] received )
    ; test "counts above Int64.max_int" (fun () ->
          let result, _ =
            after_handshake
              (with_connection
                 [ F.Recv
                 ; F.Send (1, F.ok_with ~affected:(-1L) ~insert:Int64.min_int ())
                 ]
                 (fun c -> exec_text (Conn.exec c (sql "UPDATE"))) )
          in
          equal string "-1 -9223372036854775808" result )
    ; test "more than one result" (fun () ->
          (* A CALL: two result sets with SERVER_MORE_RESULTS_EXISTS, then the
             OK of the CALL. *)
          let script =
            [ F.Recv
            ; F.Sends
                ( 1
                , rs
                    ~terminator:
                      (F.ok_with ~header:'\xfe' ~status:more_results ())
                    [int_column "a"]
                    [F.row [Some "1"]; F.row [Some "2"]]
                  @ rs
                      ~terminator:
                        (F.ok_with ~header:'\xfe' ~status:more_results ())
                      [int_column "b"]
                      [F.row [Some "x"]]
                  @ [F.ok_with ~affected:5L ~insert:6L ()] ) ]
          in
          let query, _ =
            after_handshake
              (with_connection
                 (script @ [F.Recv; F.Send (1, F.ok ())])
                 (fun c ->
                   let r = result_text (Conn.query c (sql "CALL p()")) in
                   r ^ "; " ^ unit_text (Conn.ping c) ) )
          in
          (* The value "x" of the second result set is not decoded. *)
          equal string "a | Int 1 / Int 2; ok" query ;
          let exec, _ =
            after_handshake
              (with_connection script (fun c ->
                   exec_text (Conn.exec c (sql "CALL p()")) ) )
          in
          equal string "5 6" exec ;
          (* An OK first, then a result set: query gives the result set. *)
          let first_ok, _ =
            after_handshake
              (with_connection
                 [ F.Recv
                 ; F.Sends
                     ( 1
                     , F.ok_with ~status:more_results ()
                       :: rs [int_column "c"] [F.row [Some "3"]] ) ]
                 (fun c -> result_text (Conn.query c (sql "CALL q()"))) )
          in
          equal string "c | Int 3" first_ok )
    ; cases "an error in a part of the reply"
        ~id:(fun (n, _, _) -> n)
        [ ( "after the rows"
          , ["\x01"; int_column "a"; F.row [Some "1"]; F.err 1317 "interrupted"]
          , "Driver 1317 28000; ok" )
        ; ( "after the columns, with no row"
          , ["\x01"; int_column "a"; F.err 1690 "out of range"]
          , "Invalid_value 1690: out of range; ok" )
        ; ( "in a later result"
          , rs
              ~terminator:(F.ok_with ~header:'\xfe' ~status:more_results ())
              [int_column "a"]
              [F.row [Some "1"]]
            @ [F.err 1146 "no table"]
          , "Driver 1146 28000; ok" ) ]
        (fun (_, packets, expected) ->
          let result, _ =
            after_handshake
              (with_connection
                 [F.Recv; F.Sends (1, packets); F.Recv; F.Send (1, F.ok ())]
                 (fun c ->
                   let r = result_text (Conn.query c (sql "SELECT")) in
                   r ^ "; " ^ unit_text (Conn.ping c) ) )
          in
          equal string expected result )
    ; test "a value that does not decode: the client reads the whole reply"
        (fun () ->
          let result, _ =
            after_handshake
              (with_connection
                 [ F.Recv
                 ; F.Sends
                     ( 1
                     , rs
                         ~terminator:
                           (F.ok_with ~header:'\xfe' ~status:more_results ())
                         [int_column "a"]
                         [F.row [Some "1"]; F.row [Some "x"]; F.row [Some "3"]]
                       @ [F.ok ()] )
                 ; F.Recv
                 ; F.Send (1, F.ok ()) ]
                 (fun c ->
                   let r = result_text (Conn.query c (sql "SELECT")) in
                   r ^ "; " ^ unit_text (Conn.ping c) ) )
          in
          equal string "Decode column 0: expected an integer; ok" result )
    ; cases "replies that break the connection"
        ~id:(fun (n, _, _, _) -> n)
        [ ( "LOCAL INFILE"
          , [F.Send (1, "\xfbfile")]
          , "Decode LOCAL INFILE request"
          , "Decode LOCAL INFILE request" )
        ; ( "a bad packet after the column count"
          , [F.Send (1, "\x01"); F.Send (3, int_column "a")]
          , "Outcome_unknown statement"
          , "Outcome_unknown statement" )
        ; ( "a bad packet before the column count"
          , [F.Send (2, F.ok ())]
          , "Outcome_unknown statement"
          , "Outcome_unknown statement" )
        ; ( "a bad column count"
          , [F.Send (1, "\x01\x00")]
          , "Outcome_unknown statement"
          , "Outcome_unknown statement" )
        ; ( "1053"
          , [F.Send (1, F.err 1053 "shutdown")]
          , "Outcome_unknown statement"
          , "Outcome_unknown statement" )
        ; ( "1153"
          , [F.Send (1, F.err 1153 "too large")]
          , "Invalid_value 1153: too large"
          , "Invalid_value 1153: too large" )
        ; ( "the end of the stream after the flush"
          , [F.Close]
          , "Outcome_unknown statement"
          , "Outcome_unknown statement" )
        ; ( "a reset after the flush"
          , [F.Reset]
          , "Outcome_unknown statement"
          , "Outcome_unknown statement" ) ]
        (fun (_, steps, query, exec) ->
          List.iter
            (fun (run, expected) ->
              let result, received =
                after_handshake
                  (with_connection (F.Recv :: steps) (fun c ->
                       let r = run c in
                       (* A broken connection: no I/O, also no COM_QUIT. *)
                       String.concat "; "
                         [ r
                         ; result_text (Conn.query c (sql "SELECT 1"))
                         ; exec_text (Conn.exec c (sql "DO 1"))
                         ; unit_text (Conn.ping c)
                         ; unit_text (Conn.init_db c "db")
                         ; unit_text (Conn.reset c)
                         ; unit_text (Conn.Private.reset_connection c) ] ) )
              in
              equal string
                ( expected
                ^ "; Connection_lost; Connection_lost; Connection_lost; \
                   Connection_lost; Connection_lost; Connection_lost" )
                result ;
              equal packets [(0, q "SELECT")] received )
            [ ((fun c -> result_text (Conn.query c (sql "SELECT"))), query)
            ; ((fun c -> exec_text (Conn.exec c (sql "SELECT"))), exec) ] )
    ; cases "a command that is not a write" ~id:fst
        [ ("the end of the stream", [F.Recv; F.Close])
        ; ("1053", [F.Recv; F.Send (1, F.err 1053 "shutdown")])
        ; ("a result set", [F.Recv; F.Sends (1, rs [int_column "a"] [])]) ]
        (fun (_, steps) ->
          let result, _ =
            after_handshake
              (with_connection steps (fun c ->
                   let a = unit_text (Conn.init_db c "db2") in
                   a ^ "; " ^ unit_text (Conn.ping c) ) )
          in
          equal string "Connection_lost; Connection_lost" result )
    ; test "R-SEQ0: an error before the command" (fun () ->
          let result, received =
            after_handshake
              (with_connection
                 [F.Send (0, F.err 4031 "idle"); F.Recv; F.Close]
                 (fun c ->
                   let a = exec_text (Conn.exec c (sql "INSERT")) in
                   a ^ "; " ^ unit_text (Conn.ping c) ) )
          in
          equal string "Connection_lost; Connection_lost" result ;
          equal packets [(0, q "INSERT")] received )
    ; test "a failed write" (fun () ->
          let result, received =
            after_handshake
              (with_connection [F.Reset] (fun c ->
                   Time.sleep Mono (Span.ms 1) ;
                   let a = exec_text (Conn.exec c (sql "INSERT")) in
                   a ^ "; " ^ unit_text (Conn.ping c) ) )
          in
          equal string "Connection_lost; Connection_lost" result ;
          equal packets [] received )
    ; test "a part of the command went, then a reset" (fun () ->
          (* The command has 100 000 bytes. The server reads a part of it, with
             no packet, then it resets the connection. The rest does not fit in
             its receive buffer (65536 bytes in the simulator), thus the flush
             of the client waits and then fails. *)
          let text = "SELECT '" ^ String.make 99_991 'a' ^ "'" in
          let result, received, raw =
            F.run_raw
              (F.handshake () @ [F.Read_bytes 1000; F.Reset])
              (fun sim ->
                match Conn.connect (tcp ()) with
                | Error e ->
                    "connect: " ^ error_text e
                | Ok c ->
                    let a = exec_text (Conn.exec c (sql text)) in
                    let r = a ^ "; " ^ unit_text (Conn.ping c) in
                    Conn.close c ;
                    equal int 0 (F.client_fds sim) ;
                    r )
          in
          equal string "Connection_lost; Connection_lost" result ;
          (* The server read the packets of the handshake, then at least 1000
             bytes of the command, but not all of it: the frame header (100 001
             bytes, id 0) and the command byte. *)
          equal int 3 (List.length received) ;
          let handshake =
            String.length
              (String.concat ""
                 (List.map (fun (seq, p) -> F.frame seq p) received) )
          in
          let part = String.sub raw handshake (String.length raw - handshake) in
          is_true (String.length part >= 1000) ;
          is_true (String.length part < 100_005) ;
          equal string
            (String.sub (F.frame 0 ("\x03" ^ text)) 0 (String.length part))
            part )
    ; test "a network error at the connect" (fun () ->
          let result, _ =
            F.run [] (fun sim ->
                (* No fd more: the socket of the connect gives EMFILE. *)
                Weft_sim.set_fd_limit sim (Some (Weft_sim.open_fds sim)) ;
                let r =
                  match Conn.connect (tcp ()) with
                  | Ok c ->
                      Conn.close c ; "connected"
                  | Error e ->
                      error_text e
                in
                Weft_sim.set_fd_limit sim None ;
                r )
          in
          equal string
            "Connect network error: Weft.Net.Error(No_resources \"EMFILE\", \
             \"Net.connect\")"
            result )
    ; test "1317, then ping" (fun () ->
          let result, _ =
            after_handshake
              (with_connection
                 [ F.Recv
                 ; F.Send (1, F.err 1317 "interrupted")
                 ; F.Recv
                 ; F.Send (1, F.ok ()) ]
                 (fun c ->
                   let a = exec_text (Conn.exec c (sql "INSERT")) in
                   a ^ "; " ^ unit_text (Conn.ping c) ) )
          in
          equal string "Driver 1317 28000; ok" result )
    ; test "a cancel of a command" (fun () ->
          let result, received =
            after_handshake
              (with_connection [F.Recv; F.Hang] (fun c ->
                   let a =
                     match
                       Fiber.with_timeout Mono (Span.s 1) (fun () ->
                           Conn.exec c (sql "INSERT") )
                     with
                     | _ ->
                         "no timeout"
                     | exception Fiber.Timeout ->
                         "timeout"
                   in
                   a ^ "; " ^ unit_text (Conn.ping c) ) )
          in
          equal string "timeout; Connection_lost" result ;
          equal packets [(0, q "INSERT")] received )
    ; test "the size check" (fun () ->
          (* @@max_allowed_packet is 100: the payload (1 byte and the SQL) must
             be less than 100. *)
          let result, received =
            after_handshake
              (with_connection
                 ~values:(F.session ~max_packet:"100" ())
                 [F.Recv; F.Send (1, F.ok ())]
                 (fun c ->
                   let a = exec_text (Conn.exec c (sql (String.make 99 'a'))) in
                   let b = exec_text (Conn.exec c (sql (String.make 98 'b'))) in
                   a ^ "; " ^ b ) )
          in
          equal string
            "Invalid_value: the statement is too large for \
             @@max_allowed_packet; 0 0"
            result ;
          equal packets [(0, q (String.make 98 'b')); quit] received ;
          (* With @@max_allowed_packet at 16 MiB - 1 or more, the bound is that
             of one packet. *)
          List.iter
            (fun max_packet ->
              let result, received =
                after_handshake
                  (with_connection ~values:(F.session ~max_packet ()) []
                     (fun c ->
                       exec_text (Conn.exec c (sql (String.make 0xFFFFFE 'c'))) )
                  )
              in
              equal string
                "Invalid_value: the statement is too large for one packet \
                 (less than 16 MiB)"
                result ;
              equal packets [quit] received )
            ["16777215"; "67108864"] )
    ; test "a value with no literal" (fun () ->
          let result, received =
            after_handshake
              (with_connection [] (fun c ->
                   result_text
                     (Conn.query c Raw.(sql "SELECT" ++ v (Float Float.nan))) )
              )
          in
          equal string "Invalid_value: a NaN is not a SQL value" result ;
          equal packets [quit] received )
    ; test "init_db" (fun () ->
          let result, received =
            after_handshake
              (with_connection
                 [ F.Recv
                 ; F.Send (1, F.ok_with ~status:0x4002 ())
                 ; F.Recv
                 ; F.Send (1, F.err 1049 "unknown database")
                 ; F.Recv
                 ; F.Send (1, F.ok ()) ]
                 (fun c ->
                   let a = unit_text (Conn.init_db c "db2") in
                   let s = Conn.status c in
                   let b = unit_text (Conn.init_db c "nowhere") in
                   String.concat "; "
                     [a; string_of_int s; b; unit_text (Conn.ping c)] ) )
          in
          equal string "ok; 16386; Driver 1049 28000; ok" result ;
          equal packets
            [(0, "\x02db2"); (0, "\x02nowhere"); ping; quit]
            received )
    ; test "reset" (fun () ->
          let result, received =
            after_handshake
              (with_connection
                 [ F.Recv
                 ; F.Send (1, F.ok ())
                 ; F.Recv
                 ; F.Send (1, F.ok ())
                 ; F.Recv
                 ; F.Send (1, F.ok_with ~status:0x22 ())
                 ; F.Recv
                 ; F.Send (1, F.ok ()) ]
                 (fun c ->
                   let a = unit_text (Conn.reset c) in
                   let s = Conn.status c in
                   let b = unit_text (Conn.Private.reset_connection c) in
                   String.concat "; " [a; string_of_int s; b] ) )
          in
          equal string "ok; 34; ok" result ;
          equal packets
            [ (0, "\x1f")
            ; (0, Test_connect.rt2 "utf8mb4_0900_bin")
            ; (0, "\x02db")
            ; (0, "\x1f")
            ; quit ]
            received )
    ; cases "a step of reset that fails"
        ~id:(fun (n, _, _) -> n)
        [ ( "COM_RESET_CONNECTION"
          , [F.Recv; F.Send (1, F.err 1047 "unknown command")]
          , ["\x1f"] )
        ; ( "the SET"
          , [ F.Recv
            ; F.Send (1, F.ok ())
            ; F.Recv
            ; F.Send (1, F.err 1231 "bad sql_mode") ]
          , ["\x1f"; Test_connect.rt2 "utf8mb4_0900_bin"] )
        ; ( "COM_INIT_DB"
          , [ F.Recv
            ; F.Send (1, F.ok ())
            ; F.Recv
            ; F.Send (1, F.ok ())
            ; F.Recv
            ; F.Send (1, F.err 1049 "unknown database") ]
          , ["\x1f"; Test_connect.rt2 "utf8mb4_0900_bin"; "\x02db"] ) ]
        (fun (_, steps, sent) ->
          let result, received =
            after_handshake
              (with_connection steps (fun c ->
                   let a = unit_text (Conn.reset c) in
                   a ^ "; " ^ unit_text (Conn.ping c) ) )
          in
          is_true (String.ends_with ~suffix:"; Connection_lost" result) ;
          is_false (String.starts_with ~prefix:"ok" result) ;
          equal packets (List.map (fun p -> (0, p)) sent) received ) ]
