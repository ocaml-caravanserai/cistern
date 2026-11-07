(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* Message: the decoders on the packets of the pinned servers (recorded on
   2026-11-07 on the Unix socket of make db-up), and on packets written by hand
   for the other cases. *)

open Windtrap
module M = Cistern_mysql.Message

let unhex h =
  String.init
    (String.length h / 2)
    (fun i -> Char.chr (int_of_string ("0x" ^ String.sub h (2 * i) 2)))

let mysql84 =
  unhex
    "0a382e342e3131000b0000001277514b776c776500ffffff0200ffdf150000000000000000000047771b3b236d222852332b790063616368696e675f736861325f70617373776f726400"

let mariadb118 =
  unhex
    "0a31312e382e382d4d617269614442000a000000744f6c543d734d6500feffe00200ff81150000000000003d000000367c307522436c7528654464006d7973716c5f6e61746976655f70617373776f726400"

let mariadb114 =
  unhex
    "0a31312e342e31332d4d617269614442000b00000078615c2c622d554500feffe00200ff81150000000000001d0000003c3b51662f533757302b267c006d7973716c5f6e61746976655f70617373776f726400"

(* The OK of the authentication of MariaDB 11.4.13, with 66 more bytes. *)
let mariadb_auth_ok =
  unhex
    "00000002000000410136324343313532394643383643323538463339363845433845394631463446324341364335384337373438374346344346454134323543463245313737463430"

(* The reply to SELECT 1 AS a, NULL AS b, 'x' AS c, CAST(2.5 AS DECIMAL(3,1)) AS
   d on MySQL 8.4.11: the column definitions, the row, the terminator. *)
let mysql_columns =
  List.map unhex
    [ "036465660000000161000c3f0002000000088100000000"
    ; "036465660000000162000c3f0000000000068000000000"
    ; "036465660000000163000c2d0004000000fd01001f0000"
    ; "036465660000000164000c3f0005000000f68100010000" ]

let mysql_row = unhex "0131fb017803322e35"

let terminator = unhex "fe000002000000"

let greeting_text (g : M.greeting) =
  Printf.sprintf "%s|%d|%s|%x|%d|%d|%x|%s" g.server_version g.thread_id
    (String.concat ""
       (List.map
          (fun c -> Printf.sprintf "%02x" (Char.code c))
          (List.of_seq (String.to_seq g.scramble)) ) )
    g.capabilities g.charset g.status g.mariadb_capabilities g.auth_plugin

let ok_text (o : M.ok) =
  Printf.sprintf "%Ld %Ld %x %d" o.affected_rows o.last_insert_id o.status
    o.warnings

let err_text (e : M.err) = Printf.sprintf "%d|%s|%s" e.code e.sqlstate e.message

let column_text (c : M.column) =
  Printf.sprintf "%s %d %d %d %x %d" c.name c.charset c.length c.typ c.flags
    c.decimals

let res = result string string

(* A greeting written by hand: protocol 10, version "v", the capabilities, the
   extended capabilities, the auth length and the rest. *)
let greeting_with ~caps ~ext ~auth_length ~tail =
  "\x0av\x00\x01\x00\x00\x00ABCDEFGH\x00"
  ^ String.init 2 (fun i -> Char.chr ((caps lsr (8 * i)) land 0xFF))
  ^ "\x2d\x02\x00"
  ^ String.init 2 (fun i -> Char.chr ((caps lsr (16 + (8 * i))) land 0xFF))
  ^ String.make 1 (Char.chr auth_length)
  ^ String.make 6 '\x00'
  ^ String.init 4 (fun i -> Char.chr ((ext lsr (8 * i)) land 0xFF))
  ^ tail

let tests =
  group "Message"
    [ test "flags" (fun () ->
          equal (list int)
            [ 0x1
            ; 0x2
            ; 0x8
            ; 0x80
            ; 0x200
            ; 0x800
            ; 0x2000
            ; 0x8000
            ; 0x10000
            ; 0x20000
            ; 0x80000
            ; 0x800000
            ; 0x1000000 ]
            (List.map M.bit
               [ Client_mysql
               ; Found_rows
               ; Connect_with_db
               ; Local_files
               ; Protocol_41
               ; Ssl
               ; Transactions
               ; Secure_connection
               ; Multi_statements
               ; Multi_results
               ; Plugin_auth
               ; Session_track
               ; Deprecate_eof ] ) ;
          is_true (M.has 0x201 Protocol_41) ;
          is_false (M.has 0x1 Protocol_41) ;
          equal (list int) [1; 2; 8]
            (List.map M.status_bit [In_trans; Autocommit; More_results_exists]) ;
          is_true (M.has_status 0x3 In_trans) ;
          is_false (M.has_status 0x2 In_trans) )
    ; cases "the greetings of the pinned servers"
        ~id:(fun (n, _, _) -> n)
        [ ( "mysql84"
          , mysql84
          , "8.4.11|11|1277514b776c776547771b3b236d222852332b79|dfffffff|255|2|0|caching_sha2_password"
          )
        ; ( "mariadb118"
          , mariadb118
          , "11.8.8-MariaDB|10|744f6c543d734d65367c307522436c7528654464|81fffffe|224|2|3d|mysql_native_password"
          )
        ; ( "mariadb114"
          , mariadb114
          , "11.4.13-MariaDB|11|78615c2c622d55453c3b51662f533757302b267c|81fffffe|224|2|1d|mysql_native_password"
          ) ]
        (fun (_, payload, expected) ->
          equal res (Ok expected)
            (Result.map greeting_text (M.greeting payload)) )
    ; test "greetings written by hand" (fun () ->
          let auth2 = "IJKLMNOPQRST\x00" in
          (* CLIENT_MYSQL set: the extended capabilities are not read. *)
          equal res
            (Ok "v|1|4142434445464748494a4b4c4d4e4f5051525354|201|45|2|0|p")
            (Result.map greeting_text
               (M.greeting
                  (greeting_with ~caps:0x201 ~ext:0x1d ~auth_length:21
                     ~tail:(auth2 ^ "p\x00") ) ) ) ;
          (* No NUL after the plugin name; a longer auth part. *)
          equal res
            (Ok "v|1|4142434445464748494a4b4c4d4e4f5051525354|200|45|2|7|q")
            (Result.map greeting_text
               (M.greeting
                  (greeting_with ~caps:0x200 ~ext:7 ~auth_length:22
                     ~tail:("IJKLMNOPQRSTU\x00" ^ "q") ) ) ) ;
          (* An auth length below 21 still reads 13 bytes, then the plugin
             name. *)
          equal res
            (Ok "v|1|4142434445464748494a4b4c4d4e4f5051525354|200|45|2|0|p")
            (Result.map greeting_text
               (M.greeting
                  (greeting_with ~caps:0x200 ~ext:0 ~auth_length:0
                     ~tail:(auth2 ^ "p\x00") ) ) ) ;
          equal res
            (Ok "v|1|4142434445464748494a4b4c4d4e4f5051525354|200|45|2|0|p")
            (Result.map greeting_text
               (M.greeting
                  (greeting_with ~caps:0x200 ~ext:0 ~auth_length:20
                     ~tail:(auth2 ^ "p\x00") ) ) ) ;
          (* No plugin name. *)
          equal res
            (Ok "v|1|4142434445464748494a4b4c4d4e4f5051525354|200|45|2|0|")
            (Result.map greeting_text
               (M.greeting
                  (greeting_with ~caps:0x200 ~ext:0 ~auth_length:0 ~tail:auth2) ) ) )
    ; test "greetings that are not valid" (fun () ->
          equal res (Error "the greeting is not protocol 10")
            (Result.map greeting_text (M.greeting "\x09v\x00")) ;
          equal res (Error "the greeting is not protocol 10")
            (Result.map greeting_text (M.greeting "")) ;
          equal res (Error "greeting: the packet is too short")
            (Result.map greeting_text (M.greeting (String.sub mysql84 0 40))) )
    ; test "the handshake response" (fun () ->
          let out =
            Wire_io.written (fun w ->
                M.write_handshake_response w ~capabilities:0x81a20a ~charset:45
                  ~user:"u" ~auth:"\x01\x02" ~database:"db"
                  ~plugin:"mysql_native_password" )
          in
          equal string
            ( "\x0a\xa2\x81\x00" ^ "\x00\x00\x00\x01" ^ "\x2d"
            ^ String.make 23 '\x00' ^ "u\x00" ^ "\x02\x01\x02" ^ "db\x00"
            ^ "mysql_native_password\x00" )
            out ;
          let long = String.make 255 'a' in
          is_true
            ( String.length
                (Wire_io.written (fun w ->
                     M.write_handshake_response w ~capabilities:0 ~charset:45
                       ~user:"u" ~auth:long ~database:"d" ~plugin:"p" ) )
            > 255 ) ;
          raises
            (Invalid_argument
               "Message.write_handshake_response: auth data of 256 bytes" )
            (fun () ->
              Wire_io.written (fun w ->
                  M.write_handshake_response w ~capabilities:0 ~charset:45
                    ~user:"u" ~auth:(long ^ "a") ~database:"d" ~plugin:"p" ) ) ;
          List.iter
            (fun capabilities ->
              raises
                (Invalid_argument
                   "Message.write_handshake_response: \
                    PLUGIN_AUTH_LENENC_CLIENT_DATA or CONNECT_ATTRS" )
                (fun () ->
                  Wire_io.written (fun w ->
                      M.write_handshake_response w ~capabilities ~charset:45
                        ~user:"u" ~auth:"" ~database:"d" ~plugin:"p" ) ) )
            [0x20_0000; 0x10_0000] ;
          raises (Invalid_argument "Wire.write_string_nul: a NUL") (fun () ->
              Wire_io.written (fun w ->
                  M.write_handshake_response w ~capabilities:0 ~charset:45
                    ~user:"u\x00" ~auth:"" ~database:"d" ~plugin:"p" ) ) )
    ; cases "OK"
        ~id:(fun (n, _, _) -> n)
        [ ("MariaDB after the authentication", mariadb_auth_ok, Ok "0 0 2 0")
        ; ("values", "\x00\xfc\x00\x01\x05\x03\x00\x07\x00", Ok "256 5 3 7")
        ; ( "unsigned last insert id"
          , "\x00\x01\xfe\xff\xff\xff\xff\xff\xff\xff\xff\x00\x00\x00\x00"
          , Ok "1 -1 0 0" )
        ; ( "an info text"
          , "\x00\x01\x00\x22\x00\x00\x00Rows matched: 1"
          , Ok "1 0 22 0" )
        ; ( "NULL rows"
          , "\x00\xfb\x00\x00\x00\x00\x00"
          , Error "OK: the affected rows is NULL" )
        ; ( "NULL id"
          , "\x00\x00\xfb\x00\x00\x00\x00"
          , Error "OK: the last insert id is NULL" )
        ; ( "short"
          , "\x00\x00\x00\x02\x00\x00"
          , Error "OK: the packet is too short" )
        ; ("not OK", "\x01\x00", Error "OK: the packet does not start with 0x00")
        ; ( "a terminator"
          , terminator
          , Error "OK: the packet does not start with 0x00" )
        ; ( "an AuthSwitchRequest"
          , "\xfemysql_native_password\x00abcdefghijklmnopqrst\x00"
          , Error "OK: the packet does not start with 0x00" )
        ; ("empty", "", Error "OK: the packet does not start with 0x00") ]
        (fun (_, payload, expected) ->
          equal res expected (Result.map ok_text (M.ok payload)) )
    ; cases "terminator"
        ~id:(fun (n, _, _) -> n)
        [ ("terminator", terminator, Ok "0 0 2 0")
        ; ("values", "\xfe\x02\x05\x0a\x00\x01\x00", Ok "2 5 a 1")
        ; ( "an OK"
          , "\x00\x00\x00\x02\x00\x00\x00"
          , Error "terminator: the packet does not start with 0xFE" )
        ; ( "short"
          , "\xfe\x00\x00\x02\x00\x00"
          , Error "terminator: the packet is too short" )
        ; ("empty", "", Error "terminator: the packet does not start with 0xFE")
        ]
        (fun (_, payload, expected) ->
          equal res expected (Result.map ok_text (M.terminator payload)) )
    ; cases "ERR"
        ~id:(fun (n, _, _) -> n)
        [ ( "with SQLSTATE"
          , "\xff\x26\x04#23000Duplicate entry"
          , Ok "1062|23000|Duplicate entry" )
        ; ( "before the handshake"
          , "\xff\x10\x04Too many connections"
          , Ok "1040||Too many connections" )
        ; ("no message", "\xff\x10\x04", Ok "1040||")
        ; ( "4031"
          , "\xff\xbf\x0f#HY000The client was disconnected"
          , Ok "4031|HY000|The client was disconnected" )
        ; ( "short SQLSTATE"
          , "\xff\x10\x04#230"
          , Error "ERR: the packet is too short" )
        ; ( "only the marker"
          , "\xff\x10\x04#"
          , Error "ERR: the packet is too short" )
        ; ("short", "\xff\x10", Error "ERR: the packet is too short")
        ; ( "not ERR"
          , "\x00\x00"
          , Error "ERR: the packet does not start with 0xFF" ) ]
        (fun (_, payload, expected) ->
          equal res expected (Result.map err_text (M.err payload)) )
    ; test "AuthSwitchRequest and AuthMoreData" (fun () ->
          let text =
            Result.map (fun (s : M.auth_switch) ->
                s.plugin ^ "|" ^ String.escaped s.data )
          in
          equal res (Ok "mysql_native_password|abc\\000")
            (text (M.auth_switch "\xfemysql_native_password\x00abc\x00")) ;
          equal res (Ok "mysql_old_password|") (text (M.auth_switch "\xfe")) ;
          equal res (Ok "p|") (text (M.auth_switch "\xfep\x00")) ;
          equal res (Error "AuthSwitchRequest: the packet is too short")
            (text (M.auth_switch "\xfep")) ;
          equal res
            (Error "AuthSwitchRequest: the packet does not start with 0xFE")
            (text (M.auth_switch "\x01p\x00")) ;
          equal res (Ok "\x03") (M.auth_more_data "\x01\x03") ;
          equal res (Ok "") (M.auth_more_data "\x01") ;
          equal res (Error "AuthMoreData: the packet does not start with 0x01")
            (M.auth_more_data "\x02\x03") ;
          equal res (Error "AuthMoreData: the packet does not start with 0x01")
            (M.auth_more_data "") )
    ; test "the column definitions of MySQL 8.4" (fun () ->
          equal (list res)
            [ Ok "a 63 2 8 81 0"
            ; Ok "b 63 0 6 80 0"
            ; Ok "c 45 4 253 1 31"
            ; Ok "d 63 5 246 81 1" ]
            (List.map
               (fun p -> Result.map column_text (M.column p))
               mysql_columns ) )
    ; test "column definitions that are not valid" (fun () ->
          let good = List.hd mysql_columns in
          equal res (Error "column definition: the packet is too short")
            (Result.map column_text
               (M.column (String.sub good 0 (String.length good - 1))) ) ;
          equal res (Error "column definition: a name is NULL")
            (Result.map column_text (M.column ("\xfb" ^ good))) ;
          let bad_fixed = Bytes.of_string good in
          Bytes.set bad_fixed 10 '\x0b' ;
          equal res
            (Error "column definition: the fixed fields do not have 12 bytes")
            (Result.map column_text (M.column (Bytes.to_string bad_fixed))) ;
          let null_fixed = Bytes.of_string good in
          Bytes.set null_fixed 10 '\xfb' ;
          equal res
            (Error "column definition: the length of the fixed fields is NULL")
            (Result.map column_text (M.column (Bytes.to_string null_fixed))) )
    ; test "rows" (fun () ->
          let r = result (array (option string)) string in
          equal r
            (Ok [|Some "1"; None; Some "x"; Some "2.5"|])
            (M.row ~columns:4 mysql_row) ;
          equal r (Ok [||]) (M.row ~columns:0 "") ;
          equal r (Error "row: the row has more bytes than its values")
            (M.row ~columns:3 mysql_row) ;
          equal r (Error "row: the packet is too short")
            (M.row ~columns:5 mysql_row) ;
          equal r (Error "row: a length-encoded integer starts with 0xFF")
            (M.row ~columns:1 "\xff") )
    ; test "commands" (fun () ->
          let w c = Wire_io.written (fun w -> M.write_command w c) in
          equal (list string)
            ["\x01"; "\x02db"; "\x03SELECT 1"; "\x0e"; "\x1f"]
            (List.map w
               [M.Quit; Init_db "db"; Query "SELECT 1"; Ping; Reset_connection] ) )
    ]
