(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

module Writer = Weft_flow.Writer

type capability =
  | Client_mysql
  | Found_rows
  | Connect_with_db
  | Local_files
  | Protocol_41
  | Ssl
  | Transactions
  | Secure_connection
  | Multi_statements
  | Multi_results
  | Plugin_auth
  | Session_track
  | Deprecate_eof

let bit = function
  | Client_mysql ->
      0x1
  | Found_rows ->
      0x2
  | Connect_with_db ->
      0x8
  | Local_files ->
      0x80
  | Protocol_41 ->
      0x200
  | Ssl ->
      0x800
  | Transactions ->
      0x2000
  | Secure_connection ->
      0x8000
  | Multi_statements ->
      0x1_0000
  | Multi_results ->
      0x2_0000
  | Plugin_auth ->
      0x8_0000
  | Session_track ->
      0x80_0000
  | Deprecate_eof ->
      0x100_0000

let has flags c = flags land bit c <> 0

type status = In_trans | Autocommit | More_results_exists

let status_bit = function
  | In_trans ->
      0x1
  | Autocommit ->
      0x2
  | More_results_exists ->
      0x8

let has_status flags s = flags land status_bit s <> 0

(* Run a decoder on a payload, and turn the exceptions of Wire into Error. *)
let decode what f payload =
  match f (Wire.cursor payload) with
  | v ->
      Ok v
  | exception Wire.Truncated ->
      Error (what ^ ": the packet is too short")
  | exception Wire.Malformed m ->
      Error (what ^ ": " ^ m)

let first payload = if payload = "" then -1 else Char.code payload.[0]

type greeting =
  { server_version: string
  ; thread_id: int
  ; scramble: string
  ; capabilities: int
  ; charset: int
  ; status: int
  ; mariadb_capabilities: int
  ; auth_plugin: string }

let greeting payload =
  if first payload <> 10 then Error "the greeting is not protocol 10"
  else
    decode "greeting"
      (fun c ->
        Wire.skip c 1 ;
        let server_version = Wire.string_nul c in
        let thread_id = Wire.uint32 c in
        let auth1 = Wire.bytes c 8 in
        Wire.skip c 1 ;
        let low = Wire.uint16 c in
        let charset = Wire.uint8 c in
        let status = Wire.uint16 c in
        let high = Wire.uint16 c in
        let auth_length = Wire.uint8 c in
        Wire.skip c 6 ;
        let extended = Wire.uint32 c in
        let capabilities = low lor (high lsl 16) in
        let auth2 = Wire.bytes c (max 13 (auth_length - 8)) in
        let auth_plugin =
          match Wire.string_nul c with
          | name ->
              name
          | exception Wire.Truncated ->
              Wire.rest c
        in
        { server_version
        ; thread_id
        ; scramble= auth1 ^ String.sub auth2 0 12
        ; capabilities
        ; charset
        ; status
        ; mariadb_capabilities=
            (if has capabilities Client_mysql then 0 else extended)
        ; auth_plugin } )
      payload

(* The first 32 bytes of the SSL request and of the handshake response: the
   capabilities, a maximum packet of 16 MiB, the character set, and 23 bytes of
   0. The last 4 of them are the MariaDB extended capabilities. One function
   writes them for the two packets, thus the two always have the same bytes. *)
let write_prefix w ~capabilities ~charset =
  Writer.write_int32_le w (Int32.of_int capabilities) ;
  Writer.write_int32_le w 0x100_0000l ;
  Writer.write_uint8 w charset ;
  Writer.write_string w (String.make 23 '\000')

let write_ssl_request w ~capabilities ~charset =
  if not (has capabilities Ssl) then
    invalid_arg "Message.write_ssl_request: no CLIENT_SSL" ;
  write_prefix w ~capabilities ~charset

let write_handshake_response w ~capabilities ~charset ~user ~auth ~database
    ~plugin =
  if String.length auth > 255 then
    invalid_arg "Message.write_handshake_response: auth data of 256 bytes" ;
  (* PLUGIN_AUTH_LENENC_CLIENT_DATA (0x200000) and CONNECT_ATTRS (0x100000)
     change the layout that this function writes. *)
  if capabilities land 0x30_0000 <> 0 then
    invalid_arg
      "Message.write_handshake_response: PLUGIN_AUTH_LENENC_CLIENT_DATA or \
       CONNECT_ATTRS" ;
  write_prefix w ~capabilities ~charset ;
  Wire.write_string_nul w user ;
  Writer.write_uint8 w (String.length auth) ;
  Writer.write_string w auth ;
  Wire.write_string_nul w database ;
  Wire.write_string_nul w plugin

type ok =
  {affected_rows: int64; last_insert_id: int64; status: int; warnings: int}

let lenenc_value what c =
  match Wire.lenenc c with
  | Wire.Value n ->
      n
  | Null ->
      raise (Wire.Malformed (what ^ " is NULL"))

(* The OK layout after its first byte. [what] names the packet in a message. *)
let ok_body what payload =
  decode what
    (fun c ->
      Wire.skip c 1 ;
      let affected_rows = lenenc_value "the affected rows" c in
      let last_insert_id = lenenc_value "the last insert id" c in
      let status = Wire.uint16 c in
      let warnings = Wire.uint16 c in
      {affected_rows; last_insert_id; status; warnings} )
    payload

let ok payload =
  if first payload <> 0x00 then Error "OK: the packet does not start with 0x00"
  else ok_body "OK" payload

let terminator payload =
  if first payload <> 0xFE then
    Error "terminator: the packet does not start with 0xFE"
  else ok_body "terminator" payload

type err = {code: int; sqlstate: string; message: string}

let err payload =
  if first payload <> 0xFF then Error "ERR: the packet does not start with 0xFF"
  else
    decode "ERR"
      (fun c ->
        Wire.skip c 1 ;
        let code = Wire.uint16 c in
        let sqlstate =
          if Wire.remaining c > 0 && payload.[3] = '#' then (
            Wire.skip c 1 ; Wire.bytes c 5 )
          else ""
        in
        {code; sqlstate; message= Wire.rest c} )
      payload

type auth_switch = {plugin: string; data: string}

let auth_switch payload =
  if first payload <> 0xFE then
    Error "AuthSwitchRequest: the packet does not start with 0xFE"
  else if String.length payload = 1 then
    Ok {plugin= "mysql_old_password"; data= ""}
  else
    decode "AuthSwitchRequest"
      (fun c ->
        Wire.skip c 1 ;
        let plugin = Wire.string_nul c in
        {plugin; data= Wire.rest c} )
      payload

let auth_more_data payload =
  if first payload <> 0x01 then
    Error "AuthMoreData: the packet does not start with 0x01"
  else Ok (String.sub payload 1 (String.length payload - 1))

type column =
  {name: string; charset: int; length: int; typ: int; flags: int; decimals: int}

let column payload =
  decode "column definition"
    (fun c ->
      let text () =
        match Wire.lenenc_string c with
        | Some s ->
            s
        | None ->
            raise (Wire.Malformed "a name is NULL")
      in
      let _catalog = text () in
      let _schema = text () in
      let _table = text () in
      let _org_table = text () in
      let name = text () in
      let _org_name = text () in
      let fixed = lenenc_value "the length of the fixed fields" c in
      if fixed <> 0x0CL then
        raise (Wire.Malformed "the fixed fields do not have 12 bytes") ;
      let charset = Wire.uint16 c in
      let length = Wire.uint32 c in
      let typ = Wire.uint8 c in
      let flags = Wire.uint16 c in
      let decimals = Wire.uint8 c in
      Wire.skip c 2 ;
      {name; charset; length; typ; flags; decimals} )
    payload

let row ~columns payload =
  decode "row"
    (fun c ->
      let values = Array.init columns (fun _ -> Wire.lenenc_string c) in
      if Wire.remaining c <> 0 then
        raise (Wire.Malformed "the row has more bytes than its values") ;
      values )
    payload

type command =
  | Quit
  | Init_db of string
  | Query of string
  | Ping
  | Reset_connection

let write_command w = function
  | Quit ->
      Writer.write_uint8 w 0x01
  | Init_db database ->
      Writer.write_uint8 w 0x02 ;
      Writer.write_string w database
  | Query sql ->
      Writer.write_uint8 w 0x03 ; Writer.write_string w sql
  | Ping ->
      Writer.write_uint8 w 0x0E
  | Reset_connection ->
      Writer.write_uint8 w 0x1F
