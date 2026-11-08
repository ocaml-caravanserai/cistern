(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(** The messages of the MySQL protocol (DESIGN.md, "Handshake" and "Commands and
    packets"; the plan of phase 1.1, section 3.3).

    Each decoder takes the payload of one packet. A payload that is too short,
    or that has a value that the protocol does not permit, gives [Error]. No
    decoder raises. The client sets [CLIENT_DEPRECATE_EOF], thus no message is a
    classic EOF packet: the end of a result set is an OK packet that starts with
    [0xFE]. *)

(** {1 Flags} *)

type capability =
  | Client_mysql  (** Bit 0: clear on MariaDB, set on MySQL. *)
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

val bit : capability -> int
(** The bit of a capability in the capability flags. *)

val has : int -> capability -> bool
(** [has flags c] is true if [flags] has the bit of [c]. *)

type status = In_trans | Autocommit | More_results_exists

val status_bit : status -> int

val has_status : int -> status -> bool

(** {1 The greeting and the handshake response} *)

type greeting =
  { server_version: string
        (** The version of the greeting. The dialect does not come from it
            (DESIGN.md, "Main design decisions"). *)
  ; thread_id: int  (** 4 bytes. *)
  ; scramble: string  (** The 20 bytes of the nonce. *)
  ; capabilities: int  (** 32 bits. *)
  ; charset: int
  ; status: int
  ; mariadb_capabilities: int
        (** The MariaDB extended capabilities: bytes 28 to 31 of the reserved
            area, if [Client_mysql] is clear. Else 0. *)
  ; auth_plugin: string  (** Empty if the greeting has no name. *) }

val greeting : string -> (greeting, string) result
(** [greeting payload] decodes a greeting of protocol 10. Another protocol is an
    [Error]. An ERR packet in place of the greeting is not a greeting: use
    {!val-err}. A greeting with no [Secure_connection] can have no second part
    of the auth data: it then gives "greeting: the packet is too short". The
    connect checks the capabilities of a greeting that decodes. *)

val write_ssl_request :
  Weft_flow.Writer.t -> capabilities:int -> charset:int -> unit
(** [write_ssl_request w ~capabilities ~charset] writes the payload of the SSL
    request (phase 1.4): the first 32 bytes of {!write_handshake_response}, with
    the same function. Thus bytes 28 to 31 (the MariaDB extended capabilities)
    are 0 in the two packets.

    It raises [Invalid_argument] if the capabilities do not have [Ssl]. *)

val write_handshake_response :
     Weft_flow.Writer.t
  -> capabilities:int
  -> charset:int
  -> user:string
  -> auth:string
  -> database:string
  -> plugin:string
  -> unit
(** [write_handshake_response w ~capabilities ~charset ~user ~auth ~database
     ~plugin] writes the payload of a handshake response of protocol 41: the
    capabilities, a maximum packet of 16 MiB, the character set, 23 bytes of 0
    (thus the MariaDB extended capabilities are 0), the user, the auth data with
    its length in 1 byte, the database and the plugin name. The capabilities
    must have [Connect_with_db], [Plugin_auth] and [Secure_connection]. They
    must not have [PLUGIN_AUTH_LENENC_CLIENT_DATA] (0x200000) or [CONNECT_ATTRS]
    (0x100000): the auth length has 1 byte, and the response has no attributes.

    It raises [Invalid_argument] if [auth] has more than 255 bytes, if a string
    has a NUL, or if the capabilities have one of the two bits. *)

(** {1 Replies} *)

type ok =
  { affected_rows: int64
  ; last_insert_id: int64
        (** The two values are unsigned 64-bit values. A value above
            [Int64.max_int] is a negative number with the same 64 bits. *)
  ; status: int
  ; warnings: int }

val ok : string -> (ok, string) result
(** [ok payload] decodes an OK packet, which starts with [0x00]. The bytes after
    the warnings (an info text, or the 66 bytes of MariaDB after the
    authentication) are ignored. An [AuthSwitchRequest] starts with [0xFE], thus
    it is never an OK. *)

val terminator : string -> (ok, string) result
(** [terminator payload] decodes the end of a result set: a packet with the
    layout of an OK that starts with [0xFE]. Only {!Resultset} uses it. *)

type err = {code: int; sqlstate: string; message: string}

val err : string -> (err, string) result
(** [err payload] decodes an ERR packet ([0xFF]). With no [#] marker (an error
    before the handshake), [sqlstate] is empty. *)

type auth_switch = {plugin: string; data: string}

val auth_switch : string -> (auth_switch, string) result
(** [auth_switch payload] decodes an [AuthSwitchRequest] ([0xFE]). The old form
    (the byte [0xFE] alone) gives the plugin [mysql_old_password] with no data.
*)

val auth_more_data : string -> (string, string) result
(** [auth_more_data payload] gives the data of an [AuthMoreData] packet
    ([0x01]). *)

(** {1 Result sets} *)

type column =
  { name: string
  ; charset: int  (** 2 bytes. Only 63 (binary) has a meaning. *)
  ; length: int
  ; typ: int
  ; flags: int
  ; decimals: int }

val column : string -> (column, string) result
(** [column payload] decodes a column definition of protocol 41. *)

val row : columns:int -> string -> (string option array, string) result
(** [row ~columns payload] decodes a text row of [columns] values. [None] is
    NULL. More or fewer bytes than the values is an [Error]. *)

(** {1 Commands} *)

type command =
  | Quit
  | Init_db of string
  | Query of string
  | Ping
  | Reset_connection

val write_command : Weft_flow.Writer.t -> command -> unit
(** [write_command w c] writes the payload of the command [c]. *)
