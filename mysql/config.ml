(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

type ssl_mode = Required | Verify_ca | Verify_identity

type address =
  | Tcp of {host: string; port: int}
  | Socket of {path: string; allow_cleartext: bool}

type tls =
  | No_tls
  | Tls of {mode: ssl_mode; ca: string option; fingerprint: string option}

type t =
  { address: address
  ; user: string
  ; password: string
  ; database: string
  ; tls: tls
  ; kill: bool }

let address t = t.address

let user t = t.user

let password t = t.password

let database t = t.database

let tls t = t.tls

let kill t = t.kill

let needs_tls t = t.tls <> No_tls

let ( let* ) = Result.bind

let hex c =
  match c with
  | '0' .. '9' ->
      Some (Char.code c - Char.code '0')
  | 'a' .. 'f' ->
      Some (Char.code c - Char.code 'a' + 10)
  | 'A' .. 'F' ->
      Some (Char.code c - Char.code 'A' + 10)
  | _ ->
      None

let host_char c = Char.Ascii.is_alphanum c || c = '-' || c = '.' || c = '_'

(* True if [s] is an IPv6 address in text form (RFC 4291, section 2.2): groups
   of 1 to 4 hex digits with [:] between them, [::] at most once for one or more
   groups of 0, and an IPv4 address in dotted form as the last 32 bits (each
   octet 0 to 255 with no leading zero, the dec-octet of RFC 3986). No zone id:
   weft cannot connect to it. *)
let ipv6 s =
  let hex_group g =
    g <> "" && String.length g <= 4 && String.for_all Char.Ascii.is_hex_digit g
  in
  let ipv4 g =
    match String.split_on_char '.' g with
    | [_; _; _; _] as parts ->
        List.for_all
          (fun x ->
            (* With digits only and no leading zero, a compare of the text finds
               0 to 255, with no int_of_string that can overflow. *)
            x <> ""
            && String.for_all Char.Ascii.is_digit x
            && (String.length x = 1 || x.[0] <> '0')
            && (String.length x < 3 || (String.length x = 3 && x <= "255")) )
          parts
    | _ ->
        false
  in
  (* The number of 16-bit groups of [part]: hex groups, and an IPv4 address (two
     groups) at the end. *)
  let groups part =
    if part = "" then Some 0
    else
      match List.rev (String.split_on_char ':' part) with
      | last :: before when List.for_all hex_group before ->
          if hex_group last then Some (List.length before + 1)
          else if ipv4 last then Some (List.length before + 2)
          else None
      | _ ->
          None
  in
  let rec find i =
    if i + 1 >= String.length s then None
    else if s.[i] = ':' && s.[i + 1] = ':' then Some i
    else find (i + 1)
  in
  match find 0 with
  | None ->
      groups s = Some 8
  | Some i -> (
      let left = String.sub s 0 i
      and right = String.sub s (i + 2) (String.length s - i - 2) in
      let left_groups =
        if left = "" then Some 0
        else
          let gs = String.split_on_char ':' left in
          if List.for_all hex_group gs then Some (List.length gs) else None
      in
      match (left_groups, groups right) with
      | Some l, Some r ->
          l + r < 8
      | _ ->
          false )

(* A byte that a value must not have: a control character. *)
let control s = String.exists (fun c -> Char.code c < 0x20 || c = '\x7f') s

(* The rules that of_url and make share. *)
let check ~address ~tls ~user ~password ~database ~kill =
  let paths =
    (match address with Tcp {host; _} -> [host] | Socket {path; _} -> [path])
    @ match tls with Tls {ca= Some ca; _} -> [ca] | _ -> []
  in
  let* () =
    if user = "" then Error "the user is empty"
    else if database = "" then Error "the database is empty"
    else if List.exists control (user :: password :: database :: paths) then
      Error "a value has a control character"
    else Ok ()
  in
  let* () =
    match address with
    | Tcp {host; port} ->
        if host = "" then Error "the host is empty"
        else if
          not
            ( if String.contains host ':' then ipv6 host
              else String.for_all host_char host )
        then Error "the host is not valid"
        else if port < 1 || port > 65535 then
          Error "the port is not in 1 to 65535"
        else Ok ()
    | Socket {path; _} ->
        if path = "" then Error "the socket path is empty"
        else if tls <> No_tls then Error "version 1 has no TLS on a Unix socket"
        else Ok ()
  in
  let* () =
    match tls with
    | Tls {ca= Some ""; _} ->
        Error "the CA path is empty"
    | Tls {fingerprint= Some f; _} when String.length f <> 32 ->
        Error "the fingerprint is not 32 bytes"
    | Tls {mode= Required; ca= Some _; _} ->
        Error
          "ssl-mode=REQUIRED has no ssl-ca: use VERIFY_CA or VERIFY_IDENTITY"
    | Tls {mode= Verify_ca | Verify_identity; ca= None; fingerprint= None} ->
        Error
          "ssl-ca or ssl-fingerprint is necessary for VERIFY_CA and \
           VERIFY_IDENTITY"
    | _ ->
        Ok ()
  in
  Ok {address; user; password; database; tls; kill}

let make ?tls ?(kill = true) ~address ~user ~password ~database () =
  let* tls =
    match (tls, address) with
    | Some tls, _ ->
        Ok tls
    | None, Socket _ ->
        Ok No_tls
    | None, Tcp _ ->
        Error "a TCP address must have a TLS setting"
  in
  check ~address ~tls ~user ~password ~database ~kill

(* The URL parser. No message has a part of the URL. *)

let decode s =
  let b = Buffer.create (String.length s) in
  let n = String.length s in
  let rec go i =
    if i >= n then Ok (Buffer.contents b)
    else if s.[i] <> '%' then (
      Buffer.add_char b s.[i] ;
      go (i + 1) )
    else if i + 2 >= n then Error "a percent escape is not two hex digits"
    else
      match (hex s.[i + 1], hex s.[i + 2]) with
      | Some h, Some l ->
          Buffer.add_char b (Char.chr ((h * 16) + l)) ;
          go (i + 3)
      | _ ->
          Error "a percent escape is not two hex digits"
  in
  go 0

let split_at s i =
  (String.sub s 0 i, String.sub s (i + 1) (String.length s - i - 1))

(* The host and the port (if the URL has one) of [hostport]. An empty host is
   permitted here: the socket rule decides. [check] examines the syntax of a
   host with no brackets. *)
let host_port hostport =
  let* host, rest =
    if String.starts_with ~prefix:"[" hostport then
      match String.index_opt hostport ']' with
      | None ->
          Error "the host is not valid"
      | Some j ->
          let inner = String.sub hostport 1 (j - 1) in
          if ipv6 inner then
            Ok
              ( inner
              , String.sub hostport (j + 1) (String.length hostport - j - 1) )
          else Error "the host is not valid"
    else
      let j =
        Option.value
          (String.index_opt hostport ':')
          ~default:(String.length hostport)
      in
      Ok
        ( String.sub hostport 0 j
        , String.sub hostport j (String.length hostport - j) )
  in
  match rest with
  | "" ->
      Ok (host, None)
  | _ when rest.[0] = ':' ->
      let p = String.sub rest 1 (String.length rest - 1) in
      if
        p = ""
        || String.length p > 5
        || not (String.for_all Char.Ascii.is_digit p)
      then Error "the port is not valid"
      else Ok (host, Some (int_of_string p))
  | _ ->
      Error "the host is not valid"

type query =
  { socket: string option
  ; ssl_mode: string option
  ; ssl_ca: string option
  ; fingerprint: string option
  ; cleartext: string option
  ; kill_value: string option }

let empty_query =
  { socket= None
  ; ssl_mode= None
  ; ssl_ca= None
  ; fingerprint= None
  ; cleartext= None
  ; kill_value= None }

let parse_query q =
  let items = if q = "" then [] else String.split_on_char '&' q in
  List.fold_left
    (fun acc item ->
      let* acc = acc in
      match String.index_opt item '=' with
      | None ->
          Error "a query item is not key=value"
      | Some i -> (
          let key, raw = split_at item i in
          let* value = decode raw in
          let set field = function
            | Some _ ->
                Error "a query key is in the URL two times"
            | None when value = "" ->
                Error "a query item has an empty value"
            | None ->
                Ok (field (Some value))
          in
          match key with
          | "socket" ->
              set (fun v -> {acc with socket= v}) acc.socket
          | "ssl-mode" ->
              set (fun v -> {acc with ssl_mode= v}) acc.ssl_mode
          | "ssl-ca" ->
              set (fun v -> {acc with ssl_ca= v}) acc.ssl_ca
          | "ssl-fingerprint" ->
              set (fun v -> {acc with fingerprint= v}) acc.fingerprint
          | "allow-cleartext" ->
              set (fun v -> {acc with cleartext= v}) acc.cleartext
          | "kill" ->
              set (fun v -> {acc with kill_value= v}) acc.kill_value
          | _ ->
              Error "the URL has an unknown query key" ) )
    (Ok empty_query) items

let ssl_mode_of = function
  | None ->
      Ok None
  | Some v -> (
    match String.uppercase_ascii v with
    | "DISABLED" ->
        Ok (Some `Disabled)
    | "REQUIRED" ->
        Ok (Some (`Mode Required))
    | "VERIFY_CA" ->
        Ok (Some (`Mode Verify_ca))
    | "VERIFY_IDENTITY" ->
        Ok (Some (`Mode Verify_identity))
    | _ ->
        Error "ssl-mode is not DISABLED, REQUIRED, VERIFY_CA or VERIFY_IDENTITY"
    )

let fingerprint_of = function
  | None ->
      Ok None
  | Some v ->
      let prefix = "sha256:" in
      let h =
        if String.starts_with ~prefix v then String.sub v 7 (String.length v - 7)
        else ""
      in
      if String.length h <> 64 || not (String.for_all Char.Ascii.is_hex_digit h)
      then Error "ssl-fingerprint is not sha256: and 64 hex digits"
      else
        (* Each character is a hex digit, thus [hex] gives [Some]. *)
        let digit i = Option.get (hex h.[i]) in
        Ok
          (Some
             (String.init 32 (fun i ->
                  Char.chr ((digit (2 * i) * 16) + digit ((2 * i) + 1)) ) ) )

let of_url url =
  let* rest =
    match String.index_opt url ':' with
    | Some i
      when List.mem (String.sub url 0 i) ["mysql"; "mariadb"]
           && String.length url >= i + 3
           && String.sub url i 3 = "://" ->
        Ok (String.sub url (i + 3) (String.length url - i - 3))
    | _ ->
        Error "the URL does not start with mysql:// or mariadb://"
  in
  let* () =
    if String.exists (fun c -> Char.code c <= 0x20 || c = '\x7f') rest then
      Error "the URL has a space or a control character"
    else if String.contains rest '#' then Error "the URL has a fragment (#)"
    else Ok ()
  in
  let rest, q =
    match String.index_opt rest '?' with
    | Some i ->
        split_at rest i
    | None ->
        (rest, "")
  in
  let authority, path =
    match String.index_opt rest '/' with
    | Some i ->
        split_at rest i
    | None ->
        (rest, "")
  in
  let* userinfo, hostport =
    match String.rindex_opt authority '@' with
    | Some i ->
        Ok (split_at authority i)
    | None ->
        Error "the URL has no user"
  in
  let raw_user, raw_password =
    match String.index_opt userinfo ':' with
    | Some i ->
        split_at userinfo i
    | None ->
        (userinfo, "")
  in
  let* user = decode raw_user in
  let* password = decode raw_password in
  let* () =
    if String.contains path '/' then
      Error "the URL has a path that is not /database"
    else if String.contains path '@' then
      Error
        "the URL has a raw @ after the host: percent-encode the @ of the \
         password"
    else Ok ()
  in
  let* database = decode path in
  let* host, port = host_port hostport in
  let* q = parse_query q in
  let* mode = ssl_mode_of q.ssl_mode in
  let* fingerprint = fingerprint_of q.fingerprint in
  let* allow_cleartext =
    match q.cleartext with
    | None ->
        Ok false
    | Some "socket" ->
        Ok true
    | Some _ ->
        Error "allow-cleartext has only the value socket"
  in
  let* kill =
    match q.kill_value with
    | None | Some "true" ->
        Ok true
    | Some "false" ->
        Ok false
    | Some _ ->
        Error "kill is not true or false"
  in
  let* address, tls =
    match q.socket with
    | Some path ->
        if host <> "" then Error "a URL has a host or socket=, not both"
        else if port <> None then Error "a URL with socket= has no port"
        else if
          q.ssl_ca <> None || fingerprint <> None
          || (mode <> None && mode <> Some `Disabled)
        then Error "version 1 has no TLS on a Unix socket"
        else Ok (Socket {path; allow_cleartext}, No_tls)
    | None -> (
        if host = "" then Error "the URL has no host and no socket="
        else if allow_cleartext then
          Error "allow-cleartext=socket is only for socket="
        else
          let address = Tcp {host; port= Option.value port ~default:3306} in
          match (mode, q.ssl_ca, fingerprint) with
          | Some `Disabled, None, None ->
              Ok (address, No_tls)
          | Some `Disabled, _, _ ->
              Error "ssl-mode=DISABLED has no ssl-ca and no ssl-fingerprint"
          | Some (`Mode mode), ca, fingerprint ->
              Ok (address, Tls {mode; ca; fingerprint})
          | None, (Some _ as ca), fingerprint ->
              Ok (address, Tls {mode= Verify_identity; ca; fingerprint})
          | None, None, (Some _ as fingerprint) ->
              Ok (address, Tls {mode= Required; ca= None; fingerprint})
          | None, None, None ->
              Error
                "a TCP connection must have a TLS setting: give ssl-ca, \
                 ssl-fingerprint or ssl-mode" )
  in
  check ~address ~tls ~user ~password ~database ~kill

let pp_mode ppf m =
  Format.pp_print_string ppf
    ( match m with
    | Required ->
        "REQUIRED"
    | Verify_ca ->
        "VERIFY_CA"
    | Verify_identity ->
        "VERIFY_IDENTITY" )

let pp ppf t =
  let pp_address ppf = function
    | Tcp {host; port} ->
        Format.fprintf ppf "tcp %s:%d" host port
    | Socket {path; allow_cleartext} ->
        Format.fprintf ppf "socket %s%s" path
          (if allow_cleartext then " (allow-cleartext)" else "")
  in
  let pp_tls ppf = function
    | No_tls ->
        Format.pp_print_string ppf "none"
    | Tls {mode; ca; fingerprint} ->
        Format.fprintf ppf "%a%s%s" pp_mode mode
          (match ca with Some c -> " ca=" ^ c | None -> "")
          (match fingerprint with Some _ -> " fingerprint" | None -> "")
  in
  (* One line, with no break hints, so that the text does not depend on the
     margin of the formatter. *)
  Format.fprintf ppf
    "{address = %a; user = %s; password = %s; database = %s; tls = %a; kill = \
     %b}"
    pp_address t.address t.user
    (if t.password = "" then "" else "***")
    t.database pp_tls t.tls t.kill
