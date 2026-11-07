(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* Config: the URL rules of DESIGN.md "Configuration" and of the plan of phase
   1.1 (section 4.1), make, and the printer. *)

open Windtrap
module C = Cistern_mysql.Config

let hex s =
  String.concat ""
    (List.map
       (fun c -> Printf.sprintf "%02x" (Char.code c))
       (List.of_seq (String.to_seq s)) )

(* All the fields of a configuration, also the password (only in the tests). *)
let show c =
  let address =
    match C.address c with
    | C.Tcp {host; port} ->
        Printf.sprintf "tcp %s:%d" host port
    | Socket {path; allow_cleartext} ->
        Printf.sprintf "unix %s%s" path
          (if allow_cleartext then " clear" else "")
  in
  let tls =
    match C.tls c with
    | C.No_tls ->
        "none"
    | Tls {mode; ca; fingerprint} ->
        Printf.sprintf "%s ca=%s fp=%s"
          ( match mode with
          | C.Required ->
              "REQUIRED"
          | Verify_ca ->
              "VERIFY_CA"
          | Verify_identity ->
              "VERIFY_IDENTITY" )
          (Option.value ca ~default:"-")
          (Option.fold ~none:"-" ~some:hex fingerprint)
  in
  Printf.sprintf "%s|%s|%s|%s|%s|%b" address (C.user c) (C.password c)
    (C.database c) tls (C.kill c)

let parse url = Result.map show (C.of_url url)

let fp = String.make 64 'a'

let fp_bytes = String.concat "" (List.init 32 (fun _ -> "aa"))

(* Each row: a URL, and the configuration or the message. *)
let urls =
  [ ( "mysql://u:p@127.0.0.1/db?ssl-mode=DISABLED"
    , Ok "tcp 127.0.0.1:3306|u|p|db|none|true" )
  ; ( "mariadb://u:p@h:3307/db?ssl-mode=disabled"
    , Ok "tcp h:3307|u|p|db|none|true" )
  ; ("mysql://u@h:1/db?ssl-mode=DISABLED", Ok "tcp h:1|u||db|none|true")
  ; ("mysql://u:@h:65535/db?ssl-mode=DISABLED", Ok "tcp h:65535|u||db|none|true")
  ; ( "mysql://us%65r:a%40b%3Ac%2Fd%3Fe%23f%2525@h/d%62?ssl-mode=DISABLED"
    , Ok "tcp h:3306|user|a@b:c/d?e#f%25|db|none|true" )
  ; ("mysql://u:p:q@h/db?ssl-mode=DISABLED", Ok "tcp h:3306|u|p:q|db|none|true")
  ; ("mysql://u:a@b@h/db?ssl-mode=DISABLED", Ok "tcp h:3306|u|a@b|db|none|true")
  ; ( "mysql://u:p@[::1]:3307/db?ssl-mode=DISABLED"
    , Ok "tcp ::1:3307|u|p|db|none|true" )
  ; ( "mysql://u:p@[::1]/db?ssl-mode=DISABLED"
    , Ok "tcp ::1:3306|u|p|db|none|true" )
  ; ( "mysql://u:p@db-1.example_x.com/db?ssl-mode=DISABLED"
    , Ok "tcp db-1.example_x.com:3306|u|p|db|none|true" )
  ; ("mysql://u:p@/db?socket=/tmp/s%20s", Ok "unix /tmp/s s|u|p|db|none|true")
  ; ( "mysql://u:p@/db?socket=/s&allow-cleartext=socket"
    , Ok "unix /s clear|u|p|db|none|true" )
  ; ( "mysql://u:p@/db?socket=/s&ssl-mode=DISABLED&kill=false"
    , Ok "unix /s|u|p|db|none|false" )
  ; ( "mysql://u:p@h/db?ssl-ca=/ca.pem"
    , Ok "tcp h:3306|u|p|db|VERIFY_IDENTITY ca=/ca.pem fp=-|true" )
  ; ( "mysql://u:p@h/db?ssl-ca=/ca.pem&ssl-fingerprint=sha256:" ^ fp
    , Ok
        ("tcp h:3306|u|p|db|VERIFY_IDENTITY ca=/ca.pem fp=" ^ fp_bytes ^ "|true")
    )
  ; ( "mysql://u:p@h/db?ssl-fingerprint=sha256:" ^ String.uppercase_ascii fp
    , Ok ("tcp h:3306|u|p|db|REQUIRED ca=- fp=" ^ fp_bytes ^ "|true") )
  ; ( "mysql://u:p@h/db?ssl-fingerprint=sha256:0123456789abcdefABCDEF0123456789abcdef0123456789abcdef0123456789"
    , Ok
        "tcp h:3306|u|p|db|REQUIRED ca=- \
         fp=0123456789abcdefabcdef0123456789abcdef0123456789abcdef0123456789|true"
    )
  ; ( "mysql://u:p@h/db?ssl-mode=REQUIRED"
    , Ok "tcp h:3306|u|p|db|REQUIRED ca=- fp=-|true" )
  ; ( "mysql://u:p@h/db?ssl-mode=VERIFY_CA&ssl-ca=/c"
    , Ok "tcp h:3306|u|p|db|VERIFY_CA ca=/c fp=-|true" )
  ; ( "mysql://u:p@h/db?ssl-mode=verify_identity&ssl-fingerprint=sha256:" ^ fp
    , Ok ("tcp h:3306|u|p|db|VERIFY_IDENTITY ca=- fp=" ^ fp_bytes ^ "|true") )
  ; ( "mysql://u:p@h/db?kill=true&ssl-mode=DISABLED"
    , Ok "tcp h:3306|u|p|db|none|true" )
  ; ( "postgres://u:p@h/db?ssl-mode=DISABLED"
    , Error "the URL does not start with mysql:// or mariadb://" )
  ; ( "mysql:/u:p@h/db"
    , Error "the URL does not start with mysql:// or mariadb://" )
  ; ("mysql:", Error "the URL does not start with mysql:// or mariadb://")
  ; ("mysql://", Error "the URL has no user")
  ; ("mysql:/", Error "the URL does not start with mysql:// or mariadb://")
  ; ( "MYSQL://u:p@h/db?ssl-mode=DISABLED"
    , Error "the URL does not start with mysql:// or mariadb://" )
  ; ("mysql://u:p@h/db?ssl-mode=DISABLED#x", Error "the URL has a fragment (#)")
  ; ("mysql://h/db?ssl-mode=DISABLED", Error "the URL has no user")
  ; ("mysql://:p@h/db?ssl-mode=DISABLED", Error "the user is empty")
  ; ("mysql://u:p@h?ssl-mode=DISABLED", Error "the database is empty")
  ; ("mysql://u:p@h/?ssl-mode=DISABLED", Error "the database is empty")
  ; ( "mysql://u:p@h/a/b?ssl-mode=DISABLED"
    , Error "the URL has a path that is not /database" )
  ; ( "mysql://u:p%4@h/db?ssl-mode=DISABLED"
    , Error "a percent escape is not two hex digits" )
  ; ( "mysql://u:p%zz@h/db?ssl-mode=DISABLED"
    , Error "a percent escape is not two hex digits" )
  ; ( "mysql://u:p@h/db%?ssl-mode=DISABLED"
    , Error "a percent escape is not two hex digits" )
  ; ( "mysql://u:p%00@h/db?ssl-mode=DISABLED"
    , Error "a value has a control character" )
  ; ( "mysql://u:p@h:0/db?ssl-mode=DISABLED"
    , Error "the port is not in 1 to 65535" )
  ; ( "mysql://u:p@h:65536/db?ssl-mode=DISABLED"
    , Error "the port is not in 1 to 65535" )
  ; ("mysql://u:p@h:123456/db?ssl-mode=DISABLED", Error "the port is not valid")
  ; ("mysql://u:p@h:/db?ssl-mode=DISABLED", Error "the port is not valid")
  ; ("mysql://u:p@h:x/db?ssl-mode=DISABLED", Error "the port is not valid")
  ; ("mysql://u:p@h%20x/db?ssl-mode=DISABLED", Error "the host is not valid")
  ; ("mysql://u:p@[::1/db?ssl-mode=DISABLED", Error "the host is not valid")
  ; ("mysql://u:p@[]/db?ssl-mode=DISABLED", Error "the host is not valid")
  ; ("mysql://u:p@[::g]/db?ssl-mode=DISABLED", Error "the host is not valid")
  ; ("mysql://u:p@[::1]x/db?ssl-mode=DISABLED", Error "the host is not valid")
  ; ("mysql://u:p@h/db?ssl-mode", Error "a query item is not key=value")
  ; ("mysql://u:p@h/db?ssl-mode=", Error "a query item has an empty value")
  ; ( "mysql://u:p@h/db?ssl-mode=DISABLED&ssl-mode=DISABLED"
    , Error "a query key is in the URL two times" )
  ; ( "mysql://u:p@h/db?ssl-mode=DISABLED&&kill=true"
    , Error "a query item is not key=value" )
  ; ( "mysql://u:p@h/db?sslmode=DISABLED"
    , Error "the URL has an unknown query key" )
  ; ( "mysql://u:p@h/db?ssl-mode=PREFERRED"
    , Error "ssl-mode is not DISABLED, REQUIRED, VERIFY_CA or VERIFY_IDENTITY"
    )
  ; ( "mysql://u:p@h/db?ssl-fingerprint=" ^ fp
    , Error "ssl-fingerprint is not sha256: and 64 hex digits" )
  ; ( "mysql://u:p@h/db?ssl-fingerprint=sha256:" ^ String.make 63 'a'
    , Error "ssl-fingerprint is not sha256: and 64 hex digits" )
  ; ( "mysql://u:p@h/db?ssl-fingerprint=sha256:" ^ String.make 65 'a'
    , Error "ssl-fingerprint is not sha256: and 64 hex digits" )
  ; ( "mysql://u:p@h/db?ssl-fingerprint=sha256:" ^ String.make 63 'a' ^ "g"
    , Error "ssl-fingerprint is not sha256: and 64 hex digits" )
  ; ( "mysql://u:p@/db?socket=/s&allow-cleartext=tcp"
    , Error "allow-cleartext has only the value socket" )
  ; ( "mysql://u:p@h/db?ssl-mode=DISABLED&kill=maybe"
    , Error "kill is not true or false" )
  ; ("mysql://u:p@h/db?socket=/s", Error "a URL has a host or socket=, not both")
  ; ( "mysql://u:p@/db?socket=/s&ssl-mode=REQUIRED"
    , Error "version 1 has no TLS on a Unix socket" )
  ; ( "mysql://u:p@/db?socket=/s&ssl-ca=/c"
    , Error "version 1 has no TLS on a Unix socket" )
  ; ( "mysql://u:p@/db?socket=/s&ssl-fingerprint=sha256:" ^ fp
    , Error "version 1 has no TLS on a Unix socket" )
  ; ( "mysql://u:p@/db?ssl-mode=DISABLED"
    , Error "the URL has no host and no socket=" )
  ; ( "mysql://u:p@h/db?ssl-mode=DISABLED&allow-cleartext=socket"
    , Error "allow-cleartext=socket is only for socket=" )
  ; ( "mysql://u:p@h/db?ssl-mode=DISABLED&ssl-ca=/c"
    , Error "ssl-mode=DISABLED has no ssl-ca and no ssl-fingerprint" )
  ; ( "mysql://u:p@h/db?ssl-mode=DISABLED&ssl-fingerprint=sha256:" ^ fp
    , Error "ssl-mode=DISABLED has no ssl-ca and no ssl-fingerprint" )
  ; ( "mysql://u:p@h/db?ssl-mode=VERIFY_CA"
    , Error
        "ssl-ca or ssl-fingerprint is necessary for VERIFY_CA and \
         VERIFY_IDENTITY" )
  ; ( "mysql://u:p@h/db?ssl-mode=VERIFY_IDENTITY"
    , Error
        "ssl-ca or ssl-fingerprint is necessary for VERIFY_CA and \
         VERIFY_IDENTITY" )
    (* The rows of the code review of PR 1.1a. *)
  ; ( "mysql://u:p@h/db?ssl-mode=REQUIRED&ssl-ca=/c"
    , Error "ssl-mode=REQUIRED has no ssl-ca: use VERIFY_CA or VERIFY_IDENTITY"
    )
  ; ( "mysql://u:p@h/db?ssl-mode=REQUIRED&ssl-fingerprint=sha256:" ^ fp
    , Ok ("tcp h:3306|u|p|db|REQUIRED ca=- fp=" ^ fp_bytes ^ "|true") )
  ; ( "mysql://u:p@/db?socket=/tmp/a%00/evil"
    , Error "a value has a control character" )
  ; ( "mysql://u:p@h/db?ssl-mode=VERIFY_CA&ssl-ca=/c%00x"
    , Error "a value has a control character" )
  ; ("mysql://u:p@h/db?ssl-ca=%09", Error "a value has a control character")
  ; ( "mysql://u:p%1F@h/db?ssl-mode=DISABLED"
    , Error "a value has a control character" )
  ; ( "mysql://u:p%7F@h/db?ssl-mode=DISABLED"
    , Error "a value has a control character" )
  ; ( "mysql://u%0A:p@h/db?ssl-mode=DISABLED"
    , Error "a value has a control character" )
  ; ( "mysql://u:p@h/d%0D?ssl-mode=DISABLED"
    , Error "a value has a control character" )
  ; ( "mysql://u:p%20q@h/db?ssl-mode=DISABLED"
    , Ok "tcp h:3306|u|p q|db|none|true" )
  ; ( "mysql://u:p%80@h/db?ssl-mode=DISABLED"
    , Ok "tcp h:3306|u|p\x80|db|none|true" )
  ; ( "mysql://u:a@evil/c@db.example?ssl-mode=DISABLED"
    , Error
        "the URL has a raw @ after the host: percent-encode the @ of the \
         password" )
  ; ("mysql://u:p@h/c%40d?ssl-mode=DISABLED", Ok "tcp h:3306|u|p|c@d|none|true")
  ; ("mysql://u:p@[dead]/db?ssl-mode=DISABLED", Error "the host is not valid")
  ; ("mysql://u:p@[1.2.3.4]/db?ssl-mode=DISABLED", Error "the host is not valid")
  ; ("mysql://u:p@[:]/db?ssl-mode=DISABLED", Error "the host is not valid")
  ; ( "mysql://u:p@[fe80::1%25eth0]/db?ssl-mode=DISABLED"
    , Error "the host is not valid" )
  ; ("mysql://u:p@[::]/db?ssl-mode=DISABLED", Ok "tcp :::3306|u|p|db|none|true")
  ; ( "mysql://u:p@[::ffff:1.2.3.4]/db?ssl-mode=DISABLED"
    , Ok "tcp ::ffff:1.2.3.4:3306|u|p|db|none|true" )
  ; ( "mysql://u:p@[1:2:3:4:5:6:7:8]/db?ssl-mode=DISABLED"
    , Ok "tcp 1:2:3:4:5:6:7:8:3306|u|p|db|none|true" )
  ; ( "mysql://u:p@[a:b:c:d:e:f:1.2.3.4]/db?ssl-mode=DISABLED"
    , Ok "tcp a:b:c:d:e:f:1.2.3.4:3306|u|p|db|none|true" )
  ; ( "mysql://u:p@[1:2:3:4:5:6:7::]/db?ssl-mode=DISABLED"
    , Ok "tcp 1:2:3:4:5:6:7:::3306|u|p|db|none|true" )
  ; ( "mysql://u:p@[ABCD::1]/db?ssl-mode=DISABLED"
    , Ok "tcp ABCD::1:3306|u|p|db|none|true" )
  ; ( "mysql://u:p@[1:2:3:4:5:6:7]/db?ssl-mode=DISABLED"
    , Error "the host is not valid" )
  ; ( "mysql://u:p@[1:2:3:4:5:6:7:8:9]/db?ssl-mode=DISABLED"
    , Error "the host is not valid" )
  ; ( "mysql://u:p@[1:2:3:4:5:6:7:8::]/db?ssl-mode=DISABLED"
    , Error "the host is not valid" )
  ; ( "mysql://u:p@[1:2:3:4:5:6:1.2.3.4]/db?ssl-mode=DISABLED"
    , Ok "tcp 1:2:3:4:5:6:1.2.3.4:3306|u|p|db|none|true" )
  ; ( "mysql://u:p@[1:2:3:4:5:6:7:1.2.3.4]/db?ssl-mode=DISABLED"
    , Error "the host is not valid" )
  ; ("mysql://u:p@[1::2::3]/db?ssl-mode=DISABLED", Error "the host is not valid")
  ; ("mysql://u:p@[1:::2]/db?ssl-mode=DISABLED", Error "the host is not valid")
  ; ("mysql://u:p@[:1::]/db?ssl-mode=DISABLED", Error "the host is not valid")
  ; ("mysql://u:p@[12345::]/db?ssl-mode=DISABLED", Error "the host is not valid")
  ; ( "mysql://u:p@[::1.2.3.256]/db?ssl-mode=DISABLED"
    , Error "the host is not valid" )
  ; ("mysql://u:p@[::1.2.3]/db?ssl-mode=DISABLED", Error "the host is not valid")
  ; ( "mysql://u:p@[::1.2.3.]/db?ssl-mode=DISABLED"
    , Error "the host is not valid" )
  ; ( "mysql://u:p@[::1.2.3.1234]/db?ssl-mode=DISABLED"
    , Error "the host is not valid" )
  ; ( "mysql://u:p@[::1.2.3.a]/db?ssl-mode=DISABLED"
    , Error "the host is not valid" )
  ; ( "mysql://u:p@[1.2.3.4::]/db?ssl-mode=DISABLED"
    , Error "the host is not valid" )
  ; ("mysql://u:p@[::1:]/db?ssl-mode=DISABLED", Error "the host is not valid")
  ; ( "mysql://u+v:p+q@h/d+b?ssl-mode=DISABLED"
    , Ok "tcp h:3306|u+v|p+q|d+b|none|true" )
  ; ( "mysql://u:p@h/db?SSL-MODE=DISABLED"
    , Error "the URL has an unknown query key" )
  ; ( "mysql://u:p@h/db?ssl-mode=DISABLED&kill=TRUE"
    , Error "kill is not true or false" )
  ; ( "mysql://u:p@/db?socket=/s&allow-cleartext=SOCKET"
    , Error "allow-cleartext has only the value socket" )
  ; ( "mysql://u:p@h/db?ssl-fingerprint=SHA256:" ^ fp
    , Error "ssl-fingerprint is not sha256: and 64 hex digits" )
  ; ("mysql://u:p@/db?socket=/a=b", Ok "unix /a=b|u|p|db|none|true")
  ; ("mysql://u:p@:3307/db?socket=/s", Error "a URL with socket= has no port")
  ; ("mysql://u:p@:3306/db?socket=/s", Error "a URL with socket= has no port")
  ; ( "mysql://u:p@h/db ?ssl-mode=DISABLED"
    , Error "the URL has a space or a control character" )
  ; ( "mysql://u:p q@h/db?ssl-mode=DISABLED"
    , Error "the URL has a space or a control character" )
  ; ( "mysql://u:p@h/db\n?ssl-mode=DISABLED"
    , Error "the URL has a space or a control character" )
  ; ( "mysql://u:p@h/db?ssl-mode=DISABLED\t"
    , Error "the URL has a space or a control character" )
  ; ( "mysql://u:p@h/db?ssl-mode=DISABLED\x7f"
    , Error "the URL has a space or a control character" )
  ; ( "mysql://u:p@h/db?ssl-mode=DISABLED\x00"
    , Error "the URL has a space or a control character" )
  ; ( "mysql://u:p@h/db?ssl-mode=DISABLED!"
    , Error "ssl-mode is not DISABLED, REQUIRED, VERIFY_CA or VERIFY_IDENTITY"
    )
  ; ( "mysql://u:p@[::255.1.1.1]/db?ssl-mode=DISABLED"
    , Ok "tcp ::255.1.1.1:3306|u|p|db|none|true" )
  ; ( "mysql://u:p@[::1.2.3.0001]/db?ssl-mode=DISABLED"
    , Error "the host is not valid" )
  ; ( "mysql://u:p@[::1:2:3:4:5:6:7]/db?ssl-mode=DISABLED"
    , Ok "tcp ::1:2:3:4:5:6:7:3306|u|p|db|none|true" )
  ; ( "mysql://u:p@[1:2:3:4::5:6:7:8]/db?ssl-mode=DISABLED"
    , Error "the host is not valid" )
  ; ( "mysql://u:p@[1:2:3::4:5:6:7]/db?ssl-mode=DISABLED"
    , Ok "tcp 1:2:3::4:5:6:7:3306|u|p|db|none|true" )
  ; ( "mysql://u:p@[::ffff:01.2.3.4]/db?ssl-mode=DISABLED"
    , Error "the host is not valid" )
  ; ( "mysql://u:p@[::ffff:1.2.3.00]/db?ssl-mode=DISABLED"
    , Error "the host is not valid" )
  ; ( "mysql://u:p@[::ffff:0.2.3.4]/db?ssl-mode=DISABLED"
    , Ok "tcp ::ffff:0.2.3.4:3306|u|p|db|none|true" )
  ; ( "mysql://u:p@[::ffff:10.2.3.4]/db?ssl-mode=DISABLED"
    , Ok "tcp ::ffff:10.2.3.4:3306|u|p|db|none|true" )
  ; ( "mysql://u:p@[::1.2.3.4:5]/db?ssl-mode=DISABLED"
    , Error "the host is not valid" )
  ; ( "mysql://u:p@[::1.2.3.1000]/db?ssl-mode=DISABLED"
    , Error "the host is not valid" )
  ; ( "mysql://u:p@[::1.2.3.99999999999999999999]/db?ssl-mode=DISABLED"
    , Error "the host is not valid" )
  ; ( "mysql://u:p@[::99.2.3.4]/db?ssl-mode=DISABLED"
    , Ok "tcp ::99.2.3.4:3306|u|p|db|none|true" )
  ; ( "mysql://u:p@h/db"
    , Error
        "a TCP connection must have a TLS setting: give ssl-ca, \
         ssl-fingerprint or ssl-mode" ) ]

(* Passwords with each special character, and the URLs that refuse them. No
   message may have a part of 3 characters of the password. *)
let secrets = ["a@b:c/d?e#f%25"; "s3cr3t-pw"; "@@@::://"; "pw!"]

let encode s =
  String.concat ""
    (List.map
       (fun c ->
         if Char.Ascii.is_alphanum c then String.make 1 c
         else Printf.sprintf "%%%02X" (Char.code c) )
       (List.of_seq (String.to_seq s)) )

let parts s = List.init (max 0 (String.length s - 2)) (fun i -> String.sub s i 3)

let contains s sub =
  let n = String.length s and k = String.length sub in
  let rec at i = i + k <= n && (String.sub s i k = sub || at (i + 1)) in
  at 0

let bad_urls pw =
  let e = encode pw in
  [ "mysql://u:" ^ e ^ "@h/db"
  ; "mysql://u:" ^ e ^ "@h/db?x=1"
  ; "mysql://u:" ^ pw ^ "@h/db?ssl-mode=DISABLED"
  ; "mysql://u:" ^ pw ^ "@h:99999/db"
  ; "mysql://u:" ^ e ^ "@/db?ssl-mode=DISABLED"
  ; "mysql://" ^ pw ^ "/db"
  ; "mysql://u:" ^ e ^ "%0@h/db" ]

let tcp ?(port = 3306) host = C.Tcp {host; port}

let sock ?(clear = false) path = C.Socket {path; allow_cleartext= clear}

let fp32 = String.make 32 'x'

let tls ?ca ?fingerprint mode = C.Tls {mode; ca; fingerprint}

(* Each row: the arguments of make, and the result. *)
let makes =
  [ ( "host with a NUL"
    , (fun () ->
        C.make ~tls:C.No_tls ~address:(tcp "127.0.0.1\000x") ~user:"u"
          ~password:"p" ~database:"d" () )
    , Error "a value has a control character" )
  ; ( "socket path with a control byte"
    , (fun () ->
        C.make ~address:(sock "/s\001") ~user:"u" ~password:"p" ~database:"d" () )
    , Error "a value has a control character" )
  ; ( "ca with a NUL"
    , (fun () ->
        C.make
          ~tls:(tls ~ca:"/c\000" Verify_ca)
          ~address:(tcp "h") ~user:"u" ~password:"p" ~database:"d" () )
    , Error "a value has a control character" )
  ; ( "password with a NUL"
    , (fun () ->
        C.make ~address:(sock "/s") ~user:"u" ~password:"p\000" ~database:"d" () )
    , Error "a value has a control character" )
  ; ( "host in brackets"
    , (fun () ->
        C.make ~tls:C.No_tls ~address:(tcp "[::1]") ~user:"u" ~password:"p"
          ~database:"d" () )
    , Error "the host is not valid" )
  ; ( "host with a space"
    , (fun () ->
        C.make ~tls:C.No_tls ~address:(tcp "a b") ~user:"u" ~password:"p"
          ~database:"d" () )
    , Error "the host is not valid" )
  ; ( "host with a port"
    , (fun () ->
        C.make ~tls:C.No_tls ~address:(tcp "h:1") ~user:"u" ~password:"p"
          ~database:"d" () )
    , Error "the host is not valid" )
  ; ( "host with a zone id"
    , (fun () ->
        C.make ~tls:C.No_tls ~address:(tcp "fe80::1%eth0") ~user:"u"
          ~password:"p" ~database:"d" () )
    , Error "the host is not valid" )
  ; ( "IPv6 host"
    , (fun () ->
        C.make ~tls:C.No_tls ~address:(tcp "::1") ~user:"u" ~password:"p"
          ~database:"d" () )
    , Ok "tcp ::1:3306|u|p|d|none|true" )
  ; ( "empty CA path"
    , (fun () ->
        C.make ~tls:(tls ~ca:"" Verify_ca) ~address:(tcp "h") ~user:"u"
          ~password:"p" ~database:"d" () )
    , Error "the CA path is empty" )
  ; ( "REQUIRED with a CA"
    , (fun () ->
        C.make ~tls:(tls ~ca:"/c" Required) ~address:(tcp "h") ~user:"u"
          ~password:"p" ~database:"d" () )
    , Error "ssl-mode=REQUIRED has no ssl-ca: use VERIFY_CA or VERIFY_IDENTITY"
    )
  ; ( "REQUIRED with a fingerprint"
    , (fun () ->
        C.make
          ~tls:(tls ~fingerprint:fp32 Required)
          ~address:(tcp "h") ~user:"u" ~password:"p" ~database:"d" () )
    , Ok ("tcp h:3306|u|p|d|REQUIRED ca=- fp=" ^ hex fp32 ^ "|true") )
  ; ( "socket, no tls"
    , (fun () ->
        C.make ~address:(sock "/s") ~user:"u" ~password:"p" ~database:"d" () )
    , Ok "unix /s|u|p|d|none|true" )
  ; ( "socket, opt-in, kill false"
    , (fun () ->
        C.make ~kill:false ~address:(sock ~clear:true "/s") ~user:"u"
          ~password:"" ~database:"d" () )
    , Ok "unix /s clear|u||d|none|false" )
  ; ( "tcp, no tls given"
    , (fun () ->
        C.make ~address:(tcp "h") ~user:"u" ~password:"p" ~database:"d" () )
    , Error "a TCP address must have a TLS setting" )
  ; ( "tcp, No_tls"
    , (fun () ->
        C.make ~tls:C.No_tls ~address:(tcp ~port:1 "h") ~user:"u" ~password:"p"
          ~database:"d" () )
    , Ok "tcp h:1|u|p|d|none|true" )
  ; ( "tcp, required"
    , (fun () ->
        C.make ~tls:(tls Required) ~address:(tcp ~port:65535 "h") ~user:"u"
          ~password:"p" ~database:"d" () )
    , Ok "tcp h:65535|u|p|d|REQUIRED ca=- fp=-|true" )
  ; ( "tcp, verify with fingerprint"
    , (fun () ->
        C.make
          ~tls:(tls ~fingerprint:fp32 Verify_ca)
          ~address:(tcp "h") ~user:"u" ~password:"p" ~database:"d" () )
    , Ok ("tcp h:3306|u|p|d|VERIFY_CA ca=- fp=" ^ hex fp32 ^ "|true") )
  ; ( "tcp, verify with no ca"
    , (fun () ->
        C.make ~tls:(tls Verify_identity) ~address:(tcp "h") ~user:"u"
          ~password:"p" ~database:"d" () )
    , Error
        "ssl-ca or ssl-fingerprint is necessary for VERIFY_CA and \
         VERIFY_IDENTITY" )
  ; ( "tcp, short fingerprint"
    , (fun () ->
        C.make
          ~tls:(tls ~fingerprint:"x" Required)
          ~address:(tcp "h") ~user:"u" ~password:"p" ~database:"d" () )
    , Error "the fingerprint is not 32 bytes" )
  ; ( "socket with tls"
    , (fun () ->
        C.make ~tls:(tls Required) ~address:(sock "/s") ~user:"u" ~password:"p"
          ~database:"d" () )
    , Error "version 1 has no TLS on a Unix socket" )
  ; ( "empty host"
    , (fun () ->
        C.make ~tls:C.No_tls ~address:(tcp "") ~user:"u" ~password:"p"
          ~database:"d" () )
    , Error "the host is empty" )
  ; ( "port 0"
    , (fun () ->
        C.make ~tls:C.No_tls ~address:(tcp ~port:0 "h") ~user:"u" ~password:"p"
          ~database:"d" () )
    , Error "the port is not in 1 to 65535" )
  ; ( "port 65536"
    , (fun () ->
        C.make ~tls:C.No_tls ~address:(tcp ~port:65536 "h") ~user:"u"
          ~password:"p" ~database:"d" () )
    , Error "the port is not in 1 to 65535" )
  ; ( "empty socket path"
    , (fun () ->
        C.make ~address:(sock "") ~user:"u" ~password:"p" ~database:"d" () )
    , Error "the socket path is empty" )
  ; ( "empty user"
    , (fun () ->
        C.make ~address:(sock "/s") ~user:"" ~password:"p" ~database:"d" () )
    , Error "the user is empty" )
  ; ( "empty database"
    , (fun () ->
        C.make ~address:(sock "/s") ~user:"u" ~password:"p" ~database:"" () )
    , Error "the database is empty" )
  ; ( "NUL in user"
    , (fun () ->
        C.make ~address:(sock "/s") ~user:"u\000" ~password:"p" ~database:"d" () )
    , Error "a value has a control character" )
  ; ( "NUL in database"
    , (fun () ->
        C.make ~address:(sock "/s") ~user:"u" ~password:"p" ~database:"d\000" () )
    , Error "a value has a control character" ) ]

let shown = result string string

let tests =
  group "Config"
    [ cases "of_url" ~id:fst urls (fun (url, expected) ->
          equal shown expected (parse url) )
    ; cases "make"
        ~id:(fun (n, _, _) -> n)
        makes
        (fun (_, got, expected) ->
          equal shown expected (Result.map show (got ())) )
    ; cases "no message has a part of the password" ~id:Fun.id secrets
        (fun pw ->
          List.iter
            (fun url ->
              match C.of_url url with
              | Ok c ->
                  equal string pw (C.password c)
              | Error m ->
                  List.iter
                    (fun part -> is_false ~msg:m (contains m part))
                    (parts pw) )
            (bad_urls pw) )
    ; test "needs_tls" (fun () ->
          let tls url = C.needs_tls (Result.get_ok (C.of_url url)) in
          is_false (tls "mysql://u:p@h/db?ssl-mode=DISABLED") ;
          is_false (tls "mysql://u:p@/db?socket=/s") ;
          is_true (tls "mysql://u:p@h/db?ssl-mode=REQUIRED") )
    ; test "pp hides the password" (fun () ->
          let c =
            Result.get_ok
              (C.of_url "mysql://u:s3cr3t@h:1/db?ssl-ca=/c&kill=false")
          in
          equal string
            "{address = tcp h:1; user = u; password = ***; database = db; tls \
             = VERIFY_IDENTITY ca=/c; kill = false}"
            (Format.asprintf "%a" C.pp c) ;
          let c =
            Result.get_ok
              (C.of_url "mysql://u@/db?socket=/s&allow-cleartext=socket")
          in
          equal string
            "{address = socket /s (allow-cleartext); user = u; password = ; \
             database = db; tls = none; kill = true}"
            (Format.asprintf "%a" C.pp c) ;
          let c =
            Result.get_ok
              (C.of_url
                 ( "mysql://u:p@h/db?ssl-mode=VERIFY_CA&ssl-fingerprint=sha256:"
                 ^ fp ) )
          in
          equal string
            "{address = tcp h:3306; user = u; password = ***; database = db; \
             tls = VERIFY_CA fingerprint; kill = true}"
            (Format.asprintf "%a" C.pp c) ;
          let c =
            Result.get_ok (C.of_url "mysql://u:p@h/db?ssl-mode=REQUIRED")
          in
          equal string
            "{address = tcp h:3306; user = u; password = ***; database = db; \
             tls = REQUIRED; kill = true}"
            (Format.asprintf "%a" C.pp c) ) ]
