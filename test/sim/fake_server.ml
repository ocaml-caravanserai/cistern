(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* A fake server for the simulator. It plays a script of packets that the test
   writes with its own encoders (not those of Message), so that one bug cannot
   hide itself. It records each packet that the client sends, and each byte up
   to the end of the stream, also the bytes of a part of a packet. *)

open Weft
module Flow = Weft_flow.Flow

(* Little-endian integers and length-encoded strings (less than 251 bytes). *)
let u16 n = String.init 2 (fun i -> Char.chr ((n lsr (8 * i)) land 0xFF))

let u32 n = String.init 4 (fun i -> Char.chr ((n lsr (8 * i)) land 0xFF))

let lstr s = String.make 1 (Char.chr (String.length s)) ^ s

let frame seq payload =
  let n = String.length payload in
  String.init 3 (fun i -> Char.chr ((n lsr (8 * i)) land 0xFF))
  ^ String.make 1 (Char.chr (seq land 0xFF))
  ^ payload

(* The capabilities of the three pinned servers have these four bits:
   PROTOCOL_41, SECURE_CONNECTION, PLUGIN_AUTH and DEPRECATE_EOF. *)
(* They also have CONNECT_WITH_DB. *)
let caps_mysql = 0xdfffffff

let caps_mariadb = 0x81fffffe

let nonce = "ABCDEFGHIJKLMNOPQRST"

(* A greeting of protocol 10. *)
let greeting ?(protocol = 10) ?(version = "8.4.11") ?(thread = 7)
    ?(caps = caps_mysql) ?(ext = 0) ?(plugin = "mysql_native_password") () =
  String.make 1 (Char.chr protocol)
  ^ version ^ "\x00" ^ u32 thread ^ String.sub nonce 0 8 ^ "\x00"
  ^ u16 (caps land 0xFFFF)
  ^ "\xff" ^ u16 2
  ^ u16 ((caps lsr 16) land 0xFFFF)
  ^ "\x15" ^ String.make 6 '\x00' ^ u32 ext ^ String.sub nonce 8 12 ^ "\x00"
  ^ plugin ^ "\x00"

let ok ?(status = 2) () = "\x00\x00\x00" ^ u16 status ^ "\x00\x00"

let err code msg = "\xff" ^ u16 code ^ "#28000" ^ msg

let terminator = "\xfe\x00\x00\x02\x00\x00\x00"

(* An OK packet (0x00) or a terminator (0xFE) with counts and status flags.
   [affected] and [insert] are 8-byte values. *)
let ok_with ?(header = '\x00') ?(affected = 0L) ?(insert = 0L) ?(status = 2) ()
    =
  let n8 v =
    if Int64.compare v 0L >= 0 && Int64.compare v 251L < 0 then
      String.make 1 (Char.chr (Int64.to_int v))
    else
      "\xfe"
      ^ String.init 8 (fun i ->
          Char.chr
            (Int64.to_int
               (Int64.logand (Int64.shift_right_logical v (8 * i)) 0xFFL) ) )
  in
  String.make 1 header ^ n8 affected ^ n8 insert ^ u16 status ^ "\x00\x00"

let column name =
  "\x03def\x00\x00\x00" ^ lstr name
  ^ "\x00\x0c\x21\x00\x00\x01\x00\x00\xfd\x00\x00\x00\x00\x00"

let row values =
  String.concat ""
    (List.map (function None -> "\xfb" | Some v -> lstr v) values)

(* The 9 values of round trip 1. *)
let session ?(id = "42") ?(version = "8.4.11")
    ?(comment = "Source distribution") ?(max_packet = "67108864") () =
  [ Some id
  ; Some version
  ; Some comment
  ; Some "host1"
  ; Some "3306"
  ; Some "1"
  ; Some "28800"
  ; Some "0"
  ; Some max_packet ]

(* The packets of the reply to round trip 1: count, columns, row, terminator,
   with their ids from 1. *)
let result_set values =
  let n = List.length values in
  String.make 1 (Char.chr n)
  :: List.init n (fun i -> column (Printf.sprintf "c%d" i))
  @ [row values; terminator]

type step =
  | Send of int * string  (** A packet with its sequence id. *)
  | Sends of int * string list  (** Packets with the ids from the first. *)
  | Raw of string  (** Bytes with no frame. *)
  | Recv  (** Read one packet and record it. *)
  | Close  (** Close the connection. *)
  | Reset  (** Reset the connection. *)
  | Hang  (** Wait with no end. *)
  | Wait_ms of int  (** Wait this number of milliseconds. *)
  | Read_bytes of int
      (** Read at least this number of bytes, with no packet: [raw] keeps them.
      *)
  | Start_tls of Tls.Config.server
      (** The TLS handshake of a server (phase 1.4). The steps after it run on
          the TLS flow, and [raw] then keeps the plaintext. The bytes of the
          ClientHello that the server read with the last packet go to the
          handshake. A failed handshake ends the script. *)

(* The packets that the server read, and each byte that it got: [raw] from [pos]
   is the part of a packet that it did not read. *)
type t = {mutable received: (int * string) list; raw: Buffer.t; mutable pos: int}

(* The server end of the stream: the end, a close or a reset. *)
exception End

(* Read until [raw] has [n] bytes from [pos]. *)
let fill server flow n =
  let buf = Bytes.create 4096 in
  while Buffer.length server.raw - server.pos < n do
    match Flow.read flow buf 0 4096 with
    | 0 ->
        raise End
    | k ->
        Buffer.add_subbytes server.raw buf 0 k
    | exception (Flow.Closed | Net.Error _) ->
        raise End
  done

let read_packet server flow =
  fill server flow 4 ;
  let byte i = Char.code (Buffer.nth server.raw (server.pos + i)) in
  let len = byte 0 lor (byte 1 lsl 8) lor (byte 2 lsl 16) and seq = byte 3 in
  fill server flow (4 + len) ;
  let p = Buffer.sub server.raw (server.pos + 4) len in
  server.pos <- server.pos + 4 + len ;
  server.received <- server.received @ [(seq, p)]

(* The count of the sockets of the server that are open, and of the fds of the
   simulator that are not the sockets of the client. *)
let server_sockets = ref 0

let base = ref 0

(* The fds of the client: those of the simulator that are not a listener and not
   a socket of the server. *)
let client_fds sim = Weft_sim.open_fds sim - !base - !server_sockets

let play server flow script =
  (* The flow of the steps: the TCP flow, then the TLS flow. *)
  let current = ref flow in
  let rec go flow = function
    | [] ->
        (* Read up to the end of the stream, so that the client can close. *)
        read_packet server flow ; go flow []
    | Send (seq, p) :: rest ->
        Flow.write_string flow (frame seq p) ;
        go flow rest
    | Sends (seq, ps) :: rest ->
        Flow.write_string flow
          (String.concat "" (List.mapi (fun i p -> frame (seq + i) p) ps)) ;
        go flow rest
    | Raw bytes :: rest ->
        Flow.write_string flow bytes ;
        go flow rest
    | Recv :: rest ->
        read_packet server flow ; go flow rest
    | Close :: _ ->
        ()
    | Reset :: _ ->
        Flow.reset flow
    | Hang :: _ ->
        sync never
    | Wait_ms ms :: rest ->
        Time.sleep Mono (Span.ms ms) ;
        go flow rest
    | Read_bytes n :: rest ->
        fill server flow n ; go flow rest
    | Start_tls config :: rest ->
        let held = Buffer.length server.raw - server.pos in
        let start = Buffer.sub server.raw server.pos held in
        Buffer.truncate server.raw server.pos ;
        let tls, _ = Weft_tls.server config (Flow.prepend start flow) in
        current := tls ;
        go tls rest
  in
  incr server_sockets ;
  Fun.protect
    ~finally:(fun () ->
      Flow.close !current ; Flow.close flow ; decr server_sockets )
    (fun () ->
      match go flow script with
      | () ->
          ()
      | exception (End | Flow.Closed | Net.Error _ | Weft_tls.Handshake_error _)
        ->
          () )

(* [run_raw ?listen ?dns ?extra ?short_io script client]: a simulation with a
   server that listens on [listen] (default "10.0.0.1:3306") and plays [script]
   for the first connection, and the [client] in a fiber. The addresses of
   [extra] listen and never accept. With [short_io], each read and write moves a
   random part of the bytes. It gives the result of [client], the packets that
   the server got and all the bytes that it got. At the end, each fd of the
   simulation must be closed, thus also the socket of the client. *)
let run_raw ?(listen = "10.0.0.1:3306") ?(dns = []) ?(extra = [])
    ?(short_io = false) script client =
  Weft_sim.run (fun sim ->
      List.iter (fun (name, answer) -> Weft_sim.set_dns sim name answer) dns ;
      Weft_sim.net_faults sim {Weft_sim.no_faults with short_io} ;
      let fds = Weft_sim.open_fds sim in
      let server = {received= []; raw= Buffer.create 256; pos= 0} in
      (* "[host]:port" is a TCP address with an IPv6 host, which Weft_sim.addr
         does not read. *)
      let addr a =
        if String.starts_with ~prefix:"[" a then
          let j = String.index a ']' in
          Net.Tcp
            ( String.sub a 1 (j - 1)
            , int_of_string (String.sub a (j + 2) (String.length a - j - 2)) )
        else Weft_sim.addr sim a
      in
      let listener = Weft_flow.Tcp.Listener.listen (addr listen) in
      let others =
        List.map
          (fun a -> Weft_flow.Tcp.Listener.listen (Weft_sim.addr sim a))
          extra
      in
      server_sockets := 0 ;
      base := Weft_sim.open_fds sim ;
      (* The count of the connections after the first one. *)
      let more = ref 0 in
      let result =
        Fiber.scope (fun s ->
            Fiber.fork_daemon s (fun () ->
                let sock, _ = Weft_flow.Tcp.Listener.accept listener in
                Fiber.fork_daemon s (fun () ->
                    play server (Weft_flow.Tcp.flow sock) script ) ;
                (* No test connects two times: a second connection fails the
                   test, also if the client closes it (for example after it sent
                   the password on it). *)
                while true do
                  let sock, _ = Weft_flow.Tcp.Listener.accept listener in
                  incr more ;
                  Flow.close (Weft_flow.Tcp.flow sock)
                done ) ;
            let r = client sim in
            (* Let the server read the last packets of the client. *)
            Time.sleep Mono (Span.ms 1) ;
            r )
      in
      Weft_flow.Tcp.Listener.close listener ;
      List.iter Weft_flow.Tcp.Listener.close others ;
      Windtrap.equal ~msg:"the open fds at the end" Windtrap.int fds
        (Weft_sim.open_fds sim) ;
      Windtrap.equal ~msg:"the connections after the first" Windtrap.int 0 !more ;
      (result, server.received, Buffer.contents server.raw) )

(* [run_raw] with no raw bytes. *)
let run ?listen ?dns ?extra ?short_io script client =
  let result, received, _ =
    run_raw ?listen ?dns ?extra ?short_io script client
  in
  (result, received)

(* The script of a server that accepts the handshake response of the client at
   once, and answers the two round trips. *)
let handshake ?(greet = greeting ()) ?(values = session ()) () =
  [ Send (0, greet)
  ; Recv
  ; Send (2, ok ())
  ; Recv
  ; Sends (1, result_set values)
  ; Recv
  ; Send (1, ok ()) ]
