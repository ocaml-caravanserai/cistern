(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* A fake server of many connections, for the tests of the pool. Each connection
   gets the handshake, then a small interpreter of test SQL. "SELECT n" gives
   one INT column and one row with n. "SLEEP ms" gives an OK after ms
   milliseconds. "FAIL code" gives an ERR with the code. "BEGIN" gives an OK
   with SERVER_STATUS_IN_TRANS. "INSERT" gives an OK with 2 affected rows and
   the insert id 7. "DEAF" gives an OK, then the server sends the OK of the next
   command without a read of it, and reads nothing for 3 s: a test fills the
   receive buffer of the server with that next command. Another text gives an OK
   with 1 affected row.

   COM_PING, COM_INIT_DB and COM_RESET_CONNECTION give an OK, COM_QUIT ends the
   connection. The faults are mutable, so that a test can change them in its
   run. The server counts the connections, the handshakes in progress, the
   commands, the resets and the COM_QUIT packets. It uses the encoders of
   Fake_server. *)

open Weft
module F = Fake_server
module Flow = Weft_flow.Flow
module Reader = Weft_flow.Reader

type faults =
  { mutable refuse: bool  (** Accept, then close at once. *)
  ; mutable hang_greeting: bool  (** Send no greeting. *)
  ; mutable latency_ms: int  (** Before each reply of a command. *)
  ; mutable reset_after: int option
        (** Reset the connection after this number of commands. *)
  ; mutable idle_4031_ms: int option
        (** After this idle time, send 4031 with the id 0, then close. *)
  ; mutable fail_reset: int option
        (** COM_RESET_CONNECTION gives an ERR with this code. *)
  ; mutable reset_ms: int  (** Before the reply of COM_RESET_CONNECTION. *)
  ; mutable wait_timeout: int  (** The [@@wait_timeout] of round trip 1. *) }

type t =
  { faults: faults
  ; mutable open_now: int
  ; mutable open_max: int
  ; mutable handshaking: int
  ; mutable handshaking_max: int
  ; mutable connects: int
  ; mutable commands: int
  ; mutable resets: int
  ; mutable quits: int
  ; mutable log: string list  (** The commands, the newest first. *) }

let no_faults () =
  { refuse= false
  ; hang_greeting= false
  ; latency_ms= 0
  ; reset_after= None
  ; idle_4031_ms= None
  ; fail_reset= None
  ; reset_ms= 0
  ; wait_timeout= 28800 }

(* An INT column with the binary set. *)
let int_column name =
  "\x03def\x00\x00\x00" ^ F.lstr name
  ^ "\x00\x0c\x3f\x00\x0b\x00\x00\x00\x03\x00\x00\x00\x00\x00"

let read_packet r =
  Reader.ensure r 4 ;
  let len = Reader.get_uint24_le r 0 in
  Reader.consume r 4 ;
  Reader.ensure r len ;
  let p = Reader.sub_string r 0 len in
  Reader.consume r len ; p

let send flow seq p = Flow.write_string flow (F.frame seq p)

let sends flow seq ps =
  Flow.write_string flow
    (String.concat "" (List.mapi (fun i p -> F.frame (seq + i) p) ps))

let session db =
  List.mapi
    (fun i v -> if i = 6 then Some (string_of_int db.faults.wait_timeout) else v)
    (F.session ())

exception Stop

let int_of s = try Some (int_of_string (String.trim s)) with _ -> None

let reply flow sql =
  let word, rest =
    match String.index_opt sql ' ' with
    | Some i ->
        (String.sub sql 0 i, String.sub sql (i + 1) (String.length sql - i - 1))
    | None ->
        (sql, "")
  in
  match (word, int_of rest) with
  | "SELECT", Some n ->
      sends flow 1
        ( F.result_set [Some (string_of_int n)]
        |> fun l ->
        match l with
        | count :: _ :: rest ->
            count :: int_column "n" :: rest
        | l ->
            l )
  | "SLEEP", Some ms ->
      Time.sleep Mono (Span.ms ms) ;
      send flow 1 (F.ok ())
  | "FAIL", Some code ->
      send flow 1 (F.err code "fail")
  | "BEGIN", _ ->
      send flow 1 (F.ok_with ~status:3 ())
  | "INSERT", _ ->
      send flow 1 (F.ok_with ~affected:2L ~insert:7L ())
  | "DEAF", _ ->
      (* The OK, and 5 ms later the OK of the next command, which the server
         never reads. Then the server reads nothing for 3 s, and closes. *)
      send flow 1 (F.ok ()) ;
      Time.sleep Mono (Span.ms 5) ;
      send flow 1 (F.ok_with ~affected:1L ()) ;
      Time.sleep Mono (Span.s 3) ;
      raise Stop
  | _ ->
      send flow 1 (F.ok_with ~affected:1L ())

let command db flow r =
  let p =
    match db.faults.idle_4031_ms with
    | Some ms -> (
      match Fiber.with_timeout Mono (Span.ms ms) (fun () -> read_packet r) with
      | p ->
          p
      | exception Fiber.Timeout ->
          send flow 0 (F.err 4031 "idle") ;
          raise Stop )
    | None ->
        read_packet r
  in
  db.commands <- db.commands + 1 ;
  if db.faults.latency_ms > 0 then
    Time.sleep Mono (Span.ms db.faults.latency_ms) ;
  let body = String.sub p 1 (String.length p - 1) in
  db.log <- body :: db.log ;
  match p.[0] with
  | '\x01' ->
      db.quits <- db.quits + 1 ;
      raise Stop
  | '\x0e' | '\x02' ->
      send flow 1 (F.ok ())
  | '\x1f' -> (
      db.resets <- db.resets + 1 ;
      if db.faults.reset_ms > 0 then
        Time.sleep Mono (Span.ms db.faults.reset_ms) ;
      match db.faults.fail_reset with
      | Some code ->
          send flow 1 (F.err code "reset")
      | None ->
          send flow 1 (F.ok ()) )
  | '\x03' ->
      reply flow body
  | _ ->
      send flow 1 (F.err 1047 "unknown command")

let serve db flow =
  db.connects <- db.connects + 1 ;
  db.open_now <- db.open_now + 1 ;
  db.open_max <- Int.max db.open_max db.open_now ;
  db.handshaking <- db.handshaking + 1 ;
  db.handshaking_max <- Int.max db.handshaking_max db.handshaking ;
  let handshake_done = ref false in
  let finish_handshake () =
    if not !handshake_done then (
      handshake_done := true ;
      db.handshaking <- db.handshaking - 1 )
  in
  let r = Reader.create flow in
  Fun.protect
    ~finally:(fun () ->
      finish_handshake () ;
      Flow.close flow ;
      db.open_now <- db.open_now - 1 )
    (fun () ->
      match
        if db.faults.refuse then raise Stop ;
        if db.faults.hang_greeting then sync never ;
        send flow 0 (F.greeting ()) ;
        ignore (read_packet r) ;
        send flow 2 (F.ok ()) ;
        ignore (read_packet r) ;
        sends flow 1 (F.result_set (session db)) ;
        ignore (read_packet r) ;
        send flow 1 (F.ok ()) ;
        finish_handshake () ;
        let n = ref 0 in
        while true do
          command db flow r ;
          incr n ;
          match db.faults.reset_after with
          | Some k when !n >= k ->
              Flow.reset flow ; raise Stop
          | _ ->
              ()
        done
      with
      | () ->
          ()
      | exception (Fiber.Cancelled _ as e) ->
          raise e
      | exception _ ->
          () )

let address = "10.0.0.9:3306"

let config ?(password = "pw") () =
  Result.get_ok
    (Cistern_mysql.Config.make ~tls:Cistern_mysql.Config.No_tls
       ~address:(Cistern_mysql.Config.Tcp {host= "10.0.0.9"; port= 3306})
       ~user:"u" ~password ~database:"db" () )

(* [run ?domains ?seed f]: a simulation with the fake server, and [f] in the
   main fiber. At the end each fd of the simulation must be closed. *)
let run ?(domains = 1) ?(seed = 0) f =
  Weft_sim.run ~domains ~seed (fun sim ->
      let fds = Weft_sim.open_fds sim in
      let db =
        { faults= no_faults ()
        ; open_now= 0
        ; open_max= 0
        ; handshaking= 0
        ; handshaking_max= 0
        ; connects= 0
        ; commands= 0
        ; resets= 0
        ; quits= 0
        ; log= [] }
      in
      let listener =
        Weft_flow.Tcp.Listener.listen (Weft_sim.addr sim address)
      in
      let result =
        Fiber.scope (fun s ->
            Fiber.fork_daemon s (fun () ->
                while true do
                  let sock, _ = Weft_flow.Tcp.Listener.accept listener in
                  let flow = Weft_flow.Tcp.flow sock in
                  (* At the end of the run, the fork raises: close the socket
                     that the accept gave. *)
                  match Fiber.fork_daemon s (fun () -> serve db flow) with
                  | () ->
                      ()
                  | exception e ->
                      Flow.close flow ; raise e
                done ) ;
            let r = f sim db in
            Time.sleep Mono (Span.ms 1) ;
            r )
      in
      Weft_flow.Tcp.Listener.close listener ;
      (* With more than one virtual domain, an fd closes when each domain that
         has a registration of it drops it: let the domains run first. *)
      Time.sleep Mono (Span.ms 1) ;
      Windtrap.equal ~msg:"the open fds at the end" Windtrap.int fds
        (Weft_sim.open_fds sim) ;
      result )
