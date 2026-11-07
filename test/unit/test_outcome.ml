(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* Outcome.decide: each row of the table of the plan of phase 1.1, section 3.6,
   for a write and for a command that is not a write. *)

open Windtrap
open Cistern_core
module O = Cistern_mysql.Outcome

let err code = O.Server_error {code; sqlstate= "HY000"; message= "m"}

let state =
  Testable.make
    ~pp:(fun ppf s ->
      Format.pp_print_string ppf
        (match s with O.Usable -> "Usable" | Broken -> "Broken") )
    ~equal:( = )

let unknown = Error.Outcome_unknown `Statement

let lost = Error.Connection_lost

let driver code = Error.Driver {code; sqlstate= "HY000"; message= "m"}

let tests =
  group "Outcome"
    [ cases "the table"
        ~id:(fun (n, _, _, _, _) -> n)
        [ ( "a failed write"
          , Dialect.Mysql
          , O.Write_failed
          , (lost, O.Broken)
          , (lost, O.Broken) )
        ; ( "a write deadline"
          , Dialect.Mysql
          , O.Write_deadline
          , (Error.Timeout, O.Broken)
          , (Error.Timeout, O.Broken) )
        ; ( "a read deadline"
          , Dialect.Mariadb
          , O.Read_deadline
          , (unknown, O.Broken)
          , (Error.Timeout, O.Broken) )
        ; ( "R-SEQ0"
          , Dialect.Mysql
          , O.Before_command
          , (lost, O.Broken)
          , (lost, O.Broken) )
        ; ( "the end of the stream"
          , Dialect.Mariadb
          , O.Stream_ended
          , (unknown, O.Broken)
          , (lost, O.Broken) )
        ; ( "a bad packet, also after the column count"
          , Dialect.Mysql
          , O.Bad_packet "bad"
          , (unknown, O.Broken)
          , (lost, O.Broken) )
        ; ( "LOCAL INFILE"
          , Dialect.Mysql
          , O.Local_infile
          , (Error.Decode "LOCAL INFILE request", O.Broken)
          , (Error.Decode "LOCAL INFILE request", O.Broken) )
        ; ( "1053"
          , Dialect.Mysql
          , err 1053
          , (unknown, O.Broken)
          , (lost, O.Broken) )
        ; ( "4031 on MySQL"
          , Dialect.Mysql
          , err 4031
          , (unknown, O.Broken)
          , (lost, O.Broken) )
        ; ( "1927 on MariaDB"
          , Dialect.Mariadb
          , err 1927
          , (unknown, O.Broken)
          , (lost, O.Broken) )
        ; ( "4031 on MariaDB is another error"
          , Dialect.Mariadb
          , err 4031
          , (driver 4031, O.Usable)
          , (driver 4031, O.Usable) )
        ; ( "1317"
          , Dialect.Mysql
          , err 1317
          , (driver 1317, O.Usable)
          , (driver 1317, O.Usable) )
        ; ( "1153"
          , Dialect.Mysql
          , err 1153
          , (Error.Invalid_value {code= Some 1153; message= "m"}, O.Broken)
          , (Error.Invalid_value {code= Some 1153; message= "m"}, O.Broken) )
        ; ( "another ERR"
          , Dialect.Mariadb
          , err 1062
          , (Error.Constraint_violation {kind= Unique; message= "m"}, O.Usable)
          , (Error.Constraint_violation {kind= Unique; message= "m"}, O.Usable)
          )
        ; ( "a value that does not decode"
          , Dialect.Mysql
          , O.Bad_value "column 0"
          , (Error.Decode "column 0", O.Usable)
          , (Error.Decode "column 0", O.Usable) ) ]
        (fun (_, d, event, write, not_write) ->
          equal (pair Fixture.error state) write (O.decide d O.Write event) ;
          equal (pair Fixture.error state) not_write
            (O.decide d O.Not_write event) ) ]
