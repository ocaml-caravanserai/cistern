(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

type t =
  {ca: X509.Certificate.t; cert: X509.Certificate.t; key: X509.Private_key.t}

let time s = Option.get (Ptime.of_float_s s)

let valid_from = time 1_577_836_800.

let valid_until = time 2_208_988_800.

let dn cn =
  [ X509.Distinguished_name.(
      Relative_distinguished_name.singleton (CN (Common_name.v cn)) ) ]

(* The signatures cannot fail: the keys are new, and the requests are valid. *)
let sign request ~extensions key issuer =
  match
    X509.Signing_request.sign request ~valid_from ~valid_until ~extensions key
      (dn issuer)
  with
  | Ok c ->
      c
  | Error e ->
      failwith (Fmt.to_to_string X509.Validation.pp_signature_error e)

let request cn key =
  match X509.Signing_request.create (dn cn) key with
  | Ok r ->
      r
  | Error (`Msg m) ->
      failwith m

let make () =
  let ca_key = X509.Private_key.generate `P256 in
  let ca =
    sign
      (request "cistern test CA" ca_key)
      ~extensions:
        X509.Extension.(
          empty
          |> add Basic_constraints (true, (true, None))
          |> add Key_usage (true, [`Key_cert_sign]) )
      ca_key "cistern test CA"
  in
  let key = X509.Private_key.generate `P256 in
  let names =
    X509.General_name.(
      singleton DNS ["localhost"]
      |> add IP [Ipaddr.to_octets (Ipaddr.of_string_exn "127.0.0.1")] )
  in
  let cert =
    sign (request "localhost" key)
      ~extensions:
        X509.Extension.(
          empty
          |> add Subject_alt_name (false, names)
          |> add Ext_key_usage (false, [`Server_auth])
          |> add Basic_constraints (true, (false, None)) )
      ca_key "cistern test CA"
  in
  {ca; cert; key}

let files t =
  [ ("ca.pem", X509.Certificate.encode_pem t.ca)
  ; ("server-cert.pem", X509.Certificate.encode_pem t.cert)
  ; ("server-key.pem", X509.Private_key.encode_pem t.key) ]

(* A new file with the mode [perm]. *)
let write_file path perm text =
  let oc =
    open_out_gen [Open_wronly; Open_creat; Open_excl; Open_binary] perm path
  in
  Fun.protect ~finally:(fun () -> close_out oc) (fun () -> output_string oc text)

(* Remove the files of the directory [tmp] and the directory, after a
   failure. *)
let remove tmp =
  try
    Array.iter (fun f -> Sys.remove (Filename.concat tmp f)) (Sys.readdir tmp) ;
    Sys.rmdir tmp
  with Sys_error _ -> ()

let write ~dir t =
  if Sys.file_exists dir then Error (dir ^ ": the path exists")
  else
    let path = ref (Filename.dirname dir) in
    let tmp = ref None in
    try
      let d =
        Filename.temp_dir ~temp_dir:(Filename.dirname dir)
          (Filename.basename dir ^ ".")
          ""
      in
      tmp := Some d ;
      List.iter
        (fun (name, text) ->
          path := Filename.concat d name ;
          write_file !path
            (if name = "server-key.pem" then 0o600 else 0o644)
            text )
        (files t) ;
      path := dir ;
      Sys.rename d dir ;
      Ok ()
    with Sys_error m ->
      Option.iter remove !tmp ;
      Error (Printf.sprintf "%s: %s" !path m)
