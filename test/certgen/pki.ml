(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

type t =
  { ca: X509.Certificate.t
  ; cert: X509.Certificate.t
  ; key: X509.Private_key.t
  ; other_ca: X509.Certificate.t }

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

(* A self-signed CA with the common name [cn]. *)
let new_ca cn =
  let key = X509.Private_key.generate `P256 in
  let ca =
    sign (request cn key)
      ~extensions:
        X509.Extension.(
          empty
          |> add Basic_constraints (true, (true, None))
          |> add Key_usage (true, [`Key_cert_sign]) )
      key cn
  in
  (ca, key)

let make ?(cn = "localhost") ?(dns = ["localhost"]) ?(ips = ["127.0.0.1"]) () =
  let ca, ca_key = new_ca "cistern test CA" in
  let other_ca, _ = new_ca "cistern other CA" in
  let key = X509.Private_key.generate `P256 in
  (* One entry for each kind: [add] replaces the list of a kind. *)
  let names =
    let dns =
      if dns = [] then X509.General_name.empty
      else X509.General_name.singleton DNS dns
    in
    if ips = [] then dns
    else
      X509.General_name.add IP
        (List.map (fun ip -> Ipaddr.to_octets (Ipaddr.of_string_exn ip)) ips)
        dns
  in
  let extensions =
    X509.Extension.(
      empty
      |> add Ext_key_usage (false, [`Server_auth])
      |> add Basic_constraints (true, (false, None)) )
  in
  let extensions =
    if dns = [] && ips = [] then extensions
    else X509.Extension.add Subject_alt_name (false, names) extensions
  in
  let cert = sign (request cn key) ~extensions ca_key "cistern test CA" in
  {ca; cert; key; other_ca}

let fingerprint cert =
  String.concat ""
    (List.map
       (fun c -> Printf.sprintf "%02x" (Char.code c))
       (List.of_seq (String.to_seq (X509.Certificate.fingerprint `SHA256 cert))) )

let files t =
  [ ("ca.pem", X509.Certificate.encode_pem t.ca)
  ; ("server-cert.pem", X509.Certificate.encode_pem t.cert)
  ; ("server-key.pem", X509.Private_key.encode_pem t.key)
  ; ("server-cert.sha256", fingerprint t.cert)
  ; ("other-ca.pem", X509.Certificate.encode_pem t.other_ca) ]

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
