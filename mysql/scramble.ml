(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

let xor a b =
  String.mapi (fun i c -> Char.chr (Char.code c lxor Char.code b.[i])) a

let sha1 s = Digestif.SHA1.(to_raw_string (digest_string s))

let sha256 s = Digestif.SHA256.(to_raw_string (digest_string s))

let native ~password ~nonce =
  if password = "" then ""
  else
    let h1 = sha1 password in
    xor h1 (sha1 (nonce ^ sha1 h1))

let sha2 ~password ~nonce =
  if password = "" then ""
  else
    let h1 = sha256 password in
    xor h1 (sha256 (sha256 h1 ^ nonce))
