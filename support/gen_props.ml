(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(* Extracts data from the Unicode Character Database *)

let str = Format.sprintf
let exec = Filename.basename Sys.executable_name

let ucd_or_die inf = try
  let ic = if inf = "-" then stdin else open_in inf in
  let d = Uucd.decoder (`Channel ic) in
  match Uucd.decode d with
  | `Ok db -> db
  | `Error e ->
      let (l0, c0), (l1, c1) = Uucd.decoded_range d in
      Printf.eprintf "%s:%d.%d-%d.%d: %s\n%!" inf l0 c0 l1 c1 e;
      exit 1
with Sys_error e -> Printf.eprintf "%s\n%!" e; exit 1

let process inf norm =
  let inf = match inf with None -> "support/ucd.xml" | Some inf -> inf in
  let ucd = (Gen.log "Loading Unicode character database.\n"; ucd_or_die inf) in
  let generate pp f ucd = match f with
  | None -> ()
  | Some fn ->
      try
        let oc = if fn = "-" then stdout else Pervasives.open_out fn in
        try
          let ppf = Format.formatter_of_out_channel oc in
          pp ppf ucd;
          Format.pp_print_flush ppf ();
          close_out oc
        with Sys_error _ as e -> close_out oc; raise e
      with Sys_error e -> Printf.eprintf "%s\n%!" e; exit 1
  in
  Gen.log "Note: reported sizes do not take sharing into account.\n";
  generate Gen_norm.pp_mod norm ucd;
  ()

let main () =
  let usage = str
    "Usage: %s [OPTION]... [DBFILE]\n\
     \ Generates data modules from an Unicode character database XML file.\n\
     \ DBFILE defaults to support/ucd.xml\n\
     Options:" exec
  in
  let inf = ref None in
  let set_inf f =
    if !inf = None then inf := Some f else
    raise (Arg.Bad "only one Unicode character database file can be specified")
  in
  let norm = ref None in
  let set r = Arg.String (fun s -> r := Some s) in
  let options = [
    "-norm", set norm, "<FILE> Support for the normalization properties";
  ]
  in
  Arg.parse (Arg.align options) set_inf usage;
  process !inf !norm

let () = main ()

(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli.
   All rights reserved.

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions
   are met:

   1. Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

   2. Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

   3. Neither the name of Daniel C. Bünzli nor the names of
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
  ---------------------------------------------------------------------------*)
