(*---------------------------------------------------------------------------
   Copyright (c) 2015 The uunf programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
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
   Copyright (c) 2015 The uunf programmers

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
