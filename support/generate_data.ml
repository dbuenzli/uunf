(*---------------------------------------------------------------------------
   Copyright (c) 2015 The uunf programmers. All rights reserved.
   SPDX-License-Identifier: ISC
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

let process inf outf =
  let ucd = (Gen.log "Loading Unicode character database.\n"; ucd_or_die inf) in
  let generate pp outf ucd =
    try
      let oc = if outf = "-" then stdout else open_out outf in
      try
        let ppf = Format.formatter_of_out_channel oc in
        pp ppf ucd;
        Format.pp_print_flush ppf ();
        close_out oc
      with Sys_error _ as e -> close_out oc; raise e
    with Sys_error e -> Printf.eprintf "%s\n%!" e; exit 1
  in
  Gen.log "Note: reported sizes do not take sharing into account.\n";
  generate Gen_norm.pp_mod outf ucd;
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
  let outf = ref None in
  let set r = Arg.String (fun s -> r := Some s) in
  let options = [
    "-o", set outf, "<FILE> output file, defaults to src/uunf_data.ml";
  ]
  in
  Arg.parse (Arg.align options) set_inf usage;
  let inf = match !inf with None -> "support/ucd.xml" | Some inf -> inf in
  let outf = match !outf with None -> "src/uunf_data.ml" | Some outf -> outf in
  process inf outf

let () = main ()
