(*---------------------------------------------------------------------------
   Copyright %%COPYRIGHT%%. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% version %%VERSION%%
  ---------------------------------------------------------------------------*)

let pp = Format.fprintf
let pp_pos ppf d = pp ppf "%d.%d:(%d) " 
  (Uutf.decoder_line d) (Uutf.decoder_col d) (Uutf.decoder_count d)

let pp_malformed ppf bs = 
  let l = String.length bs in 
  pp ppf "@[malformed bytes @[("; 
  if l > 0 then pp ppf "%02X" (Char.code (bs.[0])); 
  for i = 1 to l - 1 do pp ppf "@ %02X" (Char.code (bs.[i])) done; 
  pp ppf ")@]@]"

let exec = Filename.basename Sys.executable_name
let log f = Format.eprintf ("%s: " ^^ f ^^ "@?") exec 
let log_malformed inf d bs = log "%s:%a: %a@." inf pp_pos d pp_malformed bs

(* Output *)

let uchar_dump ppf = function 
| `End -> () | `Uchar u -> pp ppf "%a@\n" Uutf.pp_cp u

let uchar_encoder enc =
  let enc = match enc with `ISO_8859_1 | `US_ASCII -> `UTF_8 
  | #Uutf.encoding as enc -> enc
  in
  let e = Uutf.encoder enc (`Channel stdout) in
  fun v -> ignore (Uutf.encode e v)

let out_fun dump oe = 
  if dump then uchar_dump Format.std_formatter else uchar_encoder oe 

(* Trip *)

let u_rep = `Uchar Uutf.u_rep 
let id inf d first_dec out =                            (* no normalization. *)
  let rec loop d = function
  | `Uchar _ as v -> out v; loop d (Uutf.decode d)
  | `End as v -> out v
  | `Malformed bs -> log_malformed inf d bs; out u_rep; loop d (Uutf.decode d)
  | `Await -> assert false
  in
  if Uutf.decoder_removed_bom d then out (`Uchar Uutf.u_bom);
  loop d first_dec

let normalize nf inf d first_dec out =                   (* normalize to nf. *)
  let n = Uunf.create nf in
  let rec add v = match Uunf.add n v with 
  | `Uchar cp as u -> out u; add `Await | `Await -> () 
  in
  let rec loop d = function 
  | `Uchar _ as v -> add v; loop d (Uutf.decode d)
  | `End as v -> add v; out `End
  | `Malformed bs -> log_malformed inf d bs; add u_rep; loop d (Uutf.decode d)
  | `Await -> assert false
  in
  if Uutf.decoder_removed_bom d then add (`Uchar Uutf.u_bom);
  loop d first_dec

let trip nf dump inf ienc = 
  try
    let ic = if inf = "-" then stdin else open_in inf in 
    let d = Uutf.decoder ?encoding:ienc (`Channel ic) in
    let first_dec = Uutf.decode d in            (* guess encoding if needed. *)
    let out = out_fun dump (Uutf.decoder_encoding d) in
    begin match nf with 
    | None -> id inf d first_dec out 
    | Some nf -> normalize nf inf d first_dec out 
    end;
    if inf <> "-" then close_in ic;
    flush stdout;
  with Sys_error e -> log "%s@." e; exit 1

(* Random *)

let u_surrogate_count = 0xDFFF - 0xD800 + 1
let uchar_count = (0x10FFFF + 1) - u_surrogate_count
let r_uchar () = 
  let n = Random.int uchar_count in
  if n > 0xD7FF then `Uchar (n + u_surrogate_count) else `Uchar n

let rec r_id out rcount = 
  if rcount = 0 then out `End else (out (r_uchar ()); r_id out (rcount - 1))

let r_normalize nf out rcount = 
  let n = Uunf.create nf in 
  let rec add v = match Uunf.add n v with 
  | `Uchar cp as u -> out u; add `Await | `Await -> () 
  in
  let rec loop rcount =
    if rcount = 0 then (add `End; out `End) else 
    (add (r_uchar ()); loop (rcount - 1))
  in
  loop rcount
  
let random nf dump rseed rcount = 
  log "Normalizing %d random characters with seed %d\n" rcount rseed; 
  Random.init rseed; 
  let out = out_fun dump `UTF_8 in 
  begin match nf with 
  | None -> r_id out rcount 
  | Some nf -> r_normalize nf out rcount
  end; 
  flush stdout

(* Version *)

let version () = Format.printf 
  "%s %%VERSION%% @\nUnicode %s@." exec Uunf.unicode_version

(* Main *)

let main () = 
  let usage = Printf.sprintf 
    "Usage: %s [OPTION]... [INFILE]\n\
    \ Normalizes Unicode text from stdin to stdout.\n\
    \ If no input encoding is specified, it is guessed. If no normalization\n\
    \ is specified NFC is used.\n\
         Options:" exec
  in
  let cmd = ref `Trip in
  let set_cmd v () = cmd := v in
  let inf = ref "-" in
  let err_inf () = raise (Arg.Bad "only one file can be specified") in
  let set_inf f = if !inf <> "-" then err_inf () else inf := f in
  let ienc = ref None in 
  let ienc_fun enc = match Uutf.encoding_of_string enc with
  | Some enc -> ienc := Some enc 
  | None -> log "unsupported@ input@ encoding '%s',@ trying to guess.@." enc
  in
  let nf = ref (Some `NFC) in 
  let nf_fun v = match String.lowercase v with 
  | "nfc" -> nf := (Some `NFC) | "nfd" -> nf := (Some `NFD)
  | "nfkc" -> nf := (Some `NFKC) | "nfkd" -> nf := (Some `NFKD)
  | "none" -> nf := None
  | v -> log "unsupported@ normalization@ form@ `%s',@ using NFC.@." v
  in
  let dump = ref false in
  let rseed = ref (Random.self_init (); Random.int (1 lsl 30 - 1)) in
  let rcount = ref 1_000_000 in
  let nat s r v = if v > 0 then r := v else log "%s must be > 0, ignored\n" s in
  let options = [
    "-version", Arg.Unit (set_cmd `Version),
    " Program and Unicode version";
    "-dump", Arg.Set dump, 
    " Dump normalized scalar values in ASCII, one per line";
    "-enc", Arg.String ienc_fun,
    "<enc> Input encoding: UTF-8, UTF-16, UTF-16BE, UTF-16LE, ASCII or latin1";
    "-nf", Arg.String nf_fun, 
    "<nf> Normal form: NFC, NFD, NFKC, NFKD or none (no normalization)";
    "-r", Arg.Unit (set_cmd `Random), " Generate random characters.";
    "-rseed", Arg.Int (nat "-rseed" rseed), "<int> Random seed"; 
    "-rcount", Arg.Int (nat "-rcount" rcount), 
    "<int> Number of random characters to generate";]
  in
  Arg.parse (Arg.align options) set_inf usage; 
  match !cmd with 
  | `Trip -> trip !nf !dump !inf !ienc 
  | `Random -> random !nf !dump !rseed !rcount 
  | `Version -> version ()

let () = main ()

(*---------------------------------------------------------------------------
   Copyright %%COPYRIGHT%%
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
