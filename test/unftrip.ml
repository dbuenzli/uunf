(*---------------------------------------------------------------------------
   Copyright 2012 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% version %%VERSION%%
  ---------------------------------------------------------------------------*)

let strf = Printf.sprintf
let pp = Format.fprintf
let pp_pos ppf d = pp ppf "%d.%d:(%d,%06X) "
  (Uutf.decoder_line d) (Uutf.decoder_col d) (Uutf.decoder_count d)
  (Uutf.decoder_byte_count d)

let pp_malformed ppf bs =
  let l = String.length bs in
  pp ppf "@[malformed bytes @[(";
  if l > 0 then pp ppf "%02X" (Char.code (bs.[0]));
  for i = 1 to l - 1 do pp ppf "@ %02X" (Char.code (bs.[i])) done;
  pp ppf ")@]@]"

let exec = Filename.basename Sys.executable_name
let log f = Format.eprintf ("%s: " ^^ f ^^ "@?") exec

let input_malformed = ref false
let log_malformed inf d bs =
  input_malformed := true;
  log "%s:%a: %a@." inf pp_pos d pp_malformed bs

(* Output *)

let uchar_dump ppf = function
| `End -> () | `Uchar u -> pp ppf "%a@\n" Uutf.pp_cp u

let uchar_encoder enc =
  let enc = match enc with `ISO_8859_1 | `US_ASCII -> `UTF_8
  | #Uutf.encoding as enc -> enc
  in
  let e = Uutf.encoder enc (`Channel stdout) in
  fun v -> ignore (Uutf.encode e v)

let out_fun ascii oe =
  if ascii then uchar_dump Format.std_formatter else uchar_encoder oe

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
  | `Uchar cp as u -> out u; add `Await
  | `Await | `End -> ()
  in
  let rec loop d = function
  | `Uchar _ as v -> add v; loop d (Uutf.decode d)
  | `End as v -> add v; out `End
  | `Malformed bs -> log_malformed inf d bs; add u_rep; loop d (Uutf.decode d)
  | `Await -> assert false
  in
  if Uutf.decoder_removed_bom d then add (`Uchar Uutf.u_bom);
  loop d first_dec

let trip nf inf enc ascii =
  try
    let ic = if inf = "-" then stdin else open_in inf in
    let d = Uutf.decoder ?encoding:enc (`Channel ic) in
    let first_dec = Uutf.decode d in            (* guess encoding if needed. *)
    let out = out_fun ascii (Uutf.decoder_encoding d) in
    begin match nf with
    | None -> id inf d first_dec out
    | Some nf -> normalize nf inf d first_dec out
    end;
    if inf <> "-" then close_in ic;
    flush stdout;
  with Sys_error e -> log "%s@." e; exit 1

(* Version *)

let unicode_version () = Format.printf "%s@." Uunf.unicode_version

(* Cmd *)

let do_cmd cmd nf inf enc ascii = match cmd with
| `Unicode_version -> unicode_version ()
| `Trip -> trip nf inf enc ascii

(* Cmdline interface *)

open Cmdliner

let cmd =
  let doc = "Output supported Unicode version." in
  let unicode_version = `Unicode_version, Arg.info ["unicode-version"] ~doc in
  Arg.(value & vflag `Trip [unicode_version])

let nf_doc = "NORMALIZATION"
let nf =
  let docs = nf_doc in
  let doc = "Normalization Form D (NFD), canonical decomposition." in
  let nfd = Some `NFD, Arg.info ["nfd"] ~doc ~docs in
  let doc = "Normalization Form C (NFC), canonical decomposition followed by \
             canonical composition." in
  let nfc = Some `NFC, Arg.info ["nfc"] ~doc ~docs in
  let doc = "Normalization form KD (NFKD), compatibility decomposition." in
  let nfkd = Some `NFKD, Arg.info ["nfkd"] ~doc ~docs in
  let doc = "Normalization form KC (NFKC), compatibility decomposition \
             followed by canonical composition." in
  let nfkc = Some `NFKC, Arg.info ["nfkc"] ~doc ~docs in
  Arg.(value & vflag None [nfd; nfc; nfkd; nfkc])

let file =
  let doc = "The input file. Reads from stdin if unspecified." in
  Arg.(value & pos 0 string "-" & info [] ~doc ~docv:"FILE")

let enc =
  let enc = [ "UTF-8", `UTF_8; "UTF-16", `UTF_16; "UTF-16LE", `UTF_16LE;
              "UTF-16BE", `UTF_16BE; "ASCII", `US_ASCII; "latin1", `ISO_8859_1 ]
  in
  let doc = strf "Input encoding, must %s. If unspecified the encoding is \
                  guessed. The output encoding is the same as the input \
                  encoding except for ASCII and latin1 where UTF-8 is output."
            (Arg.doc_alts_enum enc)
  in
  Arg.(value & opt (some (enum enc)) None & info [ "e"; "encoding" ] ~doc)

let ascii =
  let doc = "Output the input text as newline (U+000A) separated Unicode
             scalar values written in the US-ASCII charset."
  in
  Arg.(value & flag & info ["a"; "ascii"] ~doc)

let cmd =
  let doc = "normalize Unicode text" in
  let man = [
    `S "DESCRIPTION";
    `P "$(tname) inputs Unicode text from stdin and rewrites it to stdout
        according to a specified Unicode normalization form (see UAX 15).";
    `P "If no normalization form is specified the character stream is left
        intact.";
    `P "Invalid byte sequences in the input are reported on stderr and
        replaced by the Unicode replacement character (U+FFFD) in the output.";
    `S nf_doc;
    `S "OPTIONS";
    `S "EXIT STATUS";
    `P "$(tname) exits with one of the following values:";
    `I ("0", "no error occured");
    `I ("1", "a command line parsing error occured");
    `I ("2", "the input text was malformed");
    `S "BUGS";
    `P "This program is distributed with the Uunf OCaml library.
        See http://erratique.ch/software/uunf for contact information." ]
  in
  Term.(pure do_cmd $ cmd $ nf $ file $ enc $ ascii),
  Term.info "unftrip" ~version:"%%VERSION%%" ~doc ~man

let () = match Term.eval cmd with
| `Error _ -> exit 1
| _ -> if !input_malformed then exit 2 else exit 0

(*---------------------------------------------------------------------------
   Copyright 2012 Daniel C. Bünzli
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
