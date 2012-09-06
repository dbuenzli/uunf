(*---------------------------------------------------------------------------
   Copyright %%COPYRIGHT%%. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% version %%VERSION%%
  ---------------------------------------------------------------------------*)

(* Checks that Uunf passes Unicode's Normalization Conformance tests and
   also performs other tests. *)

let split_string s sep =
  let rec split accum j = 
    let i = try (String.rindex_from s j sep) with Not_found -> -1 in
    if (i = -1) then 
      let p = String.sub s 0 (j + 1) in 
      if p <> "" then p :: accum else accum
    else 
    let p = String.sub s (i + 1) (j - i) in
    let accum' = if p <> "" then p :: accum else accum in
    split accum' (i - 1)
  in
  split [] (String.length s - 1)

let log f = Format.eprintf (f ^^ "@?")
let fail fmt = 
  let fail _ = failwith (Format.flush_str_formatter ()) in 
  Format.kfprintf fail Format.str_formatter fmt

(* Conformance data decoding *)

type conformance_test = int list array * string        (* columns + comment. *)
module Cp = struct type t = int let compare : int -> int -> int = compare end
module CpSet = Set.Make(Cp)             (* not a diet set, but will do here. *)

let err_test_file () = invalid_arg "test data file invariant violated"
    
let cp_of_string v =                           (* parses a code point value. *)
  let is_hex c = (0x30 <= c && c <= 0x39) || (0x41 <= c && c <= 0x46) in
  let cp = ref 0 in
  for k = 0 to (String.length v) - 1 do 
    let c = Char.code v.[k] in
    if not (is_hex c) then failwith "" else 
    cp := !cp * 16 + (if c <= 0x39 then c - 48 else c - 55)
  done;
  !cp

let cps_of_string v = List.map cp_of_string (split_string v ' ')
let decode_conformance_data ic =
  let rec loop tests collect_decomps decomps = 
    match try Some (input_line ic) with End_of_file -> None with
    | None -> List.rev tests, decomps
    | Some l ->
        try match split_string l '#' with 
        | "@Part1 " :: _ -> loop tests true decomps
        | "@Part2 " :: _ -> loop tests false decomps
        | p :: _ :: _ when p.[0] = '@' -> loop tests collect_decomps decomps
        | [] | _ :: [] -> loop tests collect_decomps decomps
        | test :: comment :: _ -> 
            begin match split_string test ';' with 
            | c1 :: c2 :: c3 :: c4 :: c5 :: _ -> 
                let test = [| cps_of_string c1; cps_of_string c2;
                              cps_of_string c3; cps_of_string c4;
                              cps_of_string c5; |], comment 
                in
                let decomps = 
                  if not collect_decomps then decomps else
                  match (fst test).(0) with [ cp ] -> CpSet.add cp decomps 
                  | _ -> failwith ""
                in
                loop (test :: tests) collect_decomps decomps
            | _ -> failwith ""
            end
        with Failure _ ->
          log "Unable to parse line:\n`%s'\n" l; 
          loop tests collect_decomps decomps
  in
  loop [] false CpSet.empty

(* Tests *)

let test_conformance_normalizations tests =
  let nc, nfc = Array.init 5 (fun _ -> Uunf.create `NFC), Array.make 5 [] in 
  let nd, nfd = Array.init 5 (fun _ -> Uunf.create `NFD), Array.make 5 [] in 
  let nkc, nfkc = Array.init 5 (fun _ -> Uunf.create `NFKC), Array.make 5 [] in 
  let nkd, nfkd = Array.init 5 (fun _ -> Uunf.create `NFKD), Array.make 5 [] in
  let rec add n acc v = match Uunf.add n v with 
  | `Uchar u -> add n (u :: acc) `Await 
  | `Await -> acc 
  in
  let parallel_add i v =
    nfc.(i) <- add nc.(i) nfc.(i) v; 
    nfd.(i) <- add nd.(i) nfd.(i) v;
    nfkc.(i) <- add nkc.(i) nfkc.(i) v;
    nfkd.(i) <- add nkd.(i) nfkd.(i) v
  in
  let test (cs, comment) = 
    for i = 0 to 4 do 
      Uunf.reset nc.(i); nfc.(i) <- []; 
      Uunf.reset nd.(i); nfd.(i) <- []; 
      Uunf.reset nkc.(i); nfkc.(i) <- []; 
      Uunf.reset nkd.(i); nfkd.(i) <- [];
      List.iter (fun u -> parallel_add i (`Uchar u)) cs.(i);
      parallel_add i `End;
      nfc.(i) <- List.rev nfc.(i); 
      nfd.(i) <- List.rev nfd.(i); 
      nfkc.(i) <- List.rev nfkc.(i); 
      nfkd.(i) <- List.rev nfkd.(i); 
    done;
    if cs.(1) <> nfc.(0) then fail "NFC: c2 <> toNFC(c1) for%s" comment;
    if cs.(1) <> nfc.(1) then fail "NFC: c2 <> toNFC(c2) for%s" comment;
    if cs.(1) <> nfc.(2) then fail "NFC: c2 <> toNFC(c3) for%s" comment;
    if cs.(3) <> nfc.(3) then fail "NFC: c4 <> toNFC(c4) for%s" comment;
    if cs.(3) <> nfc.(4) then fail "NFC: c4 <> toNFC(c5) for%s" comment;
    if cs.(2) <> nfd.(0) then fail "NFD: c3 <> toNFD(c1) for%s" comment;
    if cs.(2) <> nfd.(1) then fail "NFD: c3 <> toNFD(c2) for%s" comment;
    if cs.(2) <> nfd.(2) then fail "NFD: c3 <> toNFD(c3) for%s" comment;    
    if cs.(4) <> nfd.(3) then fail "NFD: c5 <> toNFD(c4) for%s" comment;
    if cs.(4) <> nfd.(4) then fail "NFD: c5 <> toNFD(c5) for%s" comment;
    if cs.(3) <> nfkc.(0) then fail "NFKC: c4 <> toNFKC(c1) for%s" comment;
    if cs.(3) <> nfkc.(1) then fail "NFKC: c4 <> toNFKC(c2) for%s" comment;
    if cs.(3) <> nfkc.(2) then fail "NFKC: c4 <> toNFKC(c3) for%s" comment;
    if cs.(3) <> nfkc.(3) then fail "NFKC: c4 <> toNFKC(c4) for%s" comment;
    if cs.(3) <> nfkc.(4) then fail "NFKC: c4 <> toNFKC(c5) for%s" comment;
    if cs.(4) <> nfkd.(0) then fail "NFKD: c5 <> toNFKD(c1) for%s" comment;
    if cs.(4) <> nfkd.(1) then fail "NFKD: c5 <> toNFKD(c2) for%s" comment;
    if cs.(4) <> nfkd.(2) then fail "NFKD: c5 <> toNFKD(c3) for%s" comment;
    if cs.(4) <> nfkd.(3) then fail "NFKD: c5 <> toNFKD(c4) for%s" comment;
    if cs.(4) <> nfkd.(4) then fail "NFKD: c5 <> toNFKD(c5) for%s" comment;
  in
  List.iter test tests

let test_conformance_non_decomposables decomps =
  let nc = Uunf.create `NFC in 
  let nd = Uunf.create `NFD in 
  let nkc = Uunf.create `NFKC in 
  let nkd = Uunf.create `NFKD in
  let norm n u = 
    let rec add acc v = match Uunf.add n v with 
    | `Uchar u ->  add (u :: acc) `Await 
    | `Await -> acc 
    in
    List.rev (add (add [] (`Uchar u)) `End)
  in
  let check u = 
    if CpSet.mem u decomps then () else 
    begin
      let ul = [u] in
      Uunf.reset nc; Uunf.reset nd; Uunf.reset nkc; Uunf.reset nkd;
      if norm nc u <> ul then fail "NFC: U+%04X <> toNFC(U+%04X)" u u;
      if norm nd u <> ul then fail "NFD: U+%04X <> toNFD(U+%04X)" u u;
      if norm nkc u <> ul then fail "NFKC: U+%04X <> toNFKC(U+%04X)" u u;
      if norm nkd u <> ul then fail "NFKD: U+%04X <> toNFKD(U+%04X)" u u;   
    end
  in
  (* For each unicode scalar value *)
  for u = 0x0000 to 0xD7FF do check u done;
  for u = 0xE000 to 0x10FFFF do check u done

let test_others () =
  let test src nf dst = 
    let n = Uunf.create nf in 
    let rec add acc v = match Uunf.add n v with 
    | `Uchar u -> add (u :: acc) `Await 
    | `Await -> acc 
    in
    let add_uchar acc u = add acc (`Uchar u) in
    let nseq = List.rev (add (List.fold_left add_uchar [] src) `End) in 
    if nseq <> dst then fail "" 
  in
  test [0x1E69] `NFD [0x0073; 0x0323; 0x0307]; 
  test [0x1E69] `NFC [0x1E69];
  test [0x1E0B; 0x0323] `NFD [0x0064; 0x0323; 0x0307]; 
  test [0x1E0B; 0x0323] `NFC [0x1E0D; 0x0307];
  test [0xFB01] `NFD [0xFB01];
  test [0xFB01] `NFC [0xFB01];
  test [0xFB01] `NFKD [0x0066; 0x0069];
  test [0xFB01] `NFKC [0x0066; 0x0069];
  test [0x0032; 0x2075] `NFD [0x0032; 0x2075]; 
  test [0x0032; 0x2075] `NFC [0x0032; 0x2075]; 
  test [0x0032; 0x2075] `NFKD [0x0032; 0x0035]; 
  test [0x0032; 0x2075] `NFKC [0x0032; 0x0035]; 
  test [0x1E9B; 0x0323] `NFD [0x017F; 0x0323; 0x307]; 
  test [0x1E9B; 0x0323] `NFC [0x1E9B; 0x0323; ]; 
  test [0x1E9B; 0x0323] `NFKD [0x0073; 0x0323; 0x0307];
  test [0x1E9B; 0x0323] `NFKC [0x1E69];
  test [0x0041; 0x007A; 0x0335; 0x0327; 0x0324; 0x0301; 0x0041] `NFC
       [0x0041; 0x017A; 0x0335; 0x0327; 0x0324; 0x0041]
    
let test inf = 
  try
    let ok () = log "[DONE]\n" in
    let ic = if inf = "-" then stdin else open_in inf in 
    log "Reading test data from %s... " (if inf = "-" then "stdin" else inf);
    let tests, decomps = decode_conformance_data ic in 
    if inf <> "-" then close_in ic;
    ok (); log "Testing conformance normalization invariants... ";
    test_conformance_normalizations tests; 
    ok (); log "Testing conformance of non-decomposable characters... ";
    test_conformance_non_decomposables decomps; 
    ok (); log "Unicode normalization conformance tests passed!\n";
    log "Making other tests... ";
    test_others ();
    ok (); log "Success!\n"
  with Sys_error e -> log "%s\n" e; exit 1

let main () = 
  let usage = Printf.sprintf
    "Usage: %s [INFILE]\n\
    \ Checks the Unicode normalization conformance test by reading the\n\
    \ test data from INFILE or stdin. The data file is available here: \n\
 \ http://www.unicode.org/Public/%%UNICODEVERSION%%/ucd/NormalizationTest.txt\n\
    Options:" (Filename.basename Sys.executable_name) 
  in
  let inf = ref "-" in 
  let err_inf () = raise (Arg.Bad "only one file can be specified") in
  let set_inf f = if !inf <> "-" then err_inf ()  else inf := f in
  let options = [] in
  Arg.parse (Arg.align options) set_inf usage; 
  test !inf

let () = if (not !Sys.interactive) then main ()

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
