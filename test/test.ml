(*---------------------------------------------------------------------------
   Copyright (c) 2012 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

let uchar_dump ppf u = Format.fprintf ppf "U+%04X" (Uchar.to_int u)

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
module Uset = Set.Make(Uchar)          (* not a diet set, but will do here. *)

let err_test_file () = invalid_arg "test data file invariant violated"

let uchar_of_string v =                            (* parses a scalar value. *)
  let is_hex c = (0x30 <= c && c <= 0x39) || (0x41 <= c && c <= 0x46) in
  let cp = ref 0 in
  for k = 0 to (String.length v) - 1 do
    let c = Char.code v.[k] in
    if not (is_hex c) then failwith "" else
    cp := !cp * 16 + (if c <= 0x39 then c - 48 else c - 55)
  done;
  Uchar.of_int !cp

let uchars_of_string v = List.map uchar_of_string (split_string v ' ')
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
                let test = [| uchars_of_string c1; uchars_of_string c2;
                              uchars_of_string c3; uchars_of_string c4;
                              uchars_of_string c5; |], comment
                in
                let decomps =
                  if not collect_decomps then decomps else
                  match (fst test).(0) with [ uchar ] ->
                    Uset.add uchar decomps
                  | _ -> failwith ""
                in
                loop (test :: tests) collect_decomps decomps
            | _ -> failwith ""
            end
        with Failure _ ->
          log "Unable to parse line:\n`%s'\n" l;
          loop tests collect_decomps decomps
  in
  loop [] false Uset.empty

(* Tests *)

let test_conformance_normalizations tests =
  let nc, nfc = Array.init 5 (fun _ -> Uunf.create `NFC), Array.make 5 [] in
  let nd, nfd = Array.init 5 (fun _ -> Uunf.create `NFD), Array.make 5 [] in
  let nkc, nfkc = Array.init 5 (fun _ -> Uunf.create `NFKC), Array.make 5 [] in
  let nkd, nfkd = Array.init 5 (fun _ -> Uunf.create `NFKD), Array.make 5 [] in
  let rec add n acc v = match Uunf.add n v with
  | `Uchar u -> add n (u :: acc) `Await
  | `Await | `End -> acc
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
    | `Await | `End -> acc
    in
    List.rev (add (add [] (`Uchar u)) `End)
  in
  let check u =
    if Uset.mem u decomps then () else
    begin
      let ul = [u] in
      Uunf.reset nc; Uunf.reset nd; Uunf.reset nkc; Uunf.reset nkd;
      if norm nc u <> ul then
        fail "NFC: %a <> toNFC(%a)" uchar_dump u uchar_dump u;
      if norm nd u <> ul then
        fail "NFD: %a <> toNFD(%a)" uchar_dump u uchar_dump u;
      if norm nkc u <> ul then
        fail "NFKC: %a <> toNFKC(%a)" uchar_dump u uchar_dump u;
      if norm nkd u <> ul then
        fail "NFKD: %a <> toNFKD(%a)" uchar_dump u uchar_dump u;
    end
  in
  (* For each unicode scalar value *)
  let rec loop u =
    if Uchar.equal Uchar.max u then check u else
    (check u; loop (Uchar.succ u))
  in
  loop Uchar.min

let test_others () =
  let test src nf dst =
    let n = Uunf.create nf in
    let rec add acc v = match Uunf.add n v with
    | `Uchar u -> add (u :: acc) `Await
    | `Await | `End -> acc
    in
    let add_uchar acc u = add acc (`Uchar (Uchar.of_int u)) in
    let nseq = List.rev (add (List.fold_left add_uchar [] src) `End) in
    let dst = List.map Uchar.of_int dst in
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
       [0x0041; 0x017A; 0x0335; 0x0327; 0x0324; 0x0041];
  (* found by crowbar *)
  test [0x01C6; 0x032D] `NFKC [0x0064; 0x017E; 0x032D];
  test [0xFF80; 0x1FD3; 0xFF9E; 0x1FD3;] `NFKC [0x30BF; 0x0390; 0x3099; 0x0390];
  (* found again by crowbar *)
  test [0xC100; 0x20D2; 0x11C1; 0x11C1] `NFC [0xC100; 0x20D2; 0x11C1; 0x11C1];
  ()

let test_ccc () =
  assert (Uunf.ccc (Uchar.of_int 0x0020) = 0);
  assert (Uunf.ccc (Uchar.of_int 0x0301) = 230);
  ()

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
    log "Testing Uunf.ccc\n";
    test_ccc ();
    log "Making other tests...\n";
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
   Copyright (c) 2012 Daniel C. Bünzli

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
