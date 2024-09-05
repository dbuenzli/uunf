(*---------------------------------------------------------------------------
   Copyright (c) 2012 The uunf programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_testing

(* Uunf tests, including Unicode's Normalization Conformance tests *)

let ( let* ) = Result.bind

let uchar_dump ppf u = Format.fprintf ppf "U+%04X" (Uchar.to_int u)

(* Conformance data decoding and tests *)

type conformance_test = int list array * string (* columns + comment. *)
module Uset = Set.Make (Uchar) (* not a diet set, but will do here. *)

let uchar_of_string v = (* parses a scalar value. *)
  let is_hex c = (0x30 <= c && c <= 0x39) || (0x41 <= c && c <= 0x46) in
  let cp = ref 0 in
  for k = 0 to (String.length v) - 1 do
    let c = Char.code v.[k] in
    if not (is_hex c) then failwith "" else
    cp := !cp * 16 + (if c <= 0x39 then c - 48 else c - 55)
  done;
  Uchar.of_int !cp

let uchars_of_string v = List.map uchar_of_string (String.split_on_char ' ' v)

let decode_conformance_data inf =
  Test.log "Reading test data from %s" (if inf = "-" then "stdin" else inf);
  let split_string sep s =
    List.filter (fun s -> s <> "") (String.split_on_char sep s)
  in
  let rec loop tests collect_decomps decomps = function
  | [] -> List.rev tests, decomps
  | l :: ls ->
      try match split_string '#' l with
      | "@Part1 " :: _ -> loop tests true decomps ls
      | "@Part2 " :: _ -> loop tests false decomps ls
      | p :: _ :: _ when p.[0] = '@' -> loop tests collect_decomps decomps ls
      | [] | _ :: [] -> loop tests collect_decomps decomps ls
      | test :: comment :: _ ->
          begin match split_string ';' test with
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
              loop (test :: tests) collect_decomps decomps ls
          | _ -> failwith ""
          end
      with Failure _ ->
        Test.log "Unable to parse line:\n`%s'\n" l;
        loop tests collect_decomps decomps ls
  in
  try
    let ic = if inf = "-" then stdin else In_channel.open_bin inf in
    let finally () = if inf <> "-" then close_in ic in
    let s = Fun.protect ~finally @@ fun () -> In_channel.input_all ic in
    Ok (loop [] false Uset.empty (String.split_on_char '\n' s))
  with Sys_error e -> Error e

let test_conformance_normalizations tests =
  Test.test "conformance normalization invariants" @@ fun () ->
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
    if cs.(1) <> nfc.(0) then Test.fail "NFC: c2 <> toNFC(c1) for%s" comment;
    if cs.(1) <> nfc.(1) then Test.fail "NFC: c2 <> toNFC(c2) for%s" comment;
    if cs.(1) <> nfc.(2) then Test.fail "NFC: c2 <> toNFC(c3) for%s" comment;
    if cs.(3) <> nfc.(3) then Test.fail "NFC: c4 <> toNFC(c4) for%s" comment;
    if cs.(3) <> nfc.(4) then Test.fail "NFC: c4 <> toNFC(c5) for%s" comment;
    if cs.(2) <> nfd.(0) then Test.fail "NFD: c3 <> toNFD(c1) for%s" comment;
    if cs.(2) <> nfd.(1) then Test.fail "NFD: c3 <> toNFD(c2) for%s" comment;
    if cs.(2) <> nfd.(2) then Test.fail "NFD: c3 <> toNFD(c3) for%s" comment;
    if cs.(4) <> nfd.(3) then Test.fail "NFD: c5 <> toNFD(c4) for%s" comment;
    if cs.(4) <> nfd.(4) then Test.fail "NFD: c5 <> toNFD(c5) for%s" comment;
    if cs.(3) <> nfkc.(0) then Test.fail "NFKC: c4 <> toNFKC(c1) for%s" comment;
    if cs.(3) <> nfkc.(1) then Test.fail "NFKC: c4 <> toNFKC(c2) for%s" comment;
    if cs.(3) <> nfkc.(2) then Test.fail "NFKC: c4 <> toNFKC(c3) for%s" comment;
    if cs.(3) <> nfkc.(3) then Test.fail "NFKC: c4 <> toNFKC(c4) for%s" comment;
    if cs.(3) <> nfkc.(4) then Test.fail "NFKC: c4 <> toNFKC(c5) for%s" comment;
    if cs.(4) <> nfkd.(0) then Test.fail "NFKD: c5 <> toNFKD(c1) for%s" comment;
    if cs.(4) <> nfkd.(1) then Test.fail "NFKD: c5 <> toNFKD(c2) for%s" comment;
    if cs.(4) <> nfkd.(2) then Test.fail "NFKD: c5 <> toNFKD(c3) for%s" comment;
    if cs.(4) <> nfkd.(3) then Test.fail "NFKD: c5 <> toNFKD(c4) for%s" comment;
    if cs.(4) <> nfkd.(4) then Test.fail "NFKD: c5 <> toNFKD(c5) for%s" comment;
  in
  List.iter test tests

let test_conformance_non_decomposables decomps =
  Test.test "conformance of non-decomposable characters" @@ fun () ->
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
        Test.fail "NFC: %a <> toNFC(%a)" uchar_dump u uchar_dump u;
      if norm nd u <> ul then
        Test.fail "NFD: %a <> toNFD(%a)" uchar_dump u uchar_dump u;
      if norm nkc u <> ul then
        Test.fail "NFKC: %a <> toNFKC(%a)" uchar_dump u uchar_dump u;
      if norm nkd u <> ul then
        Test.fail "NFKD: %a <> toNFKD(%a)" uchar_dump u uchar_dump u;
    end
  in
  (* For each unicode scalar value *)
  let rec loop u =
    if Uchar.equal Uchar.max u then check u else
    (check u; loop (Uchar.succ u))
  in
  loop Uchar.min

(* Other tests *)

let test_ccc () =
  Test.test "Uunf.ccc" @@ fun () ->
  assert (Uunf.ccc (Uchar.of_int 0x0020) = 0);
  assert (Uunf.ccc (Uchar.of_int 0x0301) = 230);
  ()

let various_norm_tests test =
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

let test_specific () =
  Test.test "specific normalizations" @@ fun () ->
  let test src nf dst =
    let n = Uunf.create nf in
    let rec add acc v = match Uunf.add n v with
    | `Uchar u -> add (u :: acc) `Await
    | `Await | `End -> acc
    in
    let add_uchar acc u = add acc (`Uchar (Uchar.of_int u)) in
    let nseq = List.rev (add (List.fold_left add_uchar [] src) `End) in
    let dst = List.map Uchar.of_int dst in
    if nseq <> dst then Test.fail ""
  in
  various_norm_tests test

let test_uunf_string () =
  Test.test "Uunf_string" @@ fun () ->
  let test enc normalize =
    let b = Buffer.create 42 in
    let enc us =
      let rec loop = function
      | u :: us -> enc b (Uchar.of_int u); loop us
      | [] -> Buffer.contents b
      in
      Buffer.reset b; loop us
    in
    let test src nf dst = assert ((normalize nf (enc src)) = (enc dst)) in
    various_norm_tests test
  in
  test Buffer.add_utf_8_uchar Uunf_string.normalize_utf_8;
  test Buffer.add_utf_16be_uchar Uunf_string.normalize_utf_16be;
  test Buffer.add_utf_16le_uchar Uunf_string.normalize_utf_16le;
  ()

let test_flushing_end_seq () =
  Test.test "flushing end of stream" @@ fun () ->
  let n = Uunf.create `NFKC in
  let uchar u = `Uchar (Uchar.of_int u) in
  if Uunf.add n (uchar 0x2105) <> `Await then Test.fail "";
  if Uunf.add n `Await <> `Await then Test.fail "";
  if Uunf.add n `End <> (uchar 0x0063) then Test.fail "";
  if Uunf.add n `Await <> (uchar 0x002F) then Test.fail "";
  if Uunf.add n `Await <> (uchar 0x006F) then Test.fail "";
  if Uunf.add n `Await <> `End then Test.fail "";
  ()

let test inf =
  Test.main @@ fun () ->
  test_ccc ();
  test_specific ();
  test_uunf_string ();
  test_flushing_end_seq ();
  let skipped = match decode_conformance_data inf with
  | Error e -> Test.fail "%s" e; true
  | Ok (tests, decomps) ->
      test_conformance_normalizations tests;
      test_conformance_non_decomposables decomps;
      false;
  in
  if skipped
  then Test.log "\x1B[33mWarning\x1B[0m: Conformance tests skipped.\n"

let main () =
  let usage = Printf.sprintf
    "Usage: %s [FILE]\n\
    \ Uunf test suite. Checks the Unicode normalization conformance test \n\
    \ by reading the test data from FILE (defaults to \
     test/NormalizationTest.txt)\n\
    Options:" (Filename.basename Sys.executable_name)
  in
  let inf = ref None in
  let err_inf () = raise (Arg.Bad "only one file can be specified") in
  let set_inf f = if !inf <> None then err_inf () else inf := Some f in
  Arg.parse [] set_inf usage;
  let inf = Option.value ~default:"test/NormalizationTest.txt" !inf in
  test inf

let () = if !Sys.interactive then () else exit (main ())
