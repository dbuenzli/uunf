(*---------------------------------------------------------------------------
   Copyright (c) 2015 The uunf programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let test_others enc normalize =
  let b = Buffer.create 42 in
  let enc us =
    let rec loop = function
    | u :: us -> enc b (Uchar.of_int u); loop us
    | [] -> Buffer.contents b
    in
    Buffer.reset b; loop us
  in
  let test src nf dst = assert ((normalize nf (enc src)) = (enc dst)) in
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

let main () =
  test_others Uutf.Buffer.add_utf_8 Uunf_string.normalize_utf_8;
  test_others Uutf.Buffer.add_utf_16be Uunf_string.normalize_utf_16be;
  test_others Uutf.Buffer.add_utf_16le Uunf_string.normalize_utf_16le;
  Printf.printf "Uunf_string: All tests passed\n";
  ()

let () = if (not !Sys.interactive) then main ()
