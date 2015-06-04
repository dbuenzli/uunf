(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

let test_others enc normalize =
  let b = Buffer.create 42 in
  let enc us =
    let rec loop = function
    | u :: us -> enc b u; loop us
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
  Printf.printf "All tests passed";
  ()

let () = if (not !Sys.interactive) then main ()

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
