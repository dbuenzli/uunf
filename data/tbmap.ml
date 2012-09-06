(*---------------------------------------------------------------------------
   Copyright %%COPYRIGHT%%. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(* Trie character boolean maps *)

include Defs.Tbmap;;

let create default = { default; l0 = Array.make l0_size nil }
let set m u b =
  let l2_make m = String.make l2_size (if m.default then '\xFF' else '\x00') in
  if b = m.default then () else
  let i = u lsr l0_shift in
  if m.l0.(i) == nil then m.l0.(i) <- Array.make l1_size snil;
  let j = u lsr l1_shift land l1_mask in 
  if m.l0.(i).(j) == snil then m.l0.(i).(j) <- l2_make m;
  let k = u land l2_mask in
  let byte_num = k lsr 3 (* / 8 *) in
  let bit_num = k land 7 (* mod 8 *) in
  let byte = Char.code m.l0.(i).(j).[byte_num] in
  if b then 
    m.l0.(i).(j).[byte_num] <- Char.unsafe_chr (byte lor (1 lsl bit_num))
  else 
    m.l0.(i).(j).[byte_num] <- Char.unsafe_chr (byte land lnot (1 lsl bit_num))

let size m = match m.l0 with
| [||] -> 3 + 1 
| l0 -> 
    let size = ref (3 + 1 + Array.length l0) in
    for i = 0 to Array.length l0 - 1 do match l0.(i) with
    | [||] -> ()
    | l1 -> 
        size := !size + 1 + Array.length l1; 
        for j = 0 to Array.length l1 - 1 do 
          size := !size + 1 + ((String.length l1.(j) * 8) / Sys.word_size)
        done;
    done;
    !size

let pp = Format.fprintf 
let dump ppf m =
  pp ppf "@,{ default =@ %b;@, l0 =@ " m.default;
  begin match m.l0 with
  | [||] -> pp ppf "nil"
  | l0 -> 
      pp ppf "@,[|@,";
      for i = 0 to Array.length l0 - 1 do match l0.(i) with 
      | [||] -> pp ppf "@,nil;@," 
      | l1 -> 
          pp ppf "@,[|@,";
          for j = 0 to Array.length l1 - 1 do match l1.(j) with 
          | "" -> pp ppf "@,snil;@,"
          | l2 -> 
              pp ppf "@,\"";
              for k = 0 to String.length l2 - 1 do 
                if k mod 16 = 0 && k > 0 then pp ppf "\\@\n ";
                pp ppf "\\x%02X" (Char.code l2.[k])
              done;
              pp ppf "\";@,";
          done;
          pp ppf "|];"
      done;
      pp ppf "|]"
  end;
  pp ppf "@,}"

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
