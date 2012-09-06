(*---------------------------------------------------------------------------
   Copyright %%COPYRIGHT%%. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(* Trie character maps *)

include Defs.Tset;;

let create () = Array.make 272 nil
let set ~default m u b =
  if b = default then () else
  let sdefault () = String.make 32 (if default then '\xFF' else '\x00') in
  let i = u lsr 12 in
  if Array.length m.(i) = 0 then m.(i) <- Array.make 16 snil;
  let j = u lsr 8 land 0xF in 
  if String.length m.(i).(j) = 0 then m.(i).(j) <- sdefault ();
  let bitnum = u land 0xFF in
  let k = bitnum lsr 3 in
  let l = bitnum land 7 in
  let byte = Char.code m.(i).(j).[k] in
  if b then m.(i).(j).[k] <- Char.unsafe_chr (byte lor (1 lsl l)) else 
  m.(i).(j).[k] <- Char.unsafe_chr (byte land lnot (1 lsl l))

let size = function 
| [||] -> 1
| a -> 
    let size = ref (1 + Array.length a) in
    for i = 0 to Array.length a - 1 do match a.(i) with
    | [||] -> ()
    | a -> 
        size := !size + 1 + Array.length a; 
        for i = 0 to Array.length a - 1 do 
          size := !size + 1 + ((String.length a.(i) * 8) / Sys.word_size)
        done;
    done;
    !size

let pp = Format.fprintf 
let dump ppf = function 
| [||] -> pp ppf "nil"
| a -> 
    pp ppf "@,[|@,";
    for i = 0 to Array.length a - 1 do match a.(i) with 
    | [||] -> pp ppf "@,nil;@," 
    | a -> 
        pp ppf "@,[|@,";
        for j = 0 to Array.length a - 1 do match a.(j) with 
        | "" -> pp ppf "@,snil;@,"
        | s -> 
            pp ppf "@,\"";
            for k = 0 to String.length s - 1 do 
              if k mod 16 = 0 && k > 0 then pp ppf "\\@\n ";
              pp ppf "\\x%02X" (Char.code s.[k])
            done;
            pp ppf "\";@,";
        done;
        pp ppf "|];"
    done;
    pp ppf "|]"

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
