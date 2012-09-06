(*---------------------------------------------------------------------------
   Copyright %%COPYRIGHT%%. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(* Trie character maps *)

include Defs.Tmap

let create () = Array.make l0_size nil
let set ~default m u v =
  if v = default then () else
  let i = u lsr l0_shift in
  if m.(i) == nil then m.(i) <- Array.make l1_size nil;
  let j = u lsr l1_shift land l1_mask in 
  if m.(i).(j) == nil then m.(i).(j) <- Array.make l2_size default;  
  m.(i).(j).(u land l2_mask) <- v

let size v_size  = function 
| [||] -> 1
| a -> 
    let size = ref (1 + Array.length a) in
    for i = 0 to Array.length a - 1 do match a.(i) with
    | [||] -> ()
    | a -> 
        size := !size + (1 + Array.length a); 
        for j = 0 to Array.length a - 1 do match a.(j) with
        | [||] -> ()
        | a -> 
            size := !size + (1 + Array.length a); 
            for k = 0 to Array.length a - 1 do 
              size := !size + v_size a.(k)
            done;
        done;
    done;
    !size

let pp = Format.fprintf 
let dump pr_v ppf = function 
| [||] -> pp ppf "nil"
| a -> 
    pp ppf "@,[|@,";
    for i = 0 to Array.length a - 1 do match a.(i) with 
    | [||] -> pp ppf "@,nil;@," 
    | a -> 
        pp ppf "@,[|@,";
        for j = 0 to Array.length a - 1 do match a.(j) with 
        | [||] -> pp ppf "@,nil;@,"
        | a -> 
            pp ppf "@,[|@,";
            for k = 0 to Array.length a - 1 do pp ppf "@,%a;@," pr_v a.(k) done;
            pp ppf "|];";
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
