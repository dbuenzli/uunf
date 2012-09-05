(*---------------------------------------------------------------------------
   Copyright %%COPYRIGHT%%. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(* Diet character maps and their construction. *)

include Defs.Dmap

let of_list l =                        (* map from ordered list of `C and `R *)
  let len = List.length l in 
  let rec aux len l = 
    if len = 1 then match List.hd l with 
    | `R (us, ue, v) -> R (us, ue, v), List.tl l 
    | `C (u, v) -> C (u, v), List.tl l
    else
    let len_ll = len / 2 in 
    let len_rl = len - len_ll in
    let ltree, rlist = aux len_ll l in
    match rlist with
    | [] -> ltree, []
    | b :: r ->
        if len_rl = 1 then 
        match b with 
        | `R (us, ue, v) -> Rn (ltree, Empty, us, ue, v), r
        | `C (u, v) -> Cn (ltree, Empty, u, v), r
        else
        let rtree, rlist = aux (len_rl - 1) r in
        match b with 
        | `R (us, ue, v) -> Rn (ltree, rtree, us, ue, v), rlist
        | `C (u, v) -> Cn (ltree, rtree, u, v), rlist
  in
  if len = 0 then Empty else fst (aux len l)
  
let rec height = function
| Empty -> 0 | R _ | C _ -> 1
| Rn (l, r, _, _, _) | Cn (l, r, _, _) -> 1 + max (height l) (height r)
    
let rec size v_size = function
| Empty -> 1 | C (_, v) -> 3 + v_size v | R (_, _, v) ->  4 + v_size v
| Rn (l, r, _, _, v) -> 6 + size v_size l + size v_size r + v_size v
| Cn (l, r, _, v) -> 5 + size v_size l + size v_size r + v_size v

let pp = Format.fprintf
let rec dump pp_v ppf = function
| Rn (l, r, us, ue, v) -> 
    pp ppf "@,Rn(@,%a,@,%a,@,0x%X,@,0x%X,@,%a)" 
      (dump pp_v) l (dump pp_v) r us ue pp_v v
| Cn (l, r, u, v) ->
    pp ppf "@,Cn(@,%a,@,%a,@,0x%X,@,%a)" (dump pp_v) l (dump pp_v) r u pp_v v
| R (us, ue, v) ->
    pp ppf "@,R(@,0x%X,@,0x%X,@,%a)" us ue pp_v v
| C (u, v) ->
    pp ppf "@,C(@,0x%X,@,%a)" u pp_v v 
| Empty -> 
    pp ppf "@,Empty"

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
