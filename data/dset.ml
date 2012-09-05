(*---------------------------------------------------------------------------
   Copyright %%COPYRIGHT%%. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(* Diet character sets and their construction. *)

include Defs.Dset

let of_list l =                        (* set from ordered list of `C and `R *)
  let len = List.length l in 
  let rec aux len l =
    if len = 1 then match List.hd l with 
    | `R (us, ue) -> R (us, ue), List.tl l 
    | `C u -> C u, List.tl l
    else
    let len_ll = len / 2 in 
    let len_rl = len - len_ll in 
    let ltree, rlist = aux len_ll l in 
    match rlist with 
    | [] -> ltree, []
    | b :: r -> 
        if len_rl = 1 then begin 
          match b with 
          | `R (us, ue) -> Rn (ltree, Empty, us, ue), r
          | `C u -> Cn (ltree, Empty, u), r
        end else begin 
          let rtree, rlist = aux (len_rl - 1) r in 
          match b with
          | `R (us, ue) -> Rn (ltree, rtree, us, ue), rlist
          | `C u -> Cn (ltree, rtree, u), rlist
        end
  in
  if len = 0 then Empty else fst (aux len l)
  
let rec height = function 
| Empty -> 0 | R _ | C _ -> 1
| Rn (l, r, _, _) | Cn (l, r, _) -> 1 + max (height l) (height r)

let rec size = function 
| Empty -> 1 | C _ -> 2 | R (_, _) -> 3
| Cn (l, r, _) -> 4 + size l  + size r
| Rn (l, r, _, _) -> 5 + size l + size r

let pp = Format.fprintf 
let rec dump ppf = function
| Rn (l, r, us, ue) -> 
    pp ppf "@,Rn(@,%a,@,%a,@,0x%X,@,0x%X)" dump l dump r us ue
| Cn (l, r, u) ->
    pp ppf "@,Cn(@,%a,@,%a,@,0x%X)" dump l dump r u
| R (us, ue) -> 
    pp ppf "@,R(@,0x%X,@,0x%X)" us ue
| C u -> 
    pp ppf "@,C(@,0x%X)" u
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
