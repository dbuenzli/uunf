(*---------------------------------------------------------------------------
   Copyright %%COPYRIGHT%%. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

let invalid_add () = invalid_arg "expected `Await add"

(* The normalization process is implemented as described in UAX #15
   section 9.1 for normalizing the concatenation of normalized
   strings.  We detect ranges of characters in the input sequence
   enclosed between two characters for which NFX_quick_check=YES *and*
   ccc = 0 (6.1.0 wrongly claims that quick_check=YES implies ccc = 0,
   we therefore call this property nfx_boundary).  Only these ranges
   (including the left boundary) need to be bufferized to perform the
   normalization process. *)

(* Characters *)

type uchar = int
let ux_eoi = max_int                 (* end of input, outside unicode range. *)
let ux_soi = ux_eoi - 1            (* start of input, outside unicode range. *)
let ux_none = ux_soi - 2                  (* no char, outside unicode range. *)
let is_scalar_value i = 
  (0x0000 <= i && i <= 0xD7FF) || (0xE000 <= i && i <= 0x10FFFF)

(* Normalization properties. *)

let unicode_version = "%%UNICODEVERSION%%"

(* START GENERATED DATA, DO NOT EDIT *)
module Data = struct
(* %%UNICODESUPPRESS%% *) let err () = invalid_arg "stub data, do ./build data"
(* %%UNICODESUPPRESS%% *) let nfc_boundary u = err ()
(* %%UNICODESUPPRESS%% *) let nfd_boundary u = err ()
(* %%UNICODESUPPRESS%% *) let nfkc_boundary u = err ()
(* %%UNICODESUPPRESS%% *) let nfkd_boundary u = err ()
(* %%UNICODESUPPRESS%% *) let ccc u = err ()
(* %%UNICODESUPPRESS%% *) let decomp u = err () 
(* %%UNICODESUPPRESS%% *) let compose u = err () 
(* %%UNICODESUPPRESS%% *) include Data
(* %%UNICODEDATA%% *)
end
(* END GENERATED DATA *)

let nfc_boundary u = Data.nfc_boundary u 
let nfd_boundary u = Data.nfd_boundary u
let nfkc_boundary u = Data.nfkc_boundary u
let nfkd_boundary u = Data.nfkd_boundary u
let ccc = Data.ccc

module H = struct                            (* Hangul arithmetic constants. *)
  let sbase = 0xAC00
  let lbase = 0x1100
  let vbase = 0x1161 
  let tbase = 0x11A7
  let scount = 11172
  let lcount = 19
  let vcount = 21
  let tcount = 28 
  let ncount = 588 
  let scount = 11172 
end

let decomp u = 
  if u < 0xAC00 || 0xD7A3 < u then Data.decomp u else
  begin                                        (* LV or LVT hangul composite *)
    let sindex = u - H.sbase in 
    let l = H.lbase + (sindex / H.ncount) in 
    let v = H.vbase + (sindex mod H.ncount) / H.tcount in 
    let t = H.tbase + (sindex mod H.tcount) in 
    if t = H.tbase then [|l; v|] else [|l; v; t|]
  end

(* N.B. to help stream-safe text implementers we *could* use the bits
   25-27 of [(decomp u).(0)] to indicate the number of initial non
   starters in the NFKD decomposition of [u] and bits and 28-30 to
   indicate the non starter count increment. *)

let d_compatibility i = i land (1 lsl 24) > 0
let d_uchar i = i land 0x1FFFFF

let _composite u1 u2 =
  if 0x1100 <= u1 && u1 <= 0x1112 then 
    begin
      if u2 < 0x1161 || 0x1175 < u2 then ux_none else
      let l = u1 - H.lbase in                         (* LV hangul composite *)
      let v = u2 - H.vbase in 
      H.sbase + l * H.ncount + v * H.tcount
    end 
  else if 0xAC00 <= u1 && u1 <= 0xD788 && (u1 - 0x0AC00) mod H.tcount = 0 then 
    begin 
      if u2 < 0x11A8 || u2 > 0x11C3 then ux_none else 
      (u1 + u2 - H.tbase)                            (* LVT hangul composite *)
    end
  else match Data.compose u1 with 
  | [||] -> ux_none
  | a (* [u2; c; u2'; c'; ...] sorted *) ->
      let len = Array.length a / 2 in 
      let i = ref 0 in
      try 
        while (!i < len) do 
          if a.(!i * 2) = u2 then raise Exit else incr i;
        done; 
        ux_none
      with Exit -> (a.(!i * 2 + 1))

let composite u1 u2 =
  let u = _composite u1 u2 in 
  if u = ux_none then None else Some u

(* Normalize *)

type ret = [`Await | `Uchar of uchar ]
type form = [ `NFC | `NFD | `NFKC | `NFKD ] 
type state =                                            (* normalizer state. *)
| Start                                                   (* no cp seen yet. *)
| Boundary    (* cp with boundary = true found in n.uc, no accumulation yet. *)
| Acc                      (* accumulate until next cp with boundary = true. *)
| Flush          (* next cp with boundary = true found, flush previous data. *)
| End                                      (* end of normalization sequence. *)
    
type t = 
  { form : form;                                      (* normalization form. *)
    compat : bool;            (* true if compatibility decomposition needed. *)
    compose : bool;                           (* true if composition needed. *)
    boundary : uchar -> bool;                               (* nfx_boundary. *)
    mutable state : state;                              (* normalizer state. *)
    mutable uc : [`Uchar of uchar];         (* last cp with boundary = true. *)
    mutable acc : int array;                      (* code point accumulator. *)
    mutable first : int;                (* index of first code point in acc. *)
    mutable last : int; }                (* index of last code point in acc. *)
    
let create_acc () = Array.create 35 ux_eoi
let create form  =
  let boundary, compat, compose = match form with 
  | `NFC -> nfc_boundary, false, true
  | `NFD -> nfd_boundary, false, false
  | `NFKC -> nfkc_boundary, true, true
  | `NFKD -> nfkd_boundary, true, false
  in
  { form = (form :> form); compat; compose; boundary; state = Start;
    uc = `Uchar ux_soi; acc = create_acc (); first = 0; last = -1; }

let is_end n = let `Uchar u = n.uc in u = ux_eoi
let get_u n = let `Uchar u = n.uc in u
let acc_empty n = n.first > n.last
let form n = n.form
let copy n = { n with acc = Array.copy n.acc }
let reset n =
  n.state <- Start; n.uc <- `Uchar ux_soi; n.acc <- create_acc (); 
  n.first <- 0; n.last <- -1

let grow_acc n =
  let len = Array.length n.acc in
  let acc' = Array.create (2 * len) ux_eoi in 
  Array.blit n.acc 0 acc' 0 len; n.acc <- acc'

let ordered_add n u =    (* canonical ordering algorithm via insertion sort. *)
  n.last <- n.last + 1; if n.last = Array.length n.acc then grow_acc n;
  let c = ccc u in 
  if c = 0 then n.acc.(n.last) <- u else 
  begin
    let i = ref (n.last - 1) in
    while (!i >= 0 && ccc (n.acc.(!i)) > c) do
      n.acc.(!i + 1) <- n.acc.(!i); decr i;                  (* shift right. *)
    done;
    n.acc.(!i + 1) <- u
  end

let rec add n u = 
  if 0xAC00 <= u && u <= 0xD7A3 then
    begin (* LV or LVT hangul composite, copied from decomp to avoid alloc. *)
      let sindex = u - H.sbase in 
      let l = H.lbase + (sindex / H.ncount) in 
      let v = H.vbase + (sindex mod H.ncount) / H.tcount in 
      let t = H.tbase + (sindex mod H.tcount) in 
      if t = H.tbase then (ordered_add n l; ordered_add n v) else
      (ordered_add n l; ordered_add n v; ordered_add n t)
    end
  else
    begin match Data.decomp u with
    | [||] -> ordered_add n u
    | d -> 
        if d_compatibility d.(0) && not n.compat then ordered_add n u else
        begin 
          add n (d_uchar d.(0));
          for i = 1 to Array.length d - 1 do add n d.(i) done
        end
    end

let compose n =                         (* canonical composition algorithm. *)
  let i = ref n.first in
  while (!i < n.last) do
    incr i;
    let c = ccc n.acc.(!i) in
    try
      for j = !i - 1 downto n.first do
        let c' = ccc n.acc.(j) in 
        if c' = 0 then
          let u = _composite n.acc.(j) n.acc.(!i) in
          if u = ux_none then (* no composite => blocked *) raise Exit else
          begin 
            n.acc.(j) <- u;
            Array.blit n.acc (!i + 1) n.acc !i (n.last - !i);
            decr i;
            n.last <- n.last - 1
          end
        else
        if c' >= c then (* blocked *) raise Exit
      done
    with Exit -> ()
  done

let flush_next n =
  let ret = `Uchar n.acc.(n.first) in
  if n.first = n.last then (n.first <- 0; n.last <- -1) else 
  (n.first <- n.first + 1);
  ret
    
let flush_start n = if n.compose then compose n; flush_next n    
let add n = function 
| `Uchar u as uc -> 
    begin match n.state with 
    | Boundary -> 
        if n.boundary u 
        then (let prev = n.uc in n.uc <- uc; (prev :> ret)) 
        else (n.state <- Acc; add n (get_u n); add n u; `Await)
    | Acc -> 
        if n.boundary u
        then (n.state <- Flush; n.uc <- uc; flush_start n)
        else (add n u; `Await)
    | Start ->
        if n.boundary u
        then (n.state <- Boundary; n.uc <- uc; `Await)
        else (n.state <- Acc; add n u; `Await)
    | End | Flush -> invalid_add ()
    end
| `Await ->
    begin match n.state with
    | Flush -> 
        if acc_empty n
        then (n.state <- if is_end n then End else Boundary; `Await)
        else flush_next n
    | Start | End | Boundary | Acc -> `Await
    end
| `End ->
    begin match n.state with 
    | Boundary -> n.state <- End; (n.uc :> ret)
    | Acc -> n.state <- Flush; n.uc <- `Uchar ux_eoi; flush_start n
    | Start | End -> `Await
    | Flush -> invalid_add ()
    end

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
