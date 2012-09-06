(*---------------------------------------------------------------------------
   Copyright %%COPYRIGHT%%. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(* Extracts normalization data from the Unicode Character Database *)

let log fmt = Printf.eprintf (fmt ^^ "%!")
let pp = Format.fprintf
let str = Printf.sprintf 
let str_of_size s = 
  let b = s * (Sys.word_size / 8) in
  if b < 1_048_576 then str "%.1f Ko" (float b /. 1024.) else
  if b < 1_073_741_824 then str "%.1f Mo" (float b /. 1024. ** 2.) else
  str "%.1f Go" (float b /. 1024. ** 3.)

(* Characters *)

type uchar = int 
module Uchar = struct
  let err_succ = "no successor"
  let min = 0x0000
  let max = 0x10FFFF
  let min_surrogate = 0xD800
  let max_surrogate = 0xDFFF
  let is_hangul_syllabe cp = 0xAC00 <= cp && cp <= 0xD7A3
  let succ i = 
    if i = max then invalid_arg err_succ else
    if i = min_surrogate - 1 then max_surrogate + 1 else
    i + 1

  let iter f = 
    for cp = min to min_surrogate - 1 do f cp done;
    for cp = max_surrogate + 1 to max do f cp done
end

(* Sets of characters satisfying a property *)

let bool_prop_tsets prop = 
  let p = Tset.create () in 
  let np = Tset.create () in 
  let add_uchar u = 
    let b = prop u in 
    Tset.set ~default:true p u b; 
    Tset.set ~default:false np u b;
  in
  Uchar.iter add_uchar; 
  p, np
 
let assert_bool_tsets prop p np = 
  let assert_cp u = 
    let b = prop u in 
    if b <> Tset.find ~default:true p u then 
    failwith (str "bool trie set failure for %X" u); 
    if b <> Tset.find ~default:false np u then 
    failwith (str "bool trie set failure for %X" u)
  in
  Uchar.iter assert_cp

(* Compact maps from characters to values. *)

let prop_map ~default prop =                          (* computes trie map. *)
  let trie = Tmap.create () in
  let add_uchar cp = Tmap.set ~default trie cp (prop cp) in
  Uchar.iter add_uchar; trie

let assert_map ~default prop tmap = 
  let assert_cp cp = 
    if (prop cp) = Tmap.find ~default tmap cp then () else 
    failwith (str "trie map failure for U+%04X" cp)
  in
  Uchar.iter assert_cp

(* Normalization properties. *)

let ucd_get ucd u p pstr = match Uucd.cp_prop ucd u p with
| None -> invalid_arg (str "no %s property for U+%04X" pstr u)
| Some v -> v

let pp_boundary nf ucd ppf nf_quick_check =
  log "%s boundary boolean property as trie map character set" nf;
  let prop_str = str "%s_quick_check" nf in
  let prop u = match ucd_get ucd u nf_quick_check prop_str with 
  | `Maybe | `False -> false
  | `True -> (ucd_get ucd u Uucd.canonical_combining_class "ccc") = 0 
  in
  let p, np = bool_prop_tsets prop in
  let p_size, np_size = Tset.size p, Tset.size np in
  let neg = p_size > np_size in
  let id = str "%s_boundary_%s" nf (if neg then "false" else "true") in
  log ", asserting data.\n"; 
  assert_bool_tsets prop p np;
  log " true  trie set size: %s\n" (str_of_size p_size);
  log " false trie set size: %s\n" (str_of_size np_size);
  log " Using %s definition.\n\n" (if neg then "false set" else "true set");
  pp ppf "  @[<hov>let %s = %a@]@\n@\n" id Tset.dump (if neg then np else p);
  pp ppf "  @[let %s_boundary u = Tset.find ~default:%s %s u@]@\n@\n" 
    nf (if neg then "false" else "true") id

let pp_ccc ppf ucd = 
  log "ccc property as trie map";
  let default = 0 in
  let prop u = ucd_get ucd u Uucd.canonical_combining_class "ccc" in
  let tmap = prop_map ~default prop in 
  let t_size = Tmap.size (fun _ -> 0) tmap in 
  let pp_int ppf i = Format.fprintf ppf "%d" i in
  log ", asserting data.\n"; assert_map ~default prop tmap;
  log " trie map size: %s\n\n" (str_of_size t_size);
  pp ppf "  @[let ccc_map = %a@]@\n@\n" (Tmap.dump pp_int) tmap; 
  pp ppf "  let ccc cp = Tmap.find ~default:%d ccc_map cp@\n@\n" default

let pp_decomp ppf ucd = 
  log "decomposition as trie map"; 
  let default = Tmap.nil in 
  let prop u =
    match ucd_get ucd u Uucd.decomposition_mapping "decomposition mapping" with 
    | `Self -> default
    | `Cps cps -> 
        let t = ucd_get ucd u Uucd.decomposition_type "decomposition_type"in
        if Uchar.is_hangul_syllabe u then begin 
          if t <> `Can then invalid_arg (str "hangul not canon decomp %X" u);
          default
        end else begin 
          let d = Array.of_list cps in
          let compat = t <> `Can in
          if compat then d.(0) <- (1 lsl 24) lor d.(0);
          d
        end
  in
  let tmap = prop_map ~default prop in
  let size_v = function [||] -> 0 | a -> 1 + Array.length a in
  let t_size = Tmap.size size_v tmap in 
  let pp_decomp ppf = function
  | [||] -> pp ppf "nil"
  | a -> 
      pp ppf "[|@,";
      for i = 0 to Array.length a - 1 do pp ppf "@,0x%X;@," a.(i) done;
      pp ppf "@,|]"
  in
  log ", asserting data.\n"; assert_map ~default prop tmap;
  log " trie map size: %s\n\n" (str_of_size t_size);
  pp ppf "  @[let decomp_map = %a@]@\n@\n" (Tmap.dump pp_decomp) tmap; 
  pp ppf "  let decomp cp = Tmap.find ~default:Tmap.nil decomp_map cp@\n@\n";

module Cpmap = Uucd.Cpmap 

let pp_primary_composite ppf ucd = 
  log "primary composites as trie map"; 
  let m = ref Cpmap.empty in 
  let add_map cp1 cp2 c = 
    let l = try Cpmap.find cp1 !m with Not_found -> [] in 
    m := Cpmap.add cp1 ((cp2, c) :: l) !m
  in
  let add u = 
    match ucd_get ucd u Uucd.decomposition_mapping "decomposition_mapping" with
    | `Self -> () 
    | `Cps cps -> 
        let fce = "full_decomposition_exclusion" in
        if ucd_get ucd u Uucd.full_composition_exclusion fce then () else
        let t = ucd_get ucd u Uucd.decomposition_type "decomposition_type" in
        if t <> `Can then () else
        if Uchar.is_hangul_syllabe u then () else 
        match cps with 
        | [cp1; cp2] -> add_map cp1 cp2 u
        | _ -> invalid_arg (str "cannot handle composition for %X" u);
  in
  Uchar.iter add;
  let default = Tmap.nil in
  let max_comps = ref 0 in
  let prop u = 
    try 
      let comps = List.sort compare (Cpmap.find u !m) in
      let len = List.length comps in 
      let a = Array.create (len * 2) 0 in
      let set i (cp2, c) = a.(2 * i) <- cp2; a.(2 * i + 1) <- c in 
      List.iteri set comps;
      max_comps := max !max_comps len;
      a          
    with Not_found -> Tmap.nil
  in
  let tmap = prop_map ~default prop in
  let size_v = function [||] -> 0 | a -> 1 + Array.length a in
  let t_size = Tmap.size size_v tmap in 
  let pp_d ppf = function 
  | [||] -> pp ppf "nil"
  | a -> 
      pp ppf "[|@,";
      for i = 0 to Array.length a - 1 do pp ppf "@,0x%X;@," a.(i) done;
      pp ppf "@,|]"
  in
  log ", asserting data.\n"; assert_map ~default prop tmap;
  log " trie map size: %s\n" (str_of_size t_size);
  log " max num. of possible composition for a base char: %d\n\n" !max_comps;
  pp ppf "  @[let primary_composite_map = %a@]@\n@\n" (Tmap.dump pp_d) tmap; 
  pp ppf "  let primary_composite cp = Tmap.find ~default:Tmap.nil";
  pp ppf " primary_composite_map cp@\n@\n"

let pp_data_module ppf ucd = 
  pp ppf "  open Tset;;@\n@\n";
  pp_boundary "nfc" ucd ppf Uucd.nfc_quick_check; 
  pp_boundary "nfd" ucd ppf Uucd.nfd_quick_check; 
  pp_boundary "nfkc" ucd ppf Uucd.nfkc_quick_check; 
  pp_boundary "nfkd" ucd ppf Uucd.nfkd_quick_check;
  pp ppf "  open Tmap;;@\n@\n";
  pp_ccc ppf ucd;
  pp_decomp ppf ucd; 
  pp_primary_composite ppf ucd
    
let ucd_or_die inf = try 
  let ic = if inf = "-" then stdin else open_in inf in
  let d = Uucd.decoder (`Channel ic) in 
  match Uucd.decode d with
  | `Ok db -> db 
  | `Error e -> 
      let (l0, c0), (l1, c1) = Uucd.decoded_range d in
      Printf.eprintf "%s:%d.%d-%d.%d: %s\n%!" inf l0 c0 l1 c1 e; 
      exit 1
with Sys_error e -> Printf.eprintf "%s\n%!" e; exit 1

let main () =
  let argc = Array.length Sys.argv in
  let inf = if argc = 1 then "-" else Sys.argv.(1) in 
  let ucd = (log "Loading Unicode character database.\n"; ucd_or_die inf) in
  pp_data_module Format.std_formatter ucd 

let () = if (not !Sys.interactive) then main ()

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
