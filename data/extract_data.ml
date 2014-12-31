(*---------------------------------------------------------------------------
   Copyright 2012 Daniel C. Bünzli. All rights reserved.
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

module Uchar = struct
  let err_succ = "no successor"
  let min = 0x0000
  let max = 0x10FFFF
  let min_surrogate = 0xD800
  let max_surrogate = 0xDFFF
  let is_hangul_syllabe u = 0xAC00 <= u && u <= 0xD7A3
  let succ u =
    if u = max then invalid_arg err_succ else
    if u = min_surrogate - 1 then max_surrogate + 1 else
    u + 1

  let iter f =
    for u = min to min_surrogate - 1 do f u done;
    for u = max_surrogate + 1 to max do f u done
end

(* Compact maps from characters to booleans. *)

let bool_prop_maps prop =
  let tm = Tboolmap.create true in
  let fm = Tboolmap.create false in
  let add_uchar u =
    let b = prop u in
    Tboolmap.set tm u b;
    Tboolmap.set fm u b;
  in
  Uchar.iter add_uchar; tm, fm

let assert_bool_prop_maps prop tm fm =
  let assert_uchar u =
    let fail () = failwith (str "bool prop map failure for U+%04X" u) in
    let b = prop u in
    if b <> Tboolmap.get tm u then fail ();
    if b <> Tboolmap.get fm u then fail ();
  in
  Uchar.iter assert_uchar

(* Compact maps from characters to bytes. *)

let byte_prop_map ~default prop =
  let m = Tbytemap.create default in
  let add_uchar u = Tbytemap.set m u (prop u) in
  Uchar.iter add_uchar; m

let assert_byte_prop_map prop m =
  let assert_uchar u =
    if (prop u) = Tbytemap.get m u then () else
    failwith (str "byte prop map failure for U+%04X" u)
  in
  Uchar.iter assert_uchar

(* Compact maps from characters to arbitrary values. *)

let prop_map ~default prop =
  let m = Tmap.create default in
  let add_uchar u = Tmap.set m u (prop u) in
  Uchar.iter add_uchar; m

let assert_prop_map prop m =
  let assert_uchar u =
    if (prop u) = Tmap.get m u then () else
    failwith (str "prop map failure for U+%04X" u)
  in
  Uchar.iter assert_uchar

(* Normalization properties. *)

let ucd_get ucd u p pstr = match Uucd.cp_prop ucd u p with
| None -> invalid_arg (str "no %s property for U+%04X" pstr u)
| Some v -> v

let pp_boundary nf ucd ppf nf_quick_check =
  log "%s boundary property as character boolean trie map" nf;
  let prop_str = str "%s_quick_check" nf in
  let prop u = match ucd_get ucd u nf_quick_check prop_str with
  | `Maybe | `False -> false
  | `True -> (ucd_get ucd u Uucd.canonical_combining_class "ccc") = 0
  in
  let tm, fm = bool_prop_maps prop in
  let tm_size, fm_size = Tboolmap.size tm, Tboolmap.size fm in
  let use_fm = tm_size > fm_size in
  log ", asserting data.\n"; assert_bool_prop_maps prop tm fm;
  log " boolean trie map (default true)  size: %s\n" (str_of_size tm_size);
  log " boolean trie map (default false) size: %s\n" (str_of_size fm_size);
  log " Using map with default %b.\n\n" (not use_fm);
  let m = if use_fm then fm else tm in
  pp ppf "  @[<hov>let %s_boundary_map = %a@]@\n@\n" nf Tboolmap.dump m;
  pp ppf "  @[let %s_boundary u = Tboolmap.get %s_boundary_map u@]@\n@\n" nf nf

let pp_ccc ppf ucd =
  log "ccc property as character byte trie map";
  let prop u = ucd_get ucd u Uucd.canonical_combining_class "ccc" in
  let m = byte_prop_map ~default:0 prop in
  let t_size = Tbytemap.size m in
  log ", asserting data.\n"; assert_byte_prop_map prop m;
  log " trie map size: %s\n\n" (str_of_size t_size);
  pp ppf "  @[let ccc_map = %a@]@\n@\n" Tbytemap.dump m;
  pp ppf "  let ccc u = Tbytemap.get ccc_map u@\n@\n"

let pp_decomp ppf ucd =
  log "decomposition mapping as trie map";
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
  let m = prop_map ~default prop in
  let size_v = function [||] -> 0 | a -> 1 + Array.length a in
  let t_size = Tmap.size size_v m in
  let pp_decomp ppf = function
  | [||] -> pp ppf "nil"
  | a ->
      pp ppf "[|@,";
      for i = 0 to Array.length a - 1 do pp ppf "@,0x%X;@," a.(i) done;
      pp ppf "@,|]"
  in
  log ", asserting data.\n"; assert_prop_map prop m;
  log " trie map size: %s\n\n" (str_of_size t_size);
  pp ppf "  @[let decomp_map = %a@]@\n@\n" (Tmap.dump pp_decomp) m;
  pp ppf "  let decomp u = Tmap.get decomp_map u@\n@\n";

module Cpmap = Uucd.Cpmap

let pp_compose ppf ucd =
  log "composition to primary composites as trie map";
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
  let m = prop_map ~default prop in
  let size_v = function [||] -> 0 | a -> 1 + Array.length a in
  let t_size = Tmap.size size_v m in
  let pp_d ppf = function
  | [||] -> pp ppf "nil"
  | a ->
      pp ppf "[|@,";
      for i = 0 to Array.length a - 1 do pp ppf "@,0x%X;@," a.(i) done;
      pp ppf "@,|]"
  in
  log ", asserting data.\n"; assert_prop_map prop m;
  log " trie map size: %s\n" (str_of_size t_size);
  log " max num. of possible composition for a base char: %d\n\n" !max_comps;
  pp ppf "  @[let compose_map = %a@]@\n@\n" (Tmap.dump pp_d) m;
  pp ppf "  let compose u = Tmap.get compose_map u@\n@\n"

let pp_data_module ppf ucd =
  pp ppf "  open Tboolmap;;@\n@\n";
  pp_boundary "nfc" ucd ppf Uucd.nfc_quick_check;
  pp_boundary "nfd" ucd ppf Uucd.nfd_quick_check;
  pp_boundary "nfkc" ucd ppf Uucd.nfkc_quick_check;
  pp_boundary "nfkd" ucd ppf Uucd.nfkd_quick_check;
  pp ppf "  open Tbytemap;;@\n@\n";
  pp_ccc ppf ucd;
  pp ppf "  open Tmap;;@\n@\n";
  pp_decomp ppf ucd;
  pp_compose ppf ucd

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
   Copyright 2012 Daniel C. Bünzli
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
