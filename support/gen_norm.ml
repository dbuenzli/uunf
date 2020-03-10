(*---------------------------------------------------------------------------
   Copyright (c) 2015 The uunf programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

let strf = Printf.sprintf
let pp = Format.fprintf

(* Normalization properties. *)

let pp_boundary nf ucd ppf nf_quick_check =
  Gen.log "%s boundary property as character boolean trie map" nf;
  let prop_str = strf "%s_quick_check" nf in
  let prop u = match Gen.ucd_get ucd u nf_quick_check prop_str with
  | `Maybe | `False -> false
  | `True -> (Gen.ucd_get ucd u Uucd.canonical_combining_class "ccc") = 0
  in
  let tm, fm = Gen.bool_prop_maps prop in
  let tm_size, fm_size = Uunf_tmapbool.size tm, Uunf_tmapbool.size fm in
  let use_fm = tm_size > fm_size in
  Gen.log ", asserting data.\n"; Gen.assert_bool_prop_maps prop tm fm;
  Gen.log " boolean trie map (default true)  size: %s\n"
    (Gen.str_of_size tm_size);
  Gen.log " boolean trie map (default false) size: %s\n"
    (Gen.str_of_size fm_size);
  Gen.log " Using map with default %b.\n\n" (not use_fm);
  let m = if use_fm then fm else tm in
  pp ppf "  @[<hov>let %s_boundary_map = %a@]@\n@\n" nf Uunf_tmapbool.dump m;
  ()

let pp_ccc ppf ucd =
  Gen.log "ccc property as character byte trie map";
  let prop u = Gen.ucd_get ucd u Uucd.canonical_combining_class "ccc" in
  let m = Gen.byte_prop_map ~default:0 prop in
  let t_size = Uunf_tmapbyte.size m in
  Gen.log ", asserting data.\n"; Gen.assert_byte_prop_map prop m;
  Gen.log " trie map size: %s\n\n" (Gen.str_of_size t_size);
  pp ppf "  @[let ccc_map = %a@]@\n@\n" Uunf_tmapbyte.dump m;
  ()

let pp_decomp ppf ucd =
  Gen.log "decomposition mapping as trie map";
  let default = Uunf_tmap.nil in
  let prop u =
    match Gen.ucd_get ucd u Uucd.decomposition_mapping "decomposition mapping"
    with
    | `Self -> default
    | `Cps cps ->
        let t = Gen.ucd_get ucd u Uucd.decomposition_type "decomposition_type"in
        if Gen.is_hangul_syllabe u then begin
          if t <> `Can then invalid_arg (strf "hangul not canon decomp %X" u);
          default
        end else begin
          let d = Array.of_list cps in
          let compat = t <> `Can in
          if compat then d.(0) <- (1 lsl 24) lor d.(0);
          d
        end
  in
  let m = Gen.prop_map ~default prop in
  let size_v = function [||] -> 0 | a -> 1 + Array.length a in
  let t_size = Uunf_tmap.size size_v m in
  let pp_decomp ppf = function
  | [||] -> pp ppf "nil"
  | a ->
      pp ppf "[|@,";
      for i = 0 to Array.length a - 1 do pp ppf "@,0x%X;@," a.(i) done;
      pp ppf "@,|]"
  in
  Gen.log ", asserting data.\n"; Gen.assert_prop_map prop m;
  Gen.log " trie map size: %s\n\n" (Gen.str_of_size t_size);
  pp ppf "  @[let decomp_map = %a@]@\n@\n" (Uunf_tmap.dump pp_decomp) m;
  ()

module Cpmap = Uucd.Cpmap

let pp_compose ppf ucd =
  Gen.log "composition to primary composites as trie map";
  let m = ref Cpmap.empty in
  let add_map cp1 cp2 c =
    let l = try Cpmap.find cp1 !m with Not_found -> [] in
    m := Cpmap.add cp1 ((cp2, c) :: l) !m
  in
  let add u =
    match Gen.ucd_get ucd u Uucd.decomposition_mapping "decomposition_mapping"
    with
    | `Self -> ()
    | `Cps cps ->
        let fce = "full_decomposition_exclusion" in
        if Gen.ucd_get ucd u Uucd.full_composition_exclusion fce then () else
        let t = Gen.ucd_get ucd u Uucd.decomposition_type "decomposition_type"in
        if t <> `Can then () else
        if Gen.is_hangul_syllabe u then () else
        match cps with
        | [cp1; cp2] -> add_map cp1 cp2 u
        | _ -> invalid_arg (strf "cannot handle composition for %X" u);
  in
  Gen.iter_uchar_ints add;
  let default = Uunf_tmap.nil in
  let max_comps = ref 0 in
  let prop u =
    try
      let comps = List.sort compare (Cpmap.find u !m) in
      let len = List.length comps in
      let a = Array.make (len * 2) 0 in
      let set i (cp2, c) = a.(2 * i) <- cp2; a.(2 * i + 1) <- c in
      List.iteri set comps;
      max_comps := max !max_comps len;
      a
    with Not_found -> Uunf_tmap.nil
  in
  let m = Gen.prop_map ~default prop in
  let size_v = function [||] -> 0 | a -> 1 + Array.length a in
  let t_size = Uunf_tmap.size size_v m in
  let pp_d ppf = function
  | [||] -> pp ppf "nil"
  | a ->
      pp ppf "[|@,";
      for i = 0 to Array.length a - 1 do pp ppf "@,0x%X;@," a.(i) done;
      pp ppf "@,|]"
  in
  Gen.log ", asserting data.\n"; Gen.assert_prop_map prop m;
  Gen.log " trie map size: %s\n" (Gen.str_of_size t_size);
  Gen.log " max num. of possible composition for a base char: %d\n\n"
    !max_comps;
  pp ppf "  @[let compose_map = %a@]@\n@\n" (Uunf_tmap.dump pp_d) m;
  ()

let pp_norms ppf ucd =
  pp ppf "  open Uunf_tmapbool;;@\n@\n";
  pp_boundary "nfc" ucd ppf Uucd.nfc_quick_check;
  pp_boundary "nfd" ucd ppf Uucd.nfd_quick_check;
  pp_boundary "nfkc" ucd ppf Uucd.nfkc_quick_check;
  pp_boundary "nfkd" ucd ppf Uucd.nfkd_quick_check;
  pp ppf "  open Uunf_tmapbyte;;@\n@\n";
  pp_ccc ppf ucd;
  pp ppf "  open Uunf_tmap;;@\n@\n";
  pp_decomp ppf ucd;
  pp_compose ppf ucd;
  ()

let pp_mod ppf ucd = Gen.pp_mod pp_norms ppf ucd

(*---------------------------------------------------------------------------
   Copyright (c) 2015 The uunf programmers

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
