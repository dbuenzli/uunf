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
  let tm = Uunf_tmapbool.create true in
  let fm = Uunf_tmapbool.create false in
  let add_uchar u =
    let b = prop u in
    Uunf_tmapbool.set tm u b;
    Uunf_tmapbool.set fm u b;
  in
  Uchar.iter add_uchar; tm, fm

let assert_bool_prop_maps prop tm fm =
  let assert_uchar u =
    let fail () = failwith (str "bool prop map failure for U+%04X" u) in
    let b = prop u in
    if b <> Uunf_tmapbool.get tm u then fail ();
    if b <> Uunf_tmapbool.get fm u then fail ();
  in
  Uchar.iter assert_uchar

(* Compact maps from characters to bytes. *)

let byte_prop_map ~default prop =
  let m = Uunf_tmapbyte.create default in
  let add_uchar u = Uunf_tmapbyte.set m u (prop u) in
  Uchar.iter add_uchar; m

let assert_byte_prop_map prop m =
  let assert_uchar u =
    if (prop u) = Uunf_tmapbyte.get m u then () else
    failwith (str "byte prop map failure for U+%04X" u)
  in
  Uchar.iter assert_uchar

(* Compact maps from characters to arbitrary values. *)

let prop_map ~default prop =
  let m = Uunf_tmap.create default in
  let add_uchar u = Uunf_tmap.set m u (prop u) in
  Uchar.iter add_uchar; m

let assert_prop_map prop m =
  let assert_uchar u =
    if (prop u) = Uunf_tmap.get m u then () else
    failwith (str "prop map failure for U+%04X" u)
  in
  Uchar.iter assert_uchar

let ucd_get ucd u p pstr = match Uucd.cp_prop ucd u p with
| None -> invalid_arg (str "no %s property for U+%04X" pstr u)
| Some v -> v

(* Generate a module *)

let pp_mod pp_mod ppf m =
  pp ppf
"\
(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(* WARNING do not edit. This file was automatically generated. *)
@\n@[%a@]@\n
(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli.
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
   \"AS IS\" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
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
" pp_mod m

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
