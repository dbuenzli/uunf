(*---------------------------------------------------------------------------
   Copyright 2012 The uunf programmers. All rights reserved.
   SPDX-License-Identifier: ISC
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

let is_hangul_syllabe u = 0xAC00 <= u && u <= 0xD7A3

let iter_uchar_ints f =
  let rec loop u =
    let i = Uchar.to_int u in
    if Uchar.equal u Uchar.max then f i else
    (f i; loop (Uchar.succ u))
  in
  loop Uchar.min

(* Compact maps from characters to booleans. *)

let bool_prop_maps prop =
  let tm = Uunf_tmapbool.create true in
  let fm = Uunf_tmapbool.create false in
  let add_uchar u =
    let b = prop u in
    Uunf_tmapbool.set tm u b;
    Uunf_tmapbool.set fm u b;
  in
  iter_uchar_ints add_uchar; tm, fm

let assert_bool_prop_maps prop tm fm =
  let assert_uchar u =
    let fail () = failwith (str "bool prop map failure for U+%04X" u) in
    let b = prop u in
    if b <> Uunf_tmapbool.get tm u then fail ();
    if b <> Uunf_tmapbool.get fm u then fail ();
  in
  iter_uchar_ints assert_uchar

(* Compact maps from characters to bytes. *)

let byte_prop_map ~default prop =
  let m = Uunf_tmapbyte.create default in
  let add_uchar u = Uunf_tmapbyte.set m u (prop u) in
  iter_uchar_ints add_uchar; m

let assert_byte_prop_map prop m =
  let assert_uchar u =
    if (prop u) = Uunf_tmapbyte.get m u then () else
    failwith (str "byte prop map failure for U+%04X" u)
  in
  iter_uchar_ints assert_uchar

(* Compact maps from characters to arbitrary values. *)

let prop_map ~default prop =
  let m = Uunf_tmap.create default in
  let add_uchar u = Uunf_tmap.set m u (prop u) in
  iter_uchar_ints add_uchar; m

let assert_prop_map prop m =
  let assert_uchar u =
    if (prop u) = Uunf_tmap.get m u then () else
    failwith (str "prop map failure for U+%04X" u)
  in
  iter_uchar_ints assert_uchar

let ucd_get ucd u p pstr = match Uucd.cp_prop ucd u p with
| None -> invalid_arg (str "no %s property for U+%04X" pstr u)
| Some v -> v

(* Generate a module *)

let year = (Unix.gmtime (Unix.gettimeofday ())).Unix.tm_year + 1900

let pp_mod pp_mod ppf m =
  pp ppf
"\
(*---------------------------------------------------------------------------
   Copyright (c) %d The uunf programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* WARNING do not edit. This file was automatically generated. *)
@\n@[%a@]@\n" year pp_mod m
