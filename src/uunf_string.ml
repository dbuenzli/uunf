(*---------------------------------------------------------------------------
   Copyright (c) 2015 The uunf programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let normalize_utf_x get_utf add_utf nf s =
  let rec add add_utf buf normalizer v = match Uunf.add normalizer v with
  | `Uchar u -> add_utf buf u; add add_utf buf normalizer `Await
  | `Await | `End -> ()
  in
  let rec loop get_utf add_utf buf s i max normalizer =
    if i > max then (add add_utf buf normalizer `End; Buffer.contents buf) else
    let dec = get_utf s i in
    let u = Uchar.utf_decode_uchar dec in
    add add_utf buf normalizer (`Uchar u);
    loop get_utf add_utf buf s (i + Uchar.utf_decode_length dec) max normalizer
  in
  let b = Buffer.create (String.length s * 3) in
  let normalizer = Uunf.create nf in
  loop get_utf add_utf b s 0 (String.length s - 1) normalizer

let normalize_utf_8 nf s =
  normalize_utf_x String.get_utf_8_uchar Buffer.add_utf_8_uchar nf s

let normalize_utf_16be nf s =
  normalize_utf_x String.get_utf_16be_uchar Buffer.add_utf_16be_uchar nf s

let normalize_utf_16le nf s =
  normalize_utf_x String.get_utf_16le_uchar Buffer.add_utf_16le_uchar nf s
