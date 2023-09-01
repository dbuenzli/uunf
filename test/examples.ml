(*---------------------------------------------------------------------------
   Copyright (c) 2012 The uunf programmers. All rights reserved.
   SPDX-License-Identifier: CC0-1.0
  ---------------------------------------------------------------------------*)

let utf_8_normalize nf s =
  let rec add buf normalizer v = match Uunf.add normalizer v with
  | `Uchar u -> Buffer.add_utf_8_uchar buf u; add buf normalizer `Await
  | `Await | `End -> ()
  in
  let rec loop buf s i max normalizer =
    if i > max then (add buf normalizer `End; Buffer.contents buf) else
    let dec = String.get_utf_8_uchar s i in
    add buf normalizer (`Uchar (Uchar.utf_decode_uchar dec));
    loop buf s (i + Uchar.utf_decode_length dec) max normalizer
  in
  let buf = Buffer.create (String.length s * 3) in
  let normalizer = Uunf.create nf in
  loop buf s 0 (String.length s - 1) normalizer
