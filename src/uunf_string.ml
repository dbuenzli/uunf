(*---------------------------------------------------------------------------
   Copyright (c) 2015 The uunf programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let normalize_utf_x
    (fold_x :
       ?pos:int -> ?len:int -> 'a Uutf.String.folder -> 'a -> string -> 'a)
    add_x nf s
  =
  let b = Buffer.create (String.length s * 3) in
  let n = Uunf.create nf in
  let rec add v = match Uunf.add n v with
  | `Uchar u -> add_x b u; add `Await
  | `Await | `End -> ()
  in
  let add_uchar _ _ = function
  | `Malformed _ -> add (`Uchar Uutf.u_rep)
  | `Uchar _ as u -> add u
  in
  fold_x add_uchar () s; add `End; Buffer.contents b

let normalize_utf_8 nf s =
  normalize_utf_x Uutf.String.fold_utf_8 Uutf.Buffer.add_utf_8 nf s

let normalize_utf_16be nf s =
  normalize_utf_x Uutf.String.fold_utf_16be Uutf.Buffer.add_utf_16be nf s

let normalize_utf_16le nf s =
  normalize_utf_x Uutf.String.fold_utf_16le Uutf.Buffer.add_utf_16le nf s
