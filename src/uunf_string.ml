(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
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

(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli

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
