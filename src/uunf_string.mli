(*---------------------------------------------------------------------------
   Copyright (c) 2015 The uunf programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** Unicode text normalization on UTF-X OCaml strings.

    {!Uunf} functions acting directly on UTF-X encoded OCaml strings.

    {b Warning.} All these function silently replace malformed encoded Unicode
    data by a {!Uutf.u_rep} character.

    {e Unicode version %%UNICODE_VERSION%%} *)

(** {1:norm Normalize} *)

val normalize_utf_8 : Uunf.form -> string -> string
(** [normalize_utf_8 nf s] is the UTF-8 encoded string [s] in normal
    form [nf]. *)

val normalize_utf_16be : Uunf.form -> string -> string
(** [normalize_utf_16be nf s] is the UTF-16BE encoded string [s] in
    normal form [nf]. *)

val normalize_utf_16le : Uunf.form -> string -> string
(** [normalize_utf_16le nf s] is the UTF-16LE encoded string [s] in
    normal form [nf]. *)

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
