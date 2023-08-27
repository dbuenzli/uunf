(*---------------------------------------------------------------------------
   Copyright (c) 2015 The uunf programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Unicode text normalization on UTF-X OCaml strings.

    {!Uunf} functions acting directly on UTF-X encoded OCaml strings.

    {b Warning.} All these function silently replace malformed encoded Unicode
    data by a {!Stdlib.Uchar.rep} character.

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
