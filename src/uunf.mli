(*---------------------------------------------------------------------------
   Copyright (c) 2012 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Unicode text normalization.

    [Uunf] normalizes Unicode text.  It supports all Unicode
    normalization forms. The module is independent from any IO
    mechanism or Unicode text data structure and it can process text
    without a complete in-memory representation of the data.

    The supported Unicode version is determined by the {!unicode_version}
    value.

    Consult the {{!basics}basics}, {{!limits}limitations} and
    {{!examples}examples} of use.

    {e %%VERSION%% — Unicode version %%UNICODE_VERSION%% —
       {{:%%PKG_HOMEPAGE%% }homepage}}

    {3 References}
    {ul
    {- The Unicode Consortium.
    {e {{:http://www.unicode.org/versions/latest}The Unicode Standard}}.
    (latest version)}
    {- Mark Davis.
    {e {{:http://www.unicode.org/reports/tr15/}UAX #15 Unicode Normalization
    Forms}}. (latest version)}
    {- The Unicode Consortium.
    {e {{:http://www.unicode.org/charts/normalization/}Normalization charts}.
    }}} *)

(** {1 Normalize} *)

type form = [ `NFD | `NFC | `NFKD | `NFKC ]
(** The type for normalization forms.
    {ul
    {- [`NFD] {{:http://www.unicode.org/glossary/#normalization_form_d}
       normalization form D}, canonical decomposition.}
    {- [`NFC] {{:http://www.unicode.org/glossary/#normalization_form_c}
       normalization form C}, canonical decomposition followed by
       canonical composition
       ({{:http://www.w3.org/TR/charmod-norm/}recommended} for the www).}
    {- [`NFKD] {{:http://www.unicode.org/glossary/#normalization_form_kd}
       normalization form KD}, compatibility decomposition.}
    {- [`NFKC] {{:http://www.unicode.org/glossary/#normalization_form_kc}
       normalization form KC}, compatibility decomposition,
       followed by canonical composition.}} *)

type t
(** The type for Unicode text normalizers. *)

type ret = [ `Uchar of Uchar.t | `End | `Await ]
(** The type for normalizer results. See {!add}. *)

val create : [< form ] -> t
(** [create nf] is an Unicode text normalizer for the normal form [nf]. *)

val form : t -> form
(** [form n] is the normalization form of [n]. *)

val add : t -> [ `Uchar of Uchar.t | `Await | `End ] -> ret
(** [add n v] is:
    {ul
    {- [`Uchar u] if [u] is the next character in the normalized
       sequence. The client must then call [add] with [`Await]
       until [`Await] is returned.}
    {- [`Await] when the normalizer is ready to add a new
       [`Uchar] or [`End].}}

    For [v] use [`Uchar u] to add a new character to the sequence
    to normalize and [`End] to signal the end of sequence. After
    adding one of these two values, always call [add] with [`Await]
    until [`Await] is returned.

    {b Raises.} [Invalid_argument] if [`Uchar ] or [`End] is
    added directly after an [`Uchar] was returned by the normalizer
    or if an [`Uchar] is added after [`End] was added.

    {b Warning.} [add] deals with Unicode
    {{:http://www.unicode.org/glossary/#unicode_scalar_value}
    scalar values}. If you are handling foreign data you must assert
    that before with {!is_scalar_value}. *)

val reset : t -> unit
(** [reset n] resets the normalizer to a state equivalent to the
    state of [Uunf.create (Uunf.form n)]. *)

val copy : t -> t
(** [copy n] is a copy of [n] in its current state. Subsequent
    {!add}s on [n] do not affect the copy. *)

val pp_ret : Format.formatter -> ret -> unit
(** [pp_ret ppf v] prints an unspecified representation of [v] on [ppf]. *)

(** {1:props Normalization properties}

    These properties are used internally to implement the normalizers.
    They are not needed to use the module but are exposed as they may
    be useful to implement other algorithms. *)

val unicode_version : string
(** [unicode_version] is the Unicode version supported by the module. *)

val ccc : Uchar.t -> int
(** [ccc u] is [u]'s
    {{:http://www.unicode.org/glossary/#combining_class}canonical combining
    class} value. *)

val decomp : Uchar.t -> int array
(** [decomp u] is [u]'s
    {{:http://www.unicode.org/glossary/#decomposition_mapping}decomposition
    mapping}. If the empty array is returned, [u] decomposes to itself.

    The first number in the array contains additional information, it
    cannot be used as an {!uchar}. Use {!d_uchar} on the number to get the
    actual character and {!d_compatibility} to find out if this is
    a compatibility decomposition. All other characters of the array
    are guaranteed to be convertible using {!Uchar.of_int}.

    {b Warning.} Do {b not} mutate the array. *)

val d_uchar : int -> Uchar.t
(** See {!decomp}. *)

val d_compatibility : int -> bool
(** See {!decomp}. *)

val composite : Uchar.t -> Uchar.t -> Uchar.t option
(** [composite u1 u2] is the
    {{:http://www.unicode.org/glossary/#primary_composite}primary composite}
    canonically equivalent to  the sequence [<u1,u2>], if any. *)

(** {1:limits Limitations}

    An [Uunf] normalizer consumes only a small bounded amount of
    memory on ordinary, {e meaningful} text. However on legal but {e
    degenerate} text like a
    {{:http://www.unicode.org/glossary/#starter}starter} followed by
    10'000 combining
    {{:http://www.unicode.org/glossary/#nonspacing_mark}non-spacing
    marks} it will have to bufferize all the marks (a workaround is
    to first convert your input to
    {{:http://www.unicode.org/reports/tr15/#Stream_Safe_Text_Format}stream-safe
    text format}). *)

(** {1:basics Basics}

    A normalizer is a stateful filter that inputs a sequence of
    characters and outputs an equivalent sequence in the requested
    normal form.

    The function {!create} returns a new normalizer for a given normal
    form:
{[
let nfd = Uunf.create `NFD;;
]}
    To add characters to the sequence to normalize, call {!add} on
    [nfd] with [`Uchar _].  To end the sequence, call {!add} on [nfd]
    with [`End]. The normalized sequence of characters is returned,
    character by character, by the successive calls to {!add}.

    The client and the normalizer must wait on each other to limit
    internal buffering: each time the client adds to the sequence by
    calling {!add} with [`Uchar] or [`End] it must continue to call
    {!add} with [`Await] until the normalizer returns [`Await]. In
    practice this leads to the following kind of control flow:
{[
let rec add acc v = match Uunf.add nfd v with
| `Uchar u -> add (u :: acc) `Await
| `Await | `End -> acc
]}
    For example to normalize the character [U+00E9] (é) with [nfd] to a list
    of characters we can write:
{[
let e_acute = Uchar.of_int 0x00E9
let e_acute_nfd = List.rev (add (add [] (`Uchar e_acute)) `End)
]}
    The next section has more examples.
*)

(** {1:examples Examples}

    {2:utf8 UTF-8 normalization}

    [utf_8_normalize nf s] is the UTF-8 encoded normal form [nf] of
    the UTF-8 encoded string [s]. This example uses {!Uutf} to fold
    over the characters of [s] and to encode the normalized
    sequence in a standard OCaml buffer.
{[
let utf_8_normalize nf s =
  let b = Buffer.create (String.length s * 3) in
  let n = Uunf.create nf in
  let rec add v = match Uunf.add n v with
  | `Uchar u -> Uutf.Buffer.add_utf_8 b u; add `Await
  | `Await | `End -> ()
  in
  let add_uchar _ _ = function
  | `Malformed _ -> add (`Uchar Uutf.u_rep)
  | `Uchar _ as u -> add u
  in
  Uutf.String.fold_utf_8 add_uchar () s; add `End; Buffer.contents b
]}

   Note that this functionality is available directly through
   {!Uunf_string.normalize_utf_8}
*)

(*---------------------------------------------------------------------------
   Copyright (c) 2012 Daniel C. Bünzli

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
