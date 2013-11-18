(*---------------------------------------------------------------------------
   Copyright 2012 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Unicode text normalization.

    [Uunf] normalizes Unicode text.  It supports all Unicode
    normalization forms. The module is independent from any IO mechanism or
    Unicode text data structure and it can process text without a
    complete in-memory representation of the data.

    Consult the {{!basics}basics}, {{!limits}limitations} and
    {{!examples}examples} of use.

    {e Release %%VERSION%% — Unicode version %%UNICODEVERSION%% — 
       %%MAINTAINER%% }
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

(** {1 Characters} *)

type uchar = int
(** The type for Unicode characters. A value of this type {b must}
    be an Unicode
    {{:http://www.unicode.org/glossary/#unicode_scalar_value} scalar value} 
    which is an integer value in the ranges [0x0000]...[0xD7FF] 
    and [0xE000]...[0x10FFFF]. *)

val is_scalar_value : int -> bool
(** [is_scalar_value n] is [true] iff [n] is an Unicode 
    {{:http://www.unicode.org/glossary/#Unicode_scalar_value}scalar value}. *)

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

val create : [< form ] -> t
(** [create nf] is an Unicode text normalizer for the normal form [nf]. *)

val form : t -> form
(** [form n] is the normalization form of [n]. *)

val add : t -> [ `Uchar of uchar | `Await | `End ] -> 
[ `Uchar of uchar | `Await ]
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

(** {1:props Normalization properties} 

    These properties are used internally to implement the normalizers.
    They are not needed to use the module but are exposed as they may
    be useful to implement other algorithms. *)

val unicode_version : string 
(** [unicode_version] is the Unicode version supported by the module. *)

val ccc : uchar -> int
(** [ccc u] is [u]'s 
    {{:http://www.unicode.org/glossary/#combining_class}canonical combining 
    class} value. *)

val decomp : uchar -> int array
(** [decomp u] is [u]'s
    {{:http://www.unicode.org/glossary/#decomposition_mapping}decomposition
    mapping}. If the empty array is returned, [u] decomposes to itself. 

    The first number in the array contains additional information, it
    cannot be used as an {!uchar}. Use {!d_uchar} on the number to get the
    actual character and {!d_compatibility} to find out if this is
    a compatibility decomposition. All other characters of the array 
    are guaranteed to be of type {!uchar}.

    {b Warning.} Do {b not} mutate the array. *)

val d_uchar : int -> uchar
(** See {!decomp}. *)

val d_compatibility : int -> bool
(** See {!decomp}. *)

val composite : uchar -> uchar -> uchar option
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
| `Await -> acc
]}
    For example to normalize the character [U+00E9] (é) with [nfd] to a list 
    of characters we can write:
{[
let e_acute_nfd = List.rev (add (add [] (`Uchar 0x00E9)) `End)
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
  | `Await -> ()
  in
  let add_uchar _ _ = function 
  | `Malformed _ -> add (`Uchar Uutf.u_rep) 
  | `Uchar _ as u -> add u
  in
  Uutf.String.fold_utf_8 add_uchar () s; add `End; Buffer.contents b
]}
*)

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
