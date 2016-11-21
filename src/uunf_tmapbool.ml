(*---------------------------------------------------------------------------
   Copyright (c) 2012 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(* Trie character boolean maps *)

type t =
  { default : bool;                                    (* default value. *)
    l0 : string array array }          (* 0x1FFFFF as 0x1FF - 0xF - 0xFF *)

let nil = [||]
let snil = ""
let l0_shift = 12
let l0_size = 272 (* 0x10F + 1 *)
let l1_shift = 8
let l1_mask = 0xF
let l1_size = 16 (* 0xF + 1 *)
let l2_mask = 0xFF
let l2_size = 32 (* 0xFF + 1 / 8 *)
let get m u =
  let l1 = Array.unsafe_get m.l0 (u lsr l0_shift) in
  if l1 == nil then m.default else
  let l2 = Array.unsafe_get l1 (u lsr l1_shift land l1_mask) in
  if l2 == snil then m.default else
  let k = u land l2_mask in
  let byte_num = k lsr 3 (* / 8 *) in
  let bit_num = k land 7 (* mod 8 *) in
  let byte = Char.code (String.unsafe_get l2 byte_num) in
  byte land (1 lsl bit_num) > 0

let create default = { default; l0 = Array.make l0_size nil }
let set m u b =
  let l2_make m = Bytes.make l2_size (if m.default then '\xFF' else '\x00') in
  if b = m.default then () else
  let i = u lsr l0_shift in
  if m.l0.(i) == nil then m.l0.(i) <- Array.make l1_size snil;
  let j = u lsr l1_shift land l1_mask in
  if m.l0.(i).(j) == snil then
    m.l0.(i).(j) <- Bytes.unsafe_to_string (l2_make m);
  let k = u land l2_mask in
  let byte_num = k lsr 3 (* / 8 *) in
  let bit_num = k land 7 (* mod 8 *) in
  let byte = Char.code m.l0.(i).(j).[byte_num] in
  let new_byte =
    if b then (Char.unsafe_chr (byte lor (1 lsl bit_num))) else
    (Char.unsafe_chr (byte land lnot (1 lsl bit_num)))
  in
  Bytes.set (Bytes.unsafe_of_string m.l0.(i).(j)) byte_num new_byte

let size m = match m.l0 with
| [||] -> 3 + 1
| l0 ->
    let size = ref (3 + 1 + Array.length l0) in
    for i = 0 to Array.length l0 - 1 do match l0.(i) with
    | [||] -> ()
    | l1 ->
        size := !size + 1 + Array.length l1;
        for j = 0 to Array.length l1 - 1 do
          size := !size + 1 + ((String.length l1.(j) * 8) / Sys.word_size)
        done;
    done;
    !size

let pp = Format.fprintf
let dump ppf m =
  pp ppf "@,{ default =@ %b;@, l0 =@ " m.default;
  begin match m.l0 with
  | [||] -> pp ppf "nil"
  | l0 ->
      pp ppf "@,[|@,";
      for i = 0 to Array.length l0 - 1 do match l0.(i) with
      | [||] -> pp ppf "@,nil;@,"
      | l1 ->
          pp ppf "@,[|@,";
          for j = 0 to Array.length l1 - 1 do match l1.(j) with
          | "" -> pp ppf "@,snil;@,"
          | l2 ->
              pp ppf "@,\"";
              for k = 0 to String.length l2 - 1 do
                if k mod 16 = 0 && k > 0 then pp ppf "\\@\n ";
                pp ppf "\\x%02X" (Char.code l2.[k])
              done;
              pp ppf "\";@,";
          done;
          pp ppf "|];"
      done;
      pp ppf "|]"
  end;
  pp ppf "@,}"

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
