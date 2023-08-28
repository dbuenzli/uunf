(*---------------------------------------------------------------------------
   Copyright (c) 2012 The uunf programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type ret = [ `Uchar of Uchar.t | `End | `Await ]

let pp_ret ppf v = match (v :> ret) with
| `Uchar u -> Format.fprintf ppf "`Uchar U+%04X" (Uchar.to_int u)
| `End -> Format.fprintf ppf "`End"
| `Await -> Format.fprintf ppf "`Await"

let err_exp_await add =
  invalid_arg (Format.asprintf "can't add %a, expected `Await" pp_ret add)

let err_ended add =
  invalid_arg (Format.asprintf "can't add %a, `End already added" pp_ret add)

(* The normalization process is implemented as described in UAX #15
   section 9.1 for normalizing the concatenation of normalized
   strings.  We detect ranges of characters in the input sequence
   enclosed between two characters for which NFX_quick_check=YES *and*
   ccc = 0 (6.1.0 wrongly claims that quick_check=YES implies ccc = 0,
   we therefore call this property nfx_boundary).  Only these ranges
   (including the left boundary) need to be bufferized to perform the
   normalization process. *)

(* Characters *)

let ux_none = max_int                      (* no char, outside unicode range. *)
let u_dumb =                                     (* placeholder, overwritten. *)
  `Uchar (Uchar.of_int 0x0000)

(* Normalization properties. *)

let unicode_version = Uunf_data.unicode_version

let nfc_boundary u = Uunf_tmapbool.get Uunf_data.nfc_boundary_map u
let nfd_boundary u = Uunf_tmapbool.get Uunf_data.nfd_boundary_map u
let nfkc_boundary u = Uunf_tmapbool.get Uunf_data.nfkc_boundary_map u
let nfkd_boundary u = Uunf_tmapbool.get Uunf_data.nfkd_boundary_map u
let _ccc u = Uunf_tmapbyte.get Uunf_data.ccc_map u
let ccc u = _ccc (Uchar.to_int u)
let decomp_prop u = Uunf_tmap.get Uunf_data.decomp_map u
let compose_prop u = Uunf_tmap.get Uunf_data.compose_map u

module H = struct                            (* Hangul arithmetic constants. *)
  let sbase = 0xAC00
  let lbase = 0x1100
  let vbase = 0x1161
  let tbase = 0x11A7
  let scount = 11172
  let lcount = 19
  let vcount = 21
  let tcount = 28
  let ncount = 588
  let scount = 11172
end

let decomp u =
  let u = Uchar.to_int u in
  if u < 0xAC00 || 0xD7A3 < u then decomp_prop u else
  begin                                        (* LV or LVT hangul composite *)
    let sindex = u - H.sbase in
    let l = H.lbase + (sindex / H.ncount) in
    let v = H.vbase + (sindex mod H.ncount) / H.tcount in
    let t = H.tbase + (sindex mod H.tcount) in
    if t = H.tbase then [|l; v|] else [|l; v; t|]
  end

(* N.B. to help stream-safe text implementers we *could* use the bits
   25-27 of [(decomp u).(0)] to indicate the number of initial non
   starters in the NFKD decomposition of [u] and bits and 28-30 to
   indicate the non starter count increment. *)

let d_compatibility i = i land (1 lsl 24) > 0
let _d_uchar i = i land 0x1FFFFF
let d_uchar i = Uchar.unsafe_of_int (_d_uchar i)

let _composite u1 u2 =
  if 0x1100 <= u1 && u1 <= 0x1112 then
    begin
      if u2 < 0x1161 || 0x1175 < u2 then ux_none else
      let l = u1 - H.lbase in                         (* LV hangul composite *)
      let v = u2 - H.vbase in
      H.sbase + l * H.ncount + v * H.tcount
    end
  else if 0xAC00 <= u1 && u1 <= 0xD788 && (u1 - 0x0AC00) mod H.tcount = 0 then
    begin
      if u2 < 0x11A8 || u2 > 0x11C3 then ux_none else
      (u1 + u2 - H.tbase)                            (* LVT hangul composite *)
    end
  else match compose_prop u1 with
  | [||] -> ux_none
  | a (* [u2; c; u2'; c'; ...] sorted *) ->
      let len = Array.length a / 2 in
      let i = ref 0 in
      try
        while (!i < len) do
          if a.(!i * 2) = u2 then raise Exit else incr i;
        done;
        ux_none
      with Exit -> (a.(!i * 2 + 1))

let composite u1 u2 =
  let u = _composite (Uchar.to_int u1) (Uchar.to_int u2) in
  if u = ux_none then None else Some (Uchar.unsafe_of_int u)

(* Normalize *)

type form = [ `NFC | `NFD | `NFKC | `NFKD ]
type state =                                            (* normalizer state. *)
| Start                                                   (* no cp seen yet. *)
| Boundary    (* cp with boundary = true found in n.uc, no accumulation yet. *)
| Acc                      (* accumulate until next cp with boundary = true. *)
| Flush          (* next cp with boundary = true found, flush previous data. *)
| End                                      (* end of normalization sequence. *)

type t =
  { form : form;                                      (* normalization form. *)
    compat : bool;            (* true if compatibility decomposition needed. *)
    compose : bool;                           (* true if composition needed. *)
    boundary : int -> bool;                                 (* nfx_boundary. *)
    mutable state : state;                              (* normalizer state. *)
    mutable uc : [`Uchar of Uchar.t];       (* last cp with boundary = true. *)
    mutable acc : int array;                      (* code point accumulator. *)
    mutable first : int;                (* index of first code point in acc. *)
    mutable last : int;                  (* index of last code point in acc. *)
    mutable is_end : bool;}                      (* [true] if `End was seen. *)

let create_acc () = Array.make 35 ux_none
let create form  =
  let boundary, compat, compose = match form with
  | `NFC -> nfc_boundary, false, true
  | `NFD -> nfd_boundary, false, false
  | `NFKC -> nfkc_boundary, true, true
  | `NFKD -> nfkd_boundary, true, false
  in
  { form = (form :> form); compat; compose; boundary; state = Start;
    uc = u_dumb; acc = create_acc (); first = 0; last = -1; is_end = false}

let get_u n = let `Uchar u = n.uc in Uchar.to_int u
let acc_empty n = n.first > n.last
let form n = n.form
let copy n = { n with acc = Array.copy n.acc }
let reset n =
  n.state <- Start; n.uc <- u_dumb; n.acc <- create_acc ();
  n.first <- 0; n.last <- -1; n.is_end <- false

let grow_acc n =
  let len = Array.length n.acc in
  let acc' = Array.make (2 * len) ux_none in
  Array.blit n.acc 0 acc' 0 len; n.acc <- acc'

let ordered_add n u =    (* canonical ordering algorithm via insertion sort. *)
  n.last <- n.last + 1; if n.last = Array.length n.acc then grow_acc n;
  let c = _ccc u in
  if c = 0 then n.acc.(n.last) <- u else
  begin
    let i = ref (n.last - 1) in
    while (!i >= 0 && _ccc (n.acc.(!i)) > c) do
      n.acc.(!i + 1) <- n.acc.(!i); decr i;                  (* shift right. *)
    done;
    n.acc.(!i + 1) <- u
  end

let rec add n u =
  if 0xAC00 <= u && u <= 0xD7A3 then
    begin (* LV or LVT hangul composite, copied from decomp to avoid alloc. *)
      let sindex = u - H.sbase in
      let l = H.lbase + (sindex / H.ncount) in
      let v = H.vbase + (sindex mod H.ncount) / H.tcount in
      let t = H.tbase + (sindex mod H.tcount) in
      if t = H.tbase then (ordered_add n l; ordered_add n v) else
      (ordered_add n l; ordered_add n v; ordered_add n t)
    end
  else
    begin match decomp_prop u with
    | [||] -> ordered_add n u
    | d ->
        if d_compatibility d.(0) && not n.compat then ordered_add n u else
        begin
          add n (_d_uchar d.(0));
          for i = 1 to Array.length d - 1 do add n d.(i) done
        end
    end

let compose n =                         (* canonical composition algorithm. *)
  let rec loop ~last_starter ~prev_ccc i =
    if i > n.last then () else
    let ccc_i = _ccc n.acc.(i) in
    let u_comp = _composite n.acc.(last_starter) n.acc.(i) in
    match (u_comp = ux_none || (ccc_i = 0 && last_starter <> i - 1)) with
    | true ->
        let last_starter = if ccc_i = 0 then i else last_starter in
        loop ~last_starter ~prev_ccc:ccc_i (i + 1)
    | false ->
        match prev_ccc <> 0 && prev_ccc >= ccc_i with
        | true -> loop ~last_starter ~prev_ccc:ccc_i (i + 1)
        | false ->
            n.acc.(last_starter) <- u_comp;
            Array.blit n.acc (i + 1) n.acc i (n.last - i);
            n.last <- n.last - 1;
            let prev_ccc = _ccc n.acc.(last_starter) in
            loop ~last_starter ~prev_ccc (last_starter + 1)
  in
  let last_starter = n.first in
  let prev_ccc = _ccc n.acc.(last_starter) in
  loop ~last_starter ~prev_ccc (last_starter + 1)

let flush_next n =
  let ret = `Uchar (Uchar.unsafe_of_int n.acc.(n.first)) in
  if n.first = n.last then (n.first <- 0; n.last <- -1) else
  (n.first <- n.first + 1);
  ret

let flush_start n = if n.compose then compose n; flush_next n
let add n = function
| `Uchar u as uc ->
    let u = Uchar.to_int u in
    begin match n.state with
    | Boundary ->
        if n.boundary u
        then (let prev = n.uc in n.uc <- uc; (prev :> ret))
        else (n.state <- Acc; add n (get_u n); add n u; `Await)
    | Acc ->
        if n.boundary u
        then (n.state <- Flush; n.uc <- uc; flush_start n)
        else (add n u; `Await)
    | Start ->
        if n.boundary u
        then (n.state <- Boundary; n.uc <- uc; `Await)
        else (n.state <- Acc; add n u; `Await)
    | Flush -> err_exp_await uc
    | End -> err_ended uc
    end
| `Await ->
    begin match n.state with
    | Flush ->
        if not (acc_empty n) then flush_next n else
        if n.is_end then (n.state <- End; `End) else
        (n.state <- Boundary; `Await)
    | Start | Boundary | Acc -> `Await
    | End -> `End
    end
| `End ->
    n.is_end <- true;
    begin match n.state with
    | Boundary -> n.state <- End; (n.uc :> ret)
    | Acc -> n.state <- Flush; flush_start n
    | Start -> n.state <- End; `End
    | Flush -> err_exp_await `End
    | End -> err_ended `End
    end
