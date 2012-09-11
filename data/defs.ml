  module Tmap = struct                               (* Trie character maps. *)
    type 'a t = 
      { default : 'a;                                      (* default value. *)
        l0 : 'a array array array }       (* 0x1FFFFF as 0x1FF - 0xFF - 0xF. *)

    let nil = [||]
    let l0_shift = 12 
    let l0_size = 272 (* 0x10F + 1 *)
    let l1_shift = 4
    let l1_mask = 0xFF
    let l1_size = 256 (* 0xFF + 1 *)
    let l2_mask = 0xF
    let l2_size = 16  (* 0xF + 1 *)
    let get m u = 
      let l1 = Array.get m.l0 (u lsr l0_shift) in
      if l1 == nil then m.default else
      let l2 = Array.unsafe_get l1 (u lsr l1_shift land l1_mask) in 
      if l2 == nil then m.default else 
      Array.unsafe_get l2 (u land l2_mask)
  end

  module Tboolmap = struct                     (* Trie character boolean map. *)
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
      let l1 = Array.get m.l0 (u lsr l0_shift) in 
      if l1 == nil then m.default else
      let l2 = Array.unsafe_get l1 (u lsr l1_shift land l1_mask) in 
      if l2 == snil then m.default else 
      let k = u land l2_mask in
      let byte_num = k lsr 3 (* / 8 *) in 
      let bit_num = k land 7 (* mod 8 *) in
      let byte = Char.code (String.unsafe_get l2 byte_num) in
      byte land (1 lsl bit_num) > 0
  end

  module Tbytemap = struct                       (* Trie character byte map. *)
    type t = 
      { default : int;                                     (* default value. *)
        l0 : string array array }          (* 0x1FFFFF as 0x1FF - 0xF - 0xFF *)

    let nil = [||]
    let snil = ""
    let l0_shift = 12 
    let l0_size = 272 (* 0x10F + 1 *)
    let l1_shift = 8 
    let l1_mask = 0xF
    let l1_size = 16 (* 0xF + 1 *)
    let l2_mask = 0xFF
    let l2_size = 256 (* 0xFF + 1 *)
    let get m u = 
      let l1 = Array.get m.l0 (u lsr l0_shift) in 
      if l1 == nil then m.default else
      let l2 = Array.unsafe_get l1 (u lsr l1_shift land l1_mask) in 
      if l2 == snil then m.default else 
      Char.code (String.unsafe_get l2 (u land l2_mask))
  end
