  module Tmap = struct                               (* Trie character maps. *)
    type 'a t = 'a array array array       (* 0x1FFFFF as 0x1FF - 0xFF - 0xF *)
    let nil = [||]
    let l0_shift = 12 
    let l0_size = 272 (* 0x10F + 1 *)
    let l1_shift = 4
    let l1_size = 256 (* 0xFF + 1 *)
    let l1_mask = 0xFF
    let l2_mask = 0xF
    let l2_size = 16  (* 0xF + 1 *)
    let get ~default m u = 
      let m = Array.unsafe_get m (u lsr l0_shift) in
      if m == nil then default else
      let m = Array.unsafe_get m (u lsr l1_shift land l1_mask) in 
      if m == nil then default else 
      Array.unsafe_get m (u land l2_mask)
  end

  module Tbmap = struct                       (* Trie character boolean map. *)
    type 'a t = string array array         (* 0x1FFFFF as 0x1FF - 0xF - 0xFF *)
    let nil = [||]
    let snil = ""
    let l0_shift = 12 
    let l0_size = 272 (* 0x10F + 1 *)
    let l1_shift = 8 
    let l1_size = 16 (* 0xF + 1 *)
    let l1_mask = 0xF
    let l2_mask = 0xFF
    let get ~default m u = 
      let m = Array.unsafe_get m (u lsr l0_shift) in 
      if m == nil then default else 
      let m = Array.unsafe_get m (u lsr l1_shift land l1_mask) in 
      if m == snil then default else 
      let l2 = u land l2_mask in
      let byte_num = l2 lsr 3 (* / 8 *) in 
      let bit_num = l2 land 7 (* mod 8 *) in
      let byte = Char.code (String.unsafe_get m byte_num) in
      byte land (1 lsl bit_num) > 0
  end
