  module Tmap = struct                               (* Trie character maps. *)
    type 'a t = 'a array array array 

    let nil = [||]
    let find ~default m u = 
      let m = Array.unsafe_get m (u lsr 12) in
      if m == nil then default else
      let m = Array.unsafe_get m (u lsr 4 land 0xFF) in 
      if m == nil then default else 
      Array.unsafe_get m (u land 0xF)
  end

  module Tset = struct                              (* Trie character sets. *)
    type 'a t = string array array
    let nil = [||]
    let snil = ""
    let find ~default s u = 
      let s = Array.unsafe_get s (u lsr 12) in 
      if s == nil then default else 
      let s = Array.unsafe_get s (u lsr 8 land 0xF) in 
      if s == snil then default else 
      let bitnum = u land 0xFF in
      let k = bitnum lsr 3 (* / 8 *) in 
      let l = bitnum land 7 (* mod 8 *) in
      let byte = Char.code (String.unsafe_get s k) in
      byte land (1 lsl l) > 0
  end
