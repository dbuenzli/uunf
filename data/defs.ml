  type uchar = int

  module Dset = struct                               (* diet character sets. *)
    type t = 
    | Empty 
    | C of uchar
    | R of uchar * uchar
    | Cn of t * t * uchar
    | Rn of t * t * uchar * uchar
        
    let rec mem s cp = match s with 
    | Rn (l, r, us, ue) -> 
        if cp < us then (mem l cp) else 
        if cp > ue then (mem r cp) else 
        true
    | R (us, ue) -> us <= cp && cp <= ue
    | Cn (l, r, u) ->
        if cp < u then mem l cp else 
        if cp > u then mem r cp else 
        true
    | C u -> cp = u
    | Empty -> false
  end
    
  module Dmap = struct                               (* diet character maps. *)
    type 'a t =                         
    | Empty
    | C of uchar * 'a
    | R of uchar * uchar * 'a 
    | Cn of 'a t * 'a t * uchar * 'a
    | Rn of 'a t * 'a t * uchar * uchar * 'a 
        
    let rec find ~default m cp = match m with 
    | Rn (l, r, us, ue, v) -> 
        if cp < us then find ~default l cp else
        if cp > ue then find ~default r cp else
        v
    | R (us, ue, v) -> if us <= cp && cp <= ue then v else default
    | Cn (l, r, u, v) -> 
        if cp < u then find ~default l cp else
        if cp > u then find ~default r cp else
        v
    | C (u, v) -> if cp = u then v else default
    | Empty -> default
  end
    
  module Tmap = struct                               (* Trie character maps. *)
    type 'a t = 'a array array array 

    let nil = [||]
    let find ~default m u = 
      let m = Array.unsafe_get m (u lsr 12) in
      if Array.length m = 0 then default else
      let m = Array.unsafe_get m (u lsr 4 land 0xFF) in 
      if Array.length m = 0 then default else 
      Array.unsafe_get m (u land 0xF)
  end
