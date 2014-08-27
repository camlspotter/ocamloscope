(* dirty hashconsing 

   I know I should use Hashcons by Jean-Christophe Filliatre 
   or something equivalent, but it enforces changes of types,
   which seems to be hard as far as I use OCaml's type_expr.

*)

open Spotlib.Spot

module String = struct
  include Hashcons.Make(struct
    type t = string
    let hash = Hashtbl.hash
    let equal (s : string) s' = s = s'
    let name = "string"
  end)
end

let string = String.non_rec_hcons

module Pos = struct
  include Hashcons.Make(struct
    type t= Lexing.position
    let hash = Hashtbl.hash
    let equal p p' = p = p'
    let name = "Lexing.position"
  end)
end

let position p = 
  let open Lexing in
  Pos.non_rec_hcons { p with pos_fname = string p.pos_fname }    

module L = struct
  open Location
  include Hashcons.Make(struct
    type t = Location.t
    let hash = Hashtbl.hash
    let equal x x' = 
      x.loc_start == x'.loc_start
      && x.loc_end == x'.loc_end
      && x.loc_ghost = x'.loc_ghost
    let name = "Location.t"
  end)
end

let location t = 
  let open Location in
  L.non_rec_hcons { t with loc_start = position t.loc_start;
                           loc_end = position t.loc_end }
