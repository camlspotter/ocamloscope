open Html5_types
open Eliom_content.Html5

(*
val item : int (** distance *) * int (** id *) * Item.t -> [> tr ] D.elt
*)

val group : 
  int * (Item.t (** short look *) * int (** look_length *)
         * (OCamlFind.Packages.t * (int * Item.t) list) list) 
  -> [> tr ] D.elt
