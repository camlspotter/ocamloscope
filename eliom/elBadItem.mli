open Html5_types
open Eliom_content.Html5

type is = Item.t (** short look *) * int (** look_length *)
          * (OCamlFind.Packages.t * (int * Item.t) list) list

type bad_is = (int * Item.t) list

val to_bad : is -> bad_is

val group : (int * Item.t) -> [> tr ] D.elt
