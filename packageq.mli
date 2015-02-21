type t =
  | Exclude of string (** -spotlib *)

val to_string : t -> string

val parse : string -> t option
  
val parse_query : string -> t list * string
(** try to extract package specifications from a query line *)

val compile : t list -> OCamlFind.Packages.t -> bool
(** Compile to a query funciton with cache *)
