module Query : sig
  type t = { 
    kind  : Kindkey.search option;
    path  : Spath.t option;
    type_ : Stype.t option;
    dist0 : bool; (** true then search only distance=0 *)
  }

  val to_string : t -> string

  val parse : string -> t list option
end

module QueryResult : sig 
  type t = 
    [ `EmptyQuery
    | `Error
    | `Funny
    | `Ok of Query.t list 
             * (int (** dist *) 
                * (Item.t (** short look *) * int (** look_length *)
                   * (OCamlFind.Packages.t * (int * Item.t) list) list)
               ) list
             * float * float (** times *)
             * int (** size = number of items *) 
    ]

  val size : t -> int
end

val query :
  Item.t array
  -> Query.t list option
  -> QueryResult.t

val search :
  Item.t array
  -> string 
  -> QueryResult.t

val cui : Item.t array -> 'a

val cli : unit -> unit
