module Query : sig
  type t = { 
    kind  : Kindkey.search option;
    path  : Spath.t option;
    type_ : Stype.t option;
    dist0 : bool; (** true then search only distance=0 *)
    package : Packageq.t list;
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
  Load.PooledDB.t
  -> Query.t list
  -> QueryResult.t

val funny : Query.t -> bool

val search :
  Load.PooledDB.t
  -> string 
  -> QueryResult.t

val cui : Load.PooledDB.t -> 'loop

val cli : unit -> unit
