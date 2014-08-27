open Spotlib.Spot
open Query

module User : sig
  type t = int
end

type key = Query.t list

type error = [ `Checksum_failure | `Shrink_too_many ]

val query : 
  Item.t array 
  -> User.t 
  -> key option 
  -> QueryResult.t * (bool, [> error]) Result.t

val search :
  Item.t array 
  -> User.t 
  -> string
  -> QueryResult.t * (bool, [> error]) Result.t

val clear : unit -> unit    

val print_state : unit -> unit

