open Spotlib.Spot
open Query

module User : sig
  type t = int
end

type key = Query.t list

type error = [ `Checksum_failure | `Shrink_too_many ]

val query : 
  Load.PooledDB.t
  -> User.t 
  -> key
  -> QueryResult.t * (bool, [> error]) Result.t

val search :
  Load.PooledDB.t
  -> User.t 
  -> string
  -> QueryResult.t * (bool, [> error]) Result.t

val clear : unit -> unit    

val print_state : unit -> unit

