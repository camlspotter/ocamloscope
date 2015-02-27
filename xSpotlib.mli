(* This should be ported / already ported to spotlib *)

module Exn : sig
  (** lwt does not like ~finally *)
  val protect : ('a -> 'b) -> 'a -> final:('a -> unit) -> 'b
end

module Array : sig
  val shuffle : ?random:(int -> int) -> 'a array -> unit
end

module Base : sig
  val (!++) : int ref -> int
  (** Get then incr. *)

  val timed_message : string -> ('a -> 'b) -> 'a -> 'b
end

module Gc : sig
  val used_words : unit -> int 
  (** Counts used words. Maybe dangerous since it forces cut the size of
      the minor heap temporalily to swipe data out to the major 
      as much as possible *)

  val with_big_compacts : ('a -> 'b) -> 'a -> ('b * (int * int))
end

module List : sig

  val create : int -> (int -> 'a) -> 'a list
  (** like Array.create. Creation is from the head *)

end
