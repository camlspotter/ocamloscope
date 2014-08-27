(** Hashconsing. *)

(* CR jfuruse: very lousy. Need to be rewritten *)

type 'a hcons
external (!>) : 'a hcons -> 'a = "%identity"

val clear_all_tables : unit -> unit
(** clear all the tables created by [Make] *)

val report : unit -> unit
(** report all the table usages *)

module Make(A : sig
  include Hashtbl.HashedType
  val name : string
end) : sig
  module Elem : Hashtbl.HashedType with type t = A.t
  val non_rec_hcons : A.t -> A.t
  val clear : unit -> unit 
end
