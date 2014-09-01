open Stype_core

val non_rec_hcons : t -> t
val rec_hcons : t -> t

val non_rec_hcons_datatype : datatype -> datatype
val rec_hcons_datatype : datatype -> datatype

module HashedType : sig
  include Hashtbl.HashedType with type t = Stype_core.t
end

