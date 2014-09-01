open Stype_core

val non_rec_hcons : t -> t
val rec_hcons : t -> t

val non_rec_hcons_datatype : datatype -> datatype
val rec_hcons_datatype : datatype -> datatype

val pack_types : t list -> unit
