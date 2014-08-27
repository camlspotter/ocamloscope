type t

val arg_specs : (string * Arg.spec * string) list

val fun_ : (unit -> unit) -> t
val label : string -> t -> t
val list : t list -> t

val run : bool (** default_go *) -> t -> unit
