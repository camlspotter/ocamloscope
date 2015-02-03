(*
val escape : string -> string
val unescape : string -> string
val magic : string
val escape_package : string -> string
*)
val escape_query : string -> string
(*
val unescape_package : string -> string
*)
val unescape_longident : Longident.t -> Longident.t
val unescape_core_type : Parsetree.core_type -> Parsetree.core_type
