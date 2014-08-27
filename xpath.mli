val escape_operator : string -> string
val name : Path.t -> string

val remove_package_path : Path.t -> Path.t
(** {pack}.P => P *)
