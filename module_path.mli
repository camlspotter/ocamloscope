type t [@@deriving conv{ocaml}]
(** Abstraction of directory name + module name: ex. "/usr/local/lib/ocaml/List" *)

external of_string : string -> t = "%identity" 
(** Direct conversion. May break invariants. Use with care *)

external to_string : t -> string = "%identity" 
(** String conversion *)

val file : string -> t -> string option
(** [file ext t] returns the file path of [ext] file of module [t].
    [file ".cmi" ``/usr/local/lib/ocaml/List`` = Some "/usr/local/lib/ocaml/list.cmi"]
    if the file exists.
*)

val modname : t -> string
(** /usr/local/lib/ocaml/List => List *)

val of_path : string -> t
(** /usr/local/lib/ocaml/list.cmi => /usr/local/lib/ocaml/List *)
