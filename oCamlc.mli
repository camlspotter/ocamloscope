val get_version : unit -> string

val is_ocaml_source_root : string -> bool
(** Check the given directory contains OCaml compiler source specific files *)

val find_ocaml_source_root : 
  unit (** to prepare memoization *)
  -> string 
  -> (string * string list) option
(** Find the OCaml compiler source root toward ancestor directories.
    With memoization. You must give a directory name. *)

    
