val dump : bool
(** do dump or not *)

val compiler_source_dir : string
(** compiler source code directory, to obtain stdlib cmts *)

val args : string list
(** args without option flags *)

val data_dir : string

val show_ocamldoc_message : bool
val show_cache_loading : bool
val show_scanned_ocamlfind_module_list : bool
val show_stat : bool

val prof_match_types : bool
