module CMIDigest : sig
  type t with conv(ocaml)
  (** This is OCaml module interface checksum. Not file content MD5 checksum. *)

  val to_string : t -> string
end

val cmi_md5 : Module_path.t -> (string * CMIDigest.t) option
(** For the given module path, returns its cmi file path
    and signature md5 checksum. *)

val load_archive : 
  FileDigest.tbl 
  -> (modname:string -> digest:CMIDigest.t -> Module_path.t option) 
  -> string list  (** file list in archive() of META file *)
  -> (Module_path.t * CMIDigest.t) list (** targets *)
     * (Module_path.t * string list (* ml path *) * CMIDigest.t option) list (** tops *)

val cmi_without_value : string -> bool
(** [cmi_without_value cmipath] returns [true] if the cmi file contains no value
    components. If true, the file *may* be built from an mli only module *)

val find_cmi_file : string -> string option
(** [find_cmi_file "list"] finds "list.cmi" in the current [Config.load_path] *)
