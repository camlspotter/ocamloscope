(** OCamlFind tools *)

module Package : sig

  type t = { 
    name : string;
    dir  : string;
    defs : (string * string) list
  } [@@deriving conv{ocaml}]
  (** Simplified version of [Fl_package_base.package] *)

  val name : t -> string
  (** Get package name. *)
  
  val find_var : string -> t -> string option
  (** Find a variable binding *)

  val version : t -> string option
  (** The version of the package *)

  val is_distributed_with_ocaml : t -> bool
  (** version = "[distributed with Ocaml]" *)

  val requires : t -> string list option
  (** Required packages. [None] means the "requires" field itself is not found *)

  val top_name : t -> string
  (** returns the top packgae name: i.e. "camlimages" for "camlimages.core" *)

  val is_top : t -> bool
  (** [true] if the package is a top one *)

  val parse_browse_interfaces : t -> string list option
  (** Wierd hack for "distributed with Ocaml" things 
      
      META for the packages from OCaml distribution contains
      a strange field browse_interfaces. It is a very strange string
      but helps to know which modules belong to which base package.
  *)

  val has_browse_interfaces : t -> bool
  (** [true] if the package has "browse_interface" field *)

  val group : t list -> (string, t list) Hashtbl.t
  (** Group packages by their top package names *)
end

type ocamlfind
(** Witness of initialization *)

val init : unit -> ocamlfind
(** Initialization of FindLib *)

val get_packages : ocamlfind -> Package.t list
(** Get the installed packages *)

val get_stdlib_dir : ocamlfind -> string
(** Get the stdlib directory name *)

val installed_cmi_resolver : 
  Package.t 
  -> modname: string 
  -> digest:Cmfile.CMIDigest.t 
  -> Module_path.t option
(** Scan the installation directory of the package and list up all the cmi files 
    with their signature digests.

    This hack is required since some modules are linked into a cma but their cmi's
    are installed in a sub directory and not in the directory specified by META.
    ex. CamlP4.
 *)

type modules = {
  targets : (Module_path.t * Cmfile.CMIDigest.t) list;
  (** The linked modules *)

  reachable_tops : (Module_path.t 
                    * string list (** ml path *) 
                    * Cmfile.CMIDigest.t (** cmi md5 *) option) list 
  (** Compilation units linked in. If the library is packed, this contains
      lot more modules than [targets]. *)
}

val get_modules : 
  FileDigest.tbl
  -> stdlib_dir:string 
  -> Package.t 
  -> modules
(** Get the modules of the package *)

val find_packages : 
  (Package.t * modules) list  (** The packages and their modules *)
  -> file_path: string (** Path to an existing file. It can be outside of OCamlFind installation directory *)
  -> (Package.t * string list) list option
(** Given file path of the module, deduce the packages which provide it.
    The corresponding cmi file is required.
*)
(* CR jfuruse: this should return Spath.t list option. (But currently it introduce circular deps) *)

val choose_best_package_name : string list -> string
(** Choose the shortest and canonical name of the given package names.
    
    [ choose_best_package_name [ "camlimages"; "camlimages.core" ] 
      = "camlimages" ]

    [ choose_best_package_name [ "camlimages.core"; "camlimages.exif" ] 
      = "camlimages.core" ] (* alphabetical order is chosen *)
 *)

module Packages : sig
  type t [@@deriving conv{ocaml}]
  (** set of packages *)

  val to_strings : t -> string list
  val of_strings : string list -> t

  val hcons : t -> t

  val to_id : t -> string
  (** Name of the packages. {foo#92} *)

  val of_id : string -> t
  (** Retrieve a known set of packages from its name. *)
    
  val to_string_for_printing : t -> string
  (** Name of the packages, without the id integer: {foo} *)

  val exact_string_of : t -> string
  (** "{foo,foo.core,foo.blah}" *)

  val compare : t -> t -> int
  (** {stdlib#1} has the lowest value *)

  val report : unit -> unit
  (** Print out the number of package sets registered *)

  val match_ : string -> t -> bool
  (** "aaa" matches with {aaa} and also with {aaa.bbb, ccc} *)

  val cached_match : string -> t -> bool
  (** Fast version of [match_]. Valid only when the whole package sets become stable. *)
end
