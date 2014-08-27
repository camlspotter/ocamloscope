(** OPAM tools 

    Note that these functions do not use OPAM API but results of 
    opam cli commands, so they may be inaccurate, and are fragile to 
    any change of opam command output.
    
    CR jfuruse: I should change this to use OPAM API. But for now 
    it is not very stable.    
*)

open Spotlib.Spot

val get_prefix : unit -> string
(** Check OPAM installation and returns its prefix *)

type package = {
  name    : string;
  version : string;
  desc    : string;
  base    : bool; (** true means it is a virtual base package *)
} with conv(ocaml)

val format_package : Format.t -> package -> unit
val name_of_package : package -> string

val package_dir_of : string -> (string * string * string list) option
(** Given the path of a build file, deduce its OPAM switch and package 
    This is simply done by file path name:
    [package_dir_of ".../.opam/system/camlidl/com.a" = Some ("system", "camlidl", ["com.a"])]

    BUG: the path must contains ".opam" directory name.
*)

module Make(A : sig end) : sig
  
  val current_switch : string
  (** The current OPAM switch *)
  
  val opam_build_dir : string
  (** OPAM build dir *)
  
  val installed : package list
  (** Installed OPAM packages *)
  
  val package_of : string -> package option
  (** [package_of path] returns the package which the [path] belongs to.
      For example, [package_of ".../.opam/switch/foo.1.2.3/...."] returns
      the package of foo.1.2.3 if the current switch is "switch". *)

  val all_build_table : FileDigest.tbl lazy_t
  (** Digest table of all the built files in OPAM build directory *)
  
  val get_base_package : OCamlFind.Package.t -> package option
  (** Special mapping rule from OCamlFind base packages to OPAM package names.
      For example, for OCamlFind "bigarray" package, it returns OPAM package "base-bigarray".
  *)
  
  val guess_package :
    OCamlFind.Package.t  (** The package *)
    -> (string * string lazy_t) list  (** Installed file digests *)
    -> [> `Ambiguous
       | `Base  of package
       | `Found of package
       | `Maybe of package * int
       | `NotFound ]
  (** Guess OPAM package from OCamlFind package *)
  
  val guess_build_dir : base:string -> digest:Digest.t -> string list
  (** General file build dir guessing *)
  
end
  
  
