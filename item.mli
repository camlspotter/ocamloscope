open Spotlib.Spot
open Asttypes
(* open Conv *)

type 'typ kind = 
  | Class
  | ClassType
  | ClassField of virtual_flag * 'typ
  | Constr     of 'typ
  | Exception  of 'typ
  | Field      of 'typ
  | Method     of private_flag * virtual_flag * 'typ
  | ModType
  | Module
  | Type       of 'typ list (** type params *) * 'typ option * [ `Abstract | `Record | `Variant ]
  | Value      of 'typ 
  | Package    of OCamlFind.Package.t * string list (** top modules, ex. "Dbm" *)

val name_of_kind : 'a kind -> string
val types_of_kind : 'a kind -> 'a list

(* with conv(json) *)

type ('packs, 'path, 'loc, 'doc, 'typ)  record = {
    packs : 'packs;
    path  : 'path;
    loc   : 'loc;
    doc   : 'doc;
    kind  : 'typ kind;
    alias : 'path option;
  }

type t = (OCamlFind.Packages.t,
	  Spath.t, 
	  Loc.t option, 
	  (Odoc_info.info option, unit) Result.t,
	  Stype.t) record
with conv(ocaml_of)

val rec_hcons : t -> t
val format : Format.t -> t -> unit
val format_gen : ?dont_omit_opened: bool -> Format.t -> t -> unit
