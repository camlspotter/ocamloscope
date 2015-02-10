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
  | Type       of 'typ list (** type params *) * 'typ option * [ `Abstract | `Record | `Variant | `Open ]
  | Value      of 'typ 
  | Package    of OCamlFind.Package.t * string list (** top modules, ex. "Dbm" *)
(* CR jfuruse: Exception is a special case of `Open Type *)

val kindkey_of_kind : 'a kind -> Kindkey.extracted

val name_of_kind : 'a kind -> string

val types_of_kind : 'a kind -> 'a list

type ('packs, 'path, 'loc, 'doc, 'typ)  record = {
    packs : 'packs;
    path  : 'path;
    loc   : 'loc;
    doc   : 'doc;
    kind  : 'typ kind;
  }

type t = (OCamlFind.Packages.t,
	  Spath.t, 
	  Loc.t option, 
	  (OCamlDoc.t option, unit) Result.t,
          Stype.t) record
[@@deriving conv{ocaml}]

type pooled_type = 
  | Not_pooled of Stype.t
  | Pooled of int

type pooled = (OCamlFind.Packages.t,
	       Spath.t, 
	       Loc.t option, 
	       (OCamlDoc.t option, unit) Result.t,
	       pooled_type) record
[@@deriving conv{ocaml}]

val rec_hcons : t -> t
val format : Format.t -> t -> unit
val format_gen : ?dont_omit_opened: bool -> Format.t -> t -> unit

val arity_of_item : t -> int
(** -1 for items with no type. *)

val sort_items_by_arity : t array -> t array
(** Sort them from no type, arity 0 .. .
    Believed to help the efficiency of the type match algorithm.
*)

val type_of_item : (_, _, _, _, 'a) record -> 'a option

val pack_types : t array -> unit
