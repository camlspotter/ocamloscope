module DB : sig
  type t = {
    items : Item.t array;
    ocamlfind_opam_table : (OCamlFind.Package.t * OPAM.package option) list;
    (** List of the top OCamlFind packages
        and the OPAM package which installed it if exists.
  
        Note: it lists only the top packages.
    *)
  }
end

module PooledDB : sig
  type t = {
    items : Item.pooled array;
    types : Stype.t array;
    ocamlfind_opam_table : (OCamlFind.Package.t * OPAM.package option) list;
    (** List of the top OCamlFind packages
        and the OPAM package which installed it if exists.
  
        Note: it lists only the top packages.
    *)
  }

  val poolize : DB.t -> t
end

val dump_items : unit -> unit
(**
   Scan OCamlFind and OPAM installation directories and dump 
   oco_*.bin
*)

val load_items : unit -> DB.t
(** Load oco_*.bin files exist in the current dir and its subdirs *)

