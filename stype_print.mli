open Spotlib.Spot
open Stype_core

val oformat : Format.t -> t -> unit

val print : (Spath.t -> Printer.t) (** Spath printer *)
            -> t
            -> Printer.t
                 
val format_gen : (Spath.t -> Printer.t) (** Spath printer *)
             -> Format.t
             -> t
             -> unit                                            

val format : Format.t
             -> t
             -> unit                                            

val to_string : t -> string

val show : t -> string
(** Very precise printing aimed to be [read] *)

val read : string -> (t, string) Result.t
(** Unfortunately, [read (show ty) <> ty]. It only assures the alpha-equiv.

    This is since [show] discard type variable indices and rename them
    if necessary.
*)

val cannot_read : t -> bool
(** Some types made by ocamloscope is not parsable for XParser for now.
   For example,

   type record = { id : 'a . 'a -> 'a }

   val id : record -> 'a . 'a -> 'a

   We must skip this kind of type from show-read tests.
*)

