open Spotlib.Spot
open Odoc_types

(* CR jfuruse: [kind] should be moved to more general place *) 
type kind = 
  [ `Type 
  | `Value 
  | `Class 
  | `Class_type 
  | `Exception 
  | `Module 
  | `Module_type 
  ]
with conv(ocaml)

type t = Odoc_types.info with conv(ocaml) (** as info string *)

val oformat : Format.t -> t -> unit
(** as an OCaml value *)

val format : Format.t -> t -> unit
(** Human readable *)

val to_string : t -> string (* CR jfuruse: very rough *) 
(** Human readable *)

type entry = string * location * t * kind

val oformat_entry : Format.t -> entry -> unit

type error = string list * [ `Chdir 
                           | `Exec of Unix.process_status * string list
                           | `Load_dump of string ]
with conv(ocaml)

val docs_of_cmt : Cmt_format.cmt_infos -> (entry list, error) Result.t
