open Spotlib.Spot

type t = private
  | SPpredef
  | SPpack of OCamlFind.Packages.t (** can be linked into more than one package *) 
  | SPident of string
  | SPdot of t * string
  | SPapply of t * t
  | SPattr of attr * t 

and attr = 
  [ `Pack of t * (OCamlFind.Packages.t * (string * string option))
  | `Ident of t * (string * string option)
  | `AfterDot of string * string option
  ]
with conv(ocaml_of)

val predef : t
val pack : OCamlFind.Packages.t -> t
val ident : string -> t
val dot : t -> string -> t
val apply : t -> t -> t

val nhc_pack : OCamlFind.Packages.t -> t
val nhc_ident : string -> t
val nhc_dot : t -> string -> t
val nhc_apply : t -> t -> t
val nhc_attr : attr -> t -> t

val strip_attr : t -> t
val is_attred : t -> bool

val non_rec_hcons : t -> t
val rec_hcons : t -> t

val of_path : Path.t -> t
val to_path : t -> Path.t

val package_path : string -> Path.t

val look_same : t -> t -> bool

val of_longident : Longident.t -> t
(** This does not produce Hashcons'ed t *)

val to_longident : t -> Longident.t

val print : 
  ?packages: [`Exact | `Nick | `ID]
  -> ?remove_package_names:bool
  -> ?just_postfix:bool
  -> ?opened:t
  -> ?predef:bool
  -> t
  -> Printer.t

val format : 
  ?packages: [`Exact | `Nick | `ID]
  -> ?remove_package_names:bool
  -> ?just_postfix:bool
  -> ?opened:t
  -> ?predef:bool
  -> unit
  -> Format.t -> t -> unit

val show : t -> string

val read : string -> (t, string) Result.t
(** No hcons *)

val test_read : t -> unit

val short_look : t -> t
(** Short look of the given path

        Module.Module.name => name
        Module.Module.t => Module.t    if one char
        Module.A.t => A.t
*)
