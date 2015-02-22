type t = private {
  digest : Digest.t option;
  pack   : [ `None | `OCamlc of string | `OPAM of string ];
  loc    : Location.t;
  alias  : [ `Direct | `Unknown ]
} [@@deriving conv{ocaml}]

val create : Digest.t option -> [`None | `OCamlc of string | `OPAM of string] -> Location.t -> [ `Direct | `Unknown ] -> t
(** No recursive rec cons *)

val format : Format.formatter -> t -> unit

val rec_hcons : t -> t

val id : t -> (string * string (** md5 in hex *) * int) option
