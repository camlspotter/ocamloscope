open Spotlib.Spot

(** { 6 Data type and tools } *)

type t =
  | Link of [ `Linked of t (** Loop (and intermidiate state) *) 
            | `Stub (** Placeholder only for intermidiate state *) 
            ] ref
      (** Outside of Stype, Link has always [{contents= `Linked t }]
          and it indicates a loop in a type: the linked [t] is always
          a ancestor of the node. *)
  | Nil
  | VariantClosed of int 
      (** Nil for Variant

          In OCaml, the nodes [[ `A ]] in [[ `A ] -> [ `A ]] are 
          *not* shared, but they *were* shared in our hcons.
          To have the compatible results as possible, [VariantClosed] is now
          indexed to prevent sharing in one type.
          
          Between different types [[ `A ]] can be still shared
          if their [VariantClosed] indices are the same.
       *)
  | Any
  | VarNamed of int * string (* Named and non named distinction is lousy but it should save some memory *) 
  | UnivarNamed of int * string
  | Var of int
  | Univar of int
  | Arrow of string * t * t
  | Tuple of t list
  | Constr of datatype * t list
  | Object of ((string * t) list * [ `Closed | `Open of t]) option (** This is always Some unless it is created by Ptyp_class *)
              * (Spath.t * t list) option (** named class info *)
  | Alias of t * string
  | Variant of xrow
  | Poly of t * t list
  | Package of Spath.t * (Spath.t * t) list
  | Attr of attr * t (** only used for printing *)

and attr = 
  [ `Ref of t ]

and xrow = {
  xrow_name : (Spath.t * t list) option; (* named type *)
  xrow_fields : xrow_fields;
  xrow_more : t; (** tvar for more *)
}

and xrow_fields = [
  | `Exact  of tag list               (** [ `A ] *)
  | `Open   of tag list               (** [> `A ] *)
  | `Closed of tag_full list * string list (** [< `A | `B | > `A ] *)
]

and tag = [
  | `Tag of string * t option
  | `Inherit of t
]

and tag_full = [
  | `Tag of string * bool (* zero arity exists *) * t list 
        (** false, []   :  impos
            true,  []   :  `A
            false, [t]  :  `A of t
            false, [ts] :  `A of t & t ..
            true,  [ts] :  `A of & t & t ..
        *)
  | `Inherit of t
]

and datatype = { dt_path : Spath.t;
                 dt_aliases : (t list * t) option option ref
               }

with conv(ocaml_of)

val oformat : Format.t -> t -> unit

val is_option : t -> t option
(** [is_option t] is returns [Some t'] when [t] is the option type for [t']. *)

val pathfix_for_printing : Path.t option -> Path.t -> Path.t
(* CR jfuruse: not sure it is still used or not *)

exception Unsupported

val to_type_expr : ?anon_parameter_as_constr: bool -> (Spath.t -> Path.t) -> t -> Types.type_expr
(** May raise Unsupported.

    [anon_parameter_as_const=true] if it is to print the type_expr.
    The anonymous parameter [_] in GADT is converted to a constr type
    named "_" for printing.
 *)

val format_via_type_expr : Format.t -> t -> unit
(** Print [t] using OCaml's printer for [type_expr]. May raise Unsupported *)

val to_string_via_type_expr : t -> string
(** May raise Unsupported *)
