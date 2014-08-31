open Stype_core

val of_core_type : Parsetree.core_type -> t
(** This is only for types in search, so no hashconsing *)

val to_type_expr : ?anon_parameter_as_constr:bool -> (Spath.t -> Path.t) -> t -> Types.type_expr
(** Alias of Stype_core.to_type_expr. May raise Stype_core.Unsupported *)

val of_type_expr : (Path.t -> Spath.t) -> Types.type_expr -> t

val of_type_exprs : (Path.t -> Spath.t) -> Types.type_expr list -> t list
