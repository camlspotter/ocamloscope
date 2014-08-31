(** Types *)

(** Representation of a simple parameter name *)
type simple_name = {
    sn_name : string ;
    sn_type : Types.type_expr ;
    mutable sn_text : Odoc_types.text option ;
  }

(** Representation of parameter names. We need it to represent parameter names in tuples.
   The value [Tuple ([], t)] stands for an anonymous parameter.*)
type param_info =
  | Simple_name of simple_name
  | Tuple of param_info list * Types.type_expr

(** A parameter is just a param_info.*)
type parameter = param_info
