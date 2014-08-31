(** Types *)

(** Representation of a value. *)
type t_value = {
    val_name : Name.t ;
    mutable val_info : Odoc_types.info option ;
    val_type : Types.type_expr ;
    val_recursive : bool ;
    mutable val_parameters : Odoc_parameter.parameter list ;
    mutable val_code : string option ;
    mutable val_loc : Odoc_types.location ;
  }

(** Representation of a class attribute. *)
type t_attribute = {
    att_value : t_value ; (** an attribute has almost all the same information
                             as a value *)
    att_mutable : bool ;
    att_virtual : bool ;
  }

(** Representation of a class method. *)
type t_method = {
    met_value : t_value ; (** a method has almost all the same information
                             as a value *)
    met_private : bool ;
    met_virtual : bool ;
  }
