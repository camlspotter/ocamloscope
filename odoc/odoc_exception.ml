type exception_alias = {
    ea_name : Name.t ;
    mutable ea_ex : t_exception option ;
  }

and t_exception = {
    ex_name : Name.t ;
    mutable ex_info : Odoc_types.info option ; (** optional user information *)
    ex_args : Types.type_expr list ; (** the types of the parameters *)
    ex_alias : exception_alias option ;
    mutable ex_loc : Odoc_types.location ;
    mutable ex_code : string option ;
  }
