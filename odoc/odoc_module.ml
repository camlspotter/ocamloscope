(** To keep the order of elements in a module. *)
type module_element =
    Element_module of t_module
  | Element_module_type of t_module_type
  | Element_included_module of included_module
  | Element_class of Odoc_class.t_class
  | Element_class_type of Odoc_class.t_class_type
  | Element_value of Odoc_value.t_value
  | Element_exception of Odoc_exception.t_exception
  | Element_type of Odoc_type.t_type
  | Element_module_comment of Odoc_types.text

(** Used where we can reference t_module or t_module_type *)
and mmt =
  | Mod of t_module
  | Modtype of t_module_type

and included_module = {
    im_name : Name.t ; (** the name of the included module *)
    mutable im_module : mmt option ; (** the included module or module type *)
    mutable im_info : Odoc_types.info option ; (** comment associated to the includ directive *)
  }

and module_alias = {
    ma_name : Name.t ;
    mutable ma_module : mmt option ; (** the real module or module type if we could associate it *)
  }

and module_parameter = {
    mp_name : string ; (** the name *)
    mp_type : Types.module_type ; (** the type *)
    mp_type_code : string ; (** the original code *)
    mp_kind : module_type_kind ; (** the way the parameter was built *)
  }

(** Different kinds of module. *)
and module_kind =
  | Module_struct of module_element list
  | Module_alias of module_alias (** complete name and corresponding module if we found it *)
  | Module_functor of module_parameter * module_kind
  | Module_apply of module_kind * module_kind
  | Module_with of module_type_kind * string
  | Module_constraint of module_kind * module_type_kind
  | Module_typeof of string (** by now only the code of the module expression *)
  | Module_unpack of string * module_type_alias (** code of the expression and module type alias *)

(** Representation of a module. *)
and t_module = {
    m_name : Name.t ;
    mutable m_type : Types.module_type ;
    mutable m_info : Odoc_types.info option ;
    m_is_interface : bool ; (** true for modules read from interface files *)
    m_file : string ; (** the file the module is defined in. *)
    mutable m_kind : module_kind ;
    mutable m_loc : Odoc_types.location ;
    mutable m_top_deps : Name.t list ; (** The toplevels module names this module depends on. *)
    mutable m_code : string option ; (** The whole code of the module *)
    mutable m_code_intf : string option ; (** The whole code of the interface of the module *)
    m_text_only : bool ; (** [true] if the module comes from a text file *)
  }

and module_type_alias = {
    mta_name : Name.t ;
    mutable mta_module : t_module_type option ; (** the real module type if we could associate it *)
  }

(** Different kinds of module type. *)
and module_type_kind =
  | Module_type_struct of module_element list
  | Module_type_functor of module_parameter * module_type_kind
  | Module_type_alias of module_type_alias (** complete name and corresponding module type if we found it *)
  | Module_type_with of module_type_kind * string (** the module type kind and the code of the with constraint *)
  | Module_type_typeof of string (** by now only the code of the module expression *)

(** Representation of a module type. *)
and t_module_type = {
    mt_name : Name.t ;
    mutable mt_info : Odoc_types.info option ;
    mutable mt_type : Types.module_type option ; (** [None] = abstract module type *)
    mt_is_interface : bool ; (** true for modules read from interface files *)
    mt_file : string ; (** the file the module type is defined in. *)
    mutable mt_kind : module_type_kind option ; (** [None] = abstract module type if mt_type = None ;
                                           Always [None] when the module type was extracted from the implementation file. *)
    mutable mt_loc : Odoc_types.location ;
  }
