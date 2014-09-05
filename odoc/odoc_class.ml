(** To keep the order of elements in a class *)
type class_element =
    Class_attribute of Odoc_value.t_attribute
  | Class_method of Odoc_value.t_method
  | Class_comment of Odoc_types.text

(** Used when we can reference t_class or t_class_type. *)
type cct =
    Cl of t_class
  | Cltype of t_class_type * Types.type_expr list (** class type and type parameters *)

and inherited_class = {
    ic_name : Name.t ; (** Complete name of the inherited class *)
    mutable ic_class : cct option ; (** The associated t_class or t_class_type *)
    ic_text : Odoc_types.text option ; (** The inheritance comment, if any *)
  }

and class_apply = {
    capp_name : Name.t ; (** The complete name of the applied class *)
    mutable capp_class : t_class option;  (** The associated t_class if we found it *)
    capp_params : Types.type_expr list; (** The type of expressions the class is applied to *)
    capp_params_code : string list ; (** The code of these expressions *)
  }

and class_constr = {
    cco_name : Name.t ; (** The complete name of the applied class *)
    mutable cco_class : cct option;  (** The associated class ot class type if we found it *)
    cco_type_parameters : Types.type_expr list; (** The type parameters of the class, if needed *)
  }


and class_kind =
    Class_structure of inherited_class list * class_element list
        (** an explicit class structure, used in implementation and interface *)
  | Class_apply of class_apply (** application/alias of a class, used in implementation only *)
  | Class_constr of class_constr (** a class used to give the type of the defined class,
                                    instead of a structure, used in interface only.
                                    For example, it will be used with the name "M1.M2....tutu"
                                    when the class to is defined like this :
                                    class toto : int -> tutu *)
  | Class_constraint of class_kind * class_type_kind
        (** A class definition with a constraint. *)

(** Representation of a class. *)
and t_class = {
    cl_name : Name.t ; (** Name of the class *)
    mutable cl_info : Odoc_types.info option ; (** The optional associated user information *)
    cl_type : Types.class_type ;
    cl_type_parameters : Types.type_expr list ; (** Type parameters *)
    cl_virtual : bool ; (** true = virtual *)
    mutable cl_kind : class_kind ;
    mutable cl_parameters : Odoc_parameter.parameter list ;
    mutable cl_loc : Odoc_types.location ;
  }

and class_type_alias = {
    cta_name : Name.t ;
    mutable cta_class : cct option ; (** we can have a t_class or a t_class_type *)
    cta_type_parameters : Types.type_expr list ; (** the type parameters *)
  }

and class_type_kind =
    Class_signature of inherited_class list * class_element list
  | Class_type of class_type_alias (** a class type eventually applied to type args *)

(** Representation of a class type. *)
and t_class_type = {
    clt_name : Name.t ;
    mutable clt_info : Odoc_types.info option ; (** The optional associated user information *)
    clt_type : Types.class_type ;
    clt_type_parameters : Types.type_expr list ; (** type parameters *)
    clt_virtual : bool ; (** true = virtual *)
    mutable clt_kind : class_type_kind ;
    mutable clt_loc : Odoc_types.location ;
  }
