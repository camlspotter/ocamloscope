type private_flag = Asttypes.private_flag =
    Private | Public

(** Description of a variant type constructor. *)
type variant_constructor = {
    vc_name : string ;
    vc_args : Types.type_expr list ; (** arguments of the constructor *)
    vc_ret : Types.type_expr option ;
    mutable vc_text : Odoc_types.text option ; (** optional user description *)
  }

(** Description of a record type field. *)
type record_field = {
    rf_name : string ;
    rf_mutable : bool ; (** true if mutable *)
    rf_type : Types.type_expr ;
    mutable rf_text : Odoc_types.text option ; (** optional user description *)
  }

(** The various kinds of type. *)
type type_kind =
    Type_abstract
  | Type_variant of variant_constructor list
                   (** constructors *)
  | Type_record of record_field list
                   (** fields *)

(** Representation of a type. *)
type t_type = {
    ty_name : Name.t ;
    mutable ty_info : Odoc_types.info option ; (** optional user information *)
    ty_parameters : (Types.type_expr * bool * bool) list ;
                    (** type parameters: (type, covariant, contravariant) *)
    ty_kind : type_kind ;
    ty_private : private_flag;
    ty_manifest : Types.type_expr option; (** type manifest *)
    mutable ty_loc : Odoc_types.location ;
    mutable ty_code : string option;
  }
