type t = { 
  path : Path.t;
  loc  : Location.t;
  kind : Types.type_expr Item.kind;
  env  : (Ident.t * Path.t) list;  (** To replace local idents in [kind] by paths *)
}

val reset_envs : unit -> unit

val structure : 
  Path.t 
  -> Typedtree.structure 
  -> t list (** extracted items *) 

val signature : 
  Path.t 
  -> Typedtree.signature 
  -> t list (** extracted items *) 

val get_predefined : unit -> t list
