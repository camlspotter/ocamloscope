val fix_path : 
  (string (** module name *)
   -> (OCamlFind.Package.t * string list (** ml path *)) list option (** the module *))
  -> (Ident.t * Path.t) list (** Local ident tbl *)
  -> Path.t 
  -> Spath.t

val convert_kind : (Path.t -> Spath.t) -> Types.type_expr Item.kind -> Stype.t Item.kind

