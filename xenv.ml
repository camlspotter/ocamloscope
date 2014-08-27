open Spotlib.Spot
open Env

module Summary = struct

  let format ppf env =
    let open Format in
    let rec f = function
      | Env_empty -> ()
      | Env_value (s, id, _) -> 
          fprintf ppf "val %s@," (Ident.name id);
          f s
      | Env_type (s, id, _) ->
          fprintf ppf "type %s@," (Ident.name id);
          f s
      | Env_exception (s, id, _) ->
          fprintf ppf "exception %s@," (Ident.name id);
          f s
      | Env_module (s, id, _) -> 
          fprintf ppf "module %s@," (Ident.name id);
          f s
      | Env_modtype (s, id, _) -> 
          fprintf ppf "module type %s@," (Ident.name id);
          f s
      | Env_class (s, id, _) ->
          fprintf ppf "class %s@," (Ident.name id);
          f s
      | Env_cltype (s, id, _) ->
          fprintf ppf "class type %s@," (Ident.name id);
          f s
      | Env_open (s, p) ->
          fprintf ppf "open %s@," (Path.name p);
          f s
    in
    fprintf ppf "@[<v>";
    f & summary env;  
    fprintf ppf "@]"

end
