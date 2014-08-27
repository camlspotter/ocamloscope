open Spotlib.Spot
open List

let predefined_types =
  let open Env in
  let rec f st = function
    | Env_type (sum, id, _) -> f (id::st) sum
    | Env_empty -> st
    | Env_value     (sum, _, _)
    | Env_exception (sum, _, _)
    | Env_module    (sum, _, _)
    | Env_modtype   (sum, _, _)
    | Env_class     (sum, _, _)
    | Env_cltype    (sum, _, _)
    | Env_open      (sum, _) -> f st sum
  in
  map Ident.name & f [] &  Env.summary Env.initial
