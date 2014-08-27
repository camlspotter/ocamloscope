(* Path extension *)

open Path
open Spotlib.Spot

module B = Buffer

let escape_operator s = 
  assert (s <> "");
  if s = "_*_" then "*"
  else match s.[0] with 
  | 'A'..'Z' | 'a'..'z' | '_' | '{' (* We use '{' for package names! *) -> s
  | '*' -> "( " ^ s ^ ")"
  | _ -> "(" ^ s ^ ")"

let name p = 
  let open Buffer in
  let b = create 100 in
  let add_id s = add_string b & escape_operator s in
  let rec name = function
    | Pident id -> add_id & Ident.name id
    | Pdot(p, s, _pos) -> name p; add_char b '.'; add_id s
    | Papply(p1, p2) -> name p1; add_char b '('; name p2; add_char b ')'
  in
  name p;
  Buffer.contents b

let rec remove_package_path = function
  | Pdot (Pident id, n, _x) when (Ident.name id).[0] = '{' ->
      (* we do not mind about stamp. it is just to get the name *)
      Pident (Ident.create n)
  | Pdot (p, n, x) ->
      Pdot (remove_package_path p, n, x)
  | p -> p

