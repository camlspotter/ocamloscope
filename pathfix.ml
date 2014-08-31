open Spotlib.Spot
open List
open Spath

let fix_ident find_packages local_tbl id =
  match assoc_opt id local_tbl with
  | Some p -> 
      begin match Spath.of_path p with
      | SPident _ -> assert false (* All the ident must be prefixed with the top module name *)
      | p -> p
      end
  | None ->
      let { Ident.name = id; stamp } = id in
      let id = Hcons.string id in
      if stamp > 0 && stamp < 1000 then dot predef id
      else if stamp <> 0 then
        (* The ident is local, but not scanned at load, due to something is not yet implemented *) 
        dot (ident "QunknownQ") id
      else
        (* I guess it is global, so module name.
           Search the OCamlFind package name of the module.
        *)
        match find_packages id with
        | None -> 
            (* CR jfuruse: the following is important but too noisy *)
            (* !!% "WARNING: failed to find package for %s@." id;  *)
            dot (pack (OCamlFind.Packages.of_strings [])) id
        | Some pack_ml_path_list -> 
            (* ml_path may be different each other... 
               We must choose something best *)
            let packs, path = 
              fold_left (fun st (pack, path) ->
                match st with
                | None -> Some ([pack], path)
                | Some (_packs, path') when length path > length path' ->
                    Some ([pack], path)
                | Some (packs, path') when path = path' ->
                    Some ((pack ::packs), path)
                | _ ->
                    (* CR jfuruse: We choose the first we see for now *)
                    st) None pack_ml_path_list
              |> from_Some
            in
            let ppacks = pack (OCamlFind.Packages.of_strings (map (fun x -> x.OCamlFind.Package.name) packs)) in
            let rec build st = function
              | [] -> st
              | x::xs -> build (dot st x) xs
            in
            build ppacks path

let fix_path find_packages local_tbl = 
  let open Path in
  let rec path = function
    | Pdot (Pident id, name, _) when Ident.name id = "{*predef*}" -> 
        (* This is for predefined types and exceptions 
           by [Extract.get_predefined] *)
        dot predef name
    | Pdot (t, name, _stamp) -> dot (path t) name
    | Papply (t1, t2) -> apply (path t1) (path t2)
    | Pident id when (Ident.name id).[0] = '{' && Ident.name id <> "{*predef*}" -> 
        (* It is an ident made from a package name *)
        let name = Ident.name id in
        pack (OCamlFind.Packages.of_id & String.(sub name 1 & length name - 2))
    | Pident id -> fix_ident find_packages local_tbl id
  in
  path

(* CR jfuruse: this one and the above add_pack_name are independent each other *)
open Item

let convert_kind pathconv desc =
  let tconv = Stype.of_type_expr pathconv in
  let tconvs = Stype.of_type_exprs pathconv in
  match desc with
  | Constr ty -> Constr (tconv ty)
  | Exception ty -> Exception (tconv ty)
  | Field ty -> Field (tconv ty)
  | Module -> Module
  | ModType -> ModType
  | ClassType -> ClassType
  | Type (params, None, k) -> 
      let params = tconvs params in
      Type (params, None, k)
  | Type (params, Some typ, k) -> 
      begin match tconvs (typ :: params) with
      | typ :: params ->
          Type (params, Some typ, k)
      | _ -> assert false
      end
  | Value ty -> Value (tconv ty)
  | Method (p, v, ty) -> Method (p, v, tconv ty)
  | ClassField (v, ty) -> ClassField (v, tconv ty)
  | Class -> Class
  | Package (p, paths) -> Package (p, paths)
      (* Package is created outside of Pathfix, so no need to touch it *)

(* CR jfuruse: this now fails... why? *)
(*
let test () = 
  let ty = Predef.type_int in
  let xty = Stype.of_type_expr (add_pack_name (fun _ -> assert false) (Hashtbl.create 1) Predef.path_int (* dummy *)) ty in
  let ty' = Stype.to_type_expr Spath.to_path xty in
  let xty' = Stype.of_type_expr Spath.of_path ty' in
  assert (xty == xty')
  
let () = test ()
*)
