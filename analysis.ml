open Spotlib.Spot
open Asttypes

type ident = Ident.t
type path = Path.t
type type_expr = Types.type_expr

type a_structure = a_structure_item list

and a_structure_item =
  | A_str_value      of ident * Location.t * type_expr * path option
  | A_str_primitive  of ident * Location.t * type_expr
  | A_str_type       of ident * Location.t * a_type_declaration
  | A_str_exception  of ident * Location.t * type_expr list
  | A_str_exn_rebind of ident * Location.t * path * type_expr list
  | A_str_module     of ident * Location.t * a_module(*_expr *)
  | A_str_modtype    of ident * Location.t (* * a_module(*_type *) *)
  | A_str_class      of ident * Location.t * a_class(*_declaration *)
  | A_str_class_type of ident * Location.t * a_class(*_type_declaration *)
  | A_str_include    of a_module * Location.t * a_signature

and a_type_declaration =
  { a_typ_params: string option list;
    a_typ_kind: a_type_kind;
    a_typ_private: private_flag;
    a_typ_manifest: type_expr option; }

and a_type_kind = 
  | A_type_abstract
  | A_type_variant of (ident * Location.t * type_expr list) list
  | A_type_record of  (ident * Location.t * mutable_flag * type_expr) list

and a_module(*_expr *) = 
  | A_mod_ident of path
  | A_mod_structure of a_structure
  | A_mod_functor of ident (* * Location.t * a_module(*_type*)*) * a_module(*_expr*)
  | A_mod_apply of a_module(*_expr*) * a_module(*_expr*)
  (* | A_mod_constraint of a_module(*_expr*) * a_module(*_type*) *)
  | A_mod_unpack

and a_class = {
  a_cl_virt : virtual_flag;
  a_cl_params : string list;
  a_cl_new : type_expr; (* class c x = object end  has 'a -> object end *)
  a_cl_inherits : type_expr list;
  a_cl_methods : (ident * Location.t * private_flag * virtual_flag * type_expr) list
}

and a_signature = a_signature_item list

and a_signature_item =
  | A_sig_value of Ident.t
  | A_sig_type of Ident.t
  | A_sig_exception of Ident.t
  | A_sig_module of Ident.t
  | A_sig_modtype of Ident.t
  | A_sig_class of Ident.t
  | A_sig_class_type of Ident.t

open Typedtree
open List

let rec structure s = 
  s.str_items |> concat_map @@ fun x -> match x.str_desc with
    | Tstr_eval _ -> []
    | Tstr_value (_, list) ->
        flip concat_map list (fun {vb_pat=p; vb_expr=e} ->
          let rec extract (p, eo) = 
            let alias eo = match eo with
              | Some { exp_desc = Texp_ident (p, _, _) } -> Some p
              | _ -> None 
            in
            match p.pat_desc with
            | Tpat_tuple ps ->
                begin match eo with
                | Some { exp_desc= Texp_tuple es } -> 
                    concat_map extract (combine ps (map (fun x -> Some x) es))
                | _ -> 
                    concat_map extract (map (fun p -> p, None) ps)
                end
            | Tpat_var (id, {loc}) -> 
                [ A_str_value (id, loc, p.pat_type, alias eo) ]
            | Tpat_alias (p, id, {loc}) ->
                A_str_value (id, loc, p.pat_type, alias eo)
                :: extract (p, eo)
  
            | Tpat_construct (_, _, ps)
            | Tpat_array ps ->
                concat_map extract (map (fun p -> (p, None)) ps)
            | Tpat_variant (_, Some p, _) ->
                extract (p, None)
            | Tpat_record (xs, _) -> 
                concat_map extract (map (fun (_, _, p) -> (p, None)) xs)
            | Tpat_or (p1, p2, _) ->
                extract (p1, None) @ extract (p2, None)
            | Tpat_lazy p -> extract (p, None)
                  
            | Tpat_any
            | Tpat_constant _
            | Tpat_variant (_, None, _) -> []
          in
          extract (p, Some e))
    | Tstr_primitive { val_id= id; val_name= {loc}; val_val } -> 
        [ A_str_primitive (id, loc, val_val.val_type) ]
    | Tstr_type xs ->
        xs |> map @@ fun { typ_id=id; typ_name= {loc}; typ_type = td } ->
          A_str_type (id, loc, 
                      { a_typ_params = List.map (function
                        | { Types.desc = Tvar (Some x) } -> Some x
                        | { Types.desc = Tvar None } -> assert false
                        | _ -> assert false) td.type_params;
                        a_typ_kind = assert false;
                        a_typ_private = td.type_private;
                        a_typ_manifest = td.type_manifest })

    | Tstr_exception { ext_id=id; ext_name={loc}; ext_kind= Text_decl (exn_params, None) } ->
        [ A_str_exception (id, loc, map (fun x -> x.ctyp_type) exn_params) ]
    | Tstr_exception { ext_id=_id; ext_name= {loc=_loc}; ext_kind= Text_rebind (_p, _longid) } ->
(*
        (* CR jfuruse: Redundant calculation. Tstr_exn_rebind should carry the arguments *)
        let (_path, arg) = Typedecl.transl_exn_rebind x.str_env loc longid.txt in
        [ A_str_exn_rebind (id, loc, p, arg.exn_args) ]
*)
        assert false
    | Tstr_module { mb_id= id; mb_name= {loc}; mb_expr= mexp } -> 
        [ A_str_module (id, loc, module_expr mexp) ]
    | Tstr_recmodule xs ->
        xs |> map @@ fun (id, {loc}, _mty, mexp) -> 
          A_str_module (id, loc, let _m = module_expr mexp in assert false)
    | Tstr_modtype {mtd_id=id; mtd_name= {loc}; _ } -> 
        [ A_str_modtype (id, loc) ]
    | Tstr_open _ -> []
    | Tstr_class xs ->
        xs |> map @@ fun (cd, _, _) -> class_declaration cd
    | Tstr_class_type xs ->
        xs |> map @@ fun (id, {loc}, ctd) ->
          A_str_class_type (id, loc, class_type_declaration ctd)
    | Tstr_include { incl_mod=mexp; incl_type= sg } ->
        [ A_str_include (module_expr mexp, x.str_loc, ty_signature sg) ]

and module_expr mexp = 
  match mexp.mod_desc with
  | Tmod_ident (p, _) -> A_mod_ident p 
  | Tmod_structure str -> A_mod_structure (structure str)
  | Tmod_functor (id, _, _mty, mexp) -> A_mod_functor (id, module_expr mexp)
  | Tmod_apply (mexp1, mexp2, _) -> A_mod_apply (module_expr mexp1, module_expr mexp2)
  | Tmod_constraint (mexp, _mty, _, _) -> module_expr mexp
  | Tmod_unpack _ -> A_mod_unpack

(*
and module_type mty = 
  match mty.mty_desc with
  | Tmty_ident (p, _) -> A_mod_ident p
  | Tmty_signature sg -> signature sg
  | Tmty_functor (id, {loc}, _mty, mty) ->
      A_mod_functor (id, module_type mty)
  | Tmty_with of module_type * (Path.t * Longident.t loc * with_constraint) list
  | Tmty_typeof of module_expr
*)

and class_declaration cd = 
  let id = cd.ci_id_class in
  let loc = cd.ci_id_name.loc in
  let newty = 
    let cd = cd.ci_decl in
    let last = Btype.newgenty (Tconstr (cd.cty_path, cd.cty_params, ref Types.Mnil)) in
    let rec new_ty last = function
      | Types.Cty_constr _ -> last (* CR jfuruse: Really? TODO *)
      | Cty_signature _ -> last
      | Cty_fun (label, ty, clty) -> 
          Btype.newgenty (Tarrow (label, ty, new_ty last clty, Cok))
    in 
    new_ty last cd.cty_type
  in
  let a_class = { a_cl_virt = cd.ci_virt;
                  a_cl_params = 
                     fst cd.ci_params |> List.map (fun {txt} -> txt);
                  a_cl_new = newty;
                  a_cl_inherits = []; (* CR jfuruse: TODO *)
                  a_cl_methods = [] (* CR jfuruse: TODO *)
                }
  in
  A_str_class ( id, loc, a_class )
                
and class_type_declaration _ctd = assert false

and ty_signature sg = map ty_signature_item sg

and ty_signature_item = function
  | Sig_value (id, _)         -> A_sig_value id
  | Sig_type (id, _, _)       -> A_sig_type id
  | Sig_exception (id, _)     -> A_sig_exception id
  | Sig_module (id, _, _)     -> A_sig_module id
  | Sig_modtype (id, _)       -> A_sig_modtype id
  | Sig_class (id, _, _)      -> A_sig_class id
  | Sig_class_type (id, _, _) -> A_sig_class_type id

and type_kind = function
  | Ttype_abstract -> A_type_abstract
  | Ttype_variant xs ->
      A_type_variant (map (fun (id, {loc}, ts, _) -> (id, loc, map (fun x -> x.ctyp_type) ts)) xs)
  | Ttype_record xs ->
      A_type_record (map (fun (id, {loc}, mf, ct, _) -> (id, loc, mf, ct.ctyp_type)) xs)
