(**

   The aim of this module is to extract definitions from cmt/cmti files:

   * values
   * constructors
   * record fields
   * types
   * and so on

*)

open Asttypes
open Ident
open Path
open Typedtree

module P = Printtyp

open Spotlib.Spot
open List

type t = { 
  path  : Path.t;
  loc   : Location.t * [ `Direct | `Unknown ];
  kind  : Types.type_expr Item.kind;
  env   : (Ident.t * Path.t) list (** To replace local idents in [kind] by paths *)
}

let pdot path ident = Pdot (path, ident.name, ident.stamp)

(* For module expr functor(A : sig .. end) under P, 
   returns P(A) (the result) and P.A (inside argment) 
*)
let p_functor p id =
  Papply (p, Pident id),
  Pdot (p, id.name, id.stamp)

let tuple ts = Btype.newgenty (Ttuple ts)
let arrow from to_ = Btype.newgenty & Tarrow ("", from, to_, Cok)

(* exception E of t1 * t2 => t1 * t2 -> exn *)
let type_of_extension_constructor_kind = function
  | Text_rebind _ -> assert false
  | Text_decl (_ctys, None) -> assert false (* exn? *)
  | Text_decl (ctys, Some cty) -> arrow (tuple (List.map (fun t -> t.ctyp_type) ctys)) cty.ctyp_type

let type_of_extension_constructor ec =
  let open Types in
  arrow (tuple ec.ext_args) 
    begin match ec.ext_ret_type with
    | None -> 
        Btype.newgenty (Tconstr (ec.ext_type_path, ec.ext_type_params, ref Mnil))
    | Some ty -> ty
    end

(* type 'a t = C of t1 * t2 => C : t1 * t2 -> 'a t *)
let type_of_constr tyid type_params tyargs tyopt =
  let open Btype in
  let open Types in    
  let ret = Option.default tyopt & fun () -> 
    newgenty (Tconstr (Pident tyid, type_params, ref Mnil))
  in
  match tyargs with
  | [] -> ret
  | [x] -> arrow x ret
  | xs -> arrow (tuple xs) ret

(* type 'a t = { l : ty } => l : 'a t -> ty *)
let type_of_field tyid type_params ty =
  let open Btype in
  let open Types in      
  let dom = newgenty & Tconstr (Pident tyid, type_params, ref Mnil) in
  arrow dom ty

let (!!!) = Envaux.env_of_only_summary

let restrict ~by ts =
  let ts = fold_right (fun t st -> 
    let k = Path.name t.path, Item.name_of_kind t.kind in
    if mem_assoc k st then st else (k,t) :: st) ts []
  in
  let by' = fold_right (fun t st -> 
    let k = Path.name t.path, Item.name_of_kind t.kind in
    if mem_assoc k st then st else (k,t) :: st) by []
  in
  try
    flip map by & fun t ->
      let p = t.path in
      let k = Item.name_of_kind t.kind in
      match assoc_opt (Path.name p,k) ts with
      | None -> assert false
      | Some t' ->
          (* Which to choose? *)
          t'
  with
  | e ->
      !!% "!!! restrict error@.";
      !!% " SRC @[%a@]@."
        Format.(list "@," (fun ppf ((x,y),_) -> fprintf ppf "%s : %s" x y))
        ts;
      !!% " BY  @[%a@]@."
        Format.(list "@," (fun ppf ((x,y),_) -> fprintf ppf "%s : %s" x y))
        by';
      raise e
    
let rec structure env path str =
  let ty_env = !!!(str.str_final_env) in
  let _, is = 
    fold_left (fun (env,st) i -> 
      let env, is = structure_item env path ty_env i in
      env, is @ st) (env,[]) str.str_items
  in

  (* In [include A let x = 1], [x] in [A] must be shadowned by [x] *)
  let tbl = Hashtbl.create 1023 in
  fold_left (fun st t ->
    let name = Item.name_of_kind t.kind in 
    let key = name, Xpath.name t.path in
    match Hashtbl.find_opt tbl key with
    | None -> 
        Hashtbl.add tbl key t.loc;
        t::st
    | Some loc -> 
        match t.kind with
        | Value _ | Constr _ | Field _ -> st
        | _ -> 
            !!% "@[<2>BUG: structure %s: %s %s appears twice in a structure!@,@[<v>%a@,%a@]@."
              (Xpath.name path)
              name
              (Xpath.name t.path)
              Location.print (fst loc)
              Location.print (fst t.loc);
            st) [] is

and structure_item env path ty_env sitem =
  match sitem.str_desc with
  | Tstr_eval _ -> env, []
  | Tstr_value (_, bindings) -> 
      (* let ty_env = sitem.str_env in *)
      let ids = let_bound_idents bindings in
      env,
      map (fun id -> 
        let vdesc = Env.find_value (Pident id) ty_env in
        let path = pdot path id in
        { path; 
          loc= vdesc.Types.val_loc, `Direct; 
          kind= Value vdesc.Types.val_type;
          env }
      ) ids
  | Tstr_primitive {val_id=id; val_name= {loc}; val_val= vd} -> 
      let path = pdot path id in
      env, 
      [ { path; loc = loc, `Direct; 
          kind= Value vd.Types.val_type; env } ]
  | Tstr_type decls -> 
      let env = map (fun {typ_id=id} -> id, pdot path id) decls @ env in
      env,
      concat_map (fun ({typ_id=id; typ_name= {Asttypes.loc}} as td) ->
        type_declaration env path loc id td) decls
  | Tstr_exception {ext_id=id; ext_name= {loc}; ext_kind= ed} -> 
      let path = pdot path id  in
      env,
      [ { path; loc= loc, `Direct; 
          kind= Exception (type_of_extension_constructor_kind ed); env } ]
  | Tstr_module {mb_id=id; mb_name= {loc}; mb_expr=mexp} ->  
      let path = pdot path id in
      (id, path) :: env,
      { path; loc= loc, `Direct; kind= Module; env }
      :: module_expr env path mexp
  | Tstr_recmodule xs -> 
      let env = map (fun {mb_id=id} -> id, pdot path id) xs @ env in
      env,
      concat_map (fun {mb_id=id; mb_name= {Asttypes.loc}; mb_expr= mexp } -> 
        let path = pdot path id in (* CR jfuruse: calculated above already *)
        { path; loc= loc, `Direct; kind = Module; env } :: module_expr env path mexp) xs
  | Tstr_modtype {mtd_id=id; mtd_name= {loc}; _ (* TODO *)} -> 
      let path = pdot path id in
      (id,path) :: env,
      [ { path; loc= loc, `Direct; kind= ModType; env } ]
      (* @ in_mty (module_type path mty) *)
  | Tstr_open _ -> env, []
  | Tstr_class xs ->
      let env = 
        concat_map (fun (cl_decl, _, _) ->
          map (fun id -> id, pdot path id) 
            [ cl_decl.ci_id_class;
              cl_decl.ci_id_class_type;
              cl_decl.ci_id_object;
              cl_decl.ci_id_typesharp ]) xs @ env
      in
      env,
      concat_map (fun (cl_decl, _, _) -> class_declaration env path cl_decl) xs
  | Tstr_class_type xs ->
      let env = 
        concat_map (fun (_, _, cl_decl) ->
          map (fun id -> id, pdot path id) 
            [ cl_decl.ci_id_class;
              cl_decl.ci_id_class_type;
              cl_decl.ci_id_object;
              cl_decl.ci_id_typesharp ]) xs @ env
      in
      env,
      concat_map (fun (_, _, x) -> class_type_declaration env path x) xs
  | Tstr_include { incl_mod=mexp; incl_type= sg } ->
      (* CR jfuruse: constrain by sg *)
      env_ty_signature env path sg,
      let ts1 = module_expr env path mexp in
      let ts2 = ty_signature env !!!(sitem.str_env) mexp.mod_loc path sg in
      restrict ~by:ts2 ts1
  | Tstr_typext _ |  Tstr_attribute _ -> assert false (* CR jfuruse: not yet *)

and signature env path sg =
  fold_left (fun (env,st) i ->
    let env, is = signature_item env path i in
    env, is @ st) (env, []) sg.sig_items
  |> snd

and ty_signature env ty_env loc path sg =
  fold_left (fun (env,st) i ->
    let env, is = ty_signature_item env ty_env loc path i in
    env, is@st) (env, []) sg
  |> snd

(* enrich env with top defined idents in sg *)
and env_ty_signature env path sg =
  fold_left (fun env i -> env_ty_signature_item env path i) env sg

(* enrich env with top defined idents in sg *)
and env_ty_signature_item env path sgitem =
  let open Types in
  match sgitem with
  | Sig_value (_id, _vd) -> env
  | Sig_typext (_id, _ty_ed, _rec) -> env
  | Sig_module (id, _mty, _) -> 
      let path = pdot path id in
      (id, path) :: env
  | Sig_modtype (id, _mtyd) ->
      let path = pdot path id in
      (id, path) :: env
  | Sig_type (id, _ty_tydecl, _) -> 
      (id, pdot path id) :: env
  | Sig_class (id, _cd, _) -> 
      (id, pdot path id) :: env
  | Sig_class_type (id, _ctd, _) -> 
      (id, pdot path id) :: env

and signature_item env path sgitem = match sgitem.sig_desc with
  | Tsig_value {val_id=id; val_name={loc}; val_val=vd} -> 
      let path = pdot path id in
      env,
      [ { path; loc= loc, `Direct; kind= Value vd.Types.val_type; env } ]
  | Tsig_type type_decls ->
      let env = map (fun {typ_id=id} -> (id, pdot path id)) type_decls @ env in
      env,
      concat_map (fun ({typ_id=id; typ_name= {Asttypes.loc}} as tdecl) ->
        type_declaration env path loc id tdecl
      ) type_decls 
  | Tsig_exception {ext_id=id; ext_name={loc}; ext_kind} -> 
      let path = pdot path id in 
      env, 
      [ { path; loc= loc, `Direct; kind= Exception (type_of_extension_constructor_kind ext_kind); env } ]
  | Tsig_module {md_id=id; md_name= {loc}; md_type= mty} -> 
      let path = pdot path id in
      (id,path)::env,
      { path; loc= loc, `Direct; kind= Module; env } :: module_type env path mty
  | Tsig_recmodule xs ->
      let env = map (fun {md_id=id} -> id, pdot path id) xs @ env in 
      env,
      concat_map (fun {md_id=id; md_name= {Asttypes.loc}; md_type= mty} ->
        let path = pdot path id in (* CR jfuruse: calculated above already *)
        { path; loc= loc, `Direct; kind= Module; env } :: module_type env path mty) xs
  | Tsig_modtype {mtd_id=id; mtd_name={Asttypes.loc}} -> 
      let path = pdot path id in
      (id, path) :: env,
      [ { path; loc= loc, `Direct; kind= ModType; env } ]
  | Tsig_open _ -> env, []
  | Tsig_include { incl_mod=mty; incl_type= ty_sig } -> 
      env_ty_signature env path ty_sig,
      let ts1 = module_type env path mty in
      let ts2 = ty_signature env !!!(sgitem.sig_env) mty.mty_loc path ty_sig in
      restrict ~by:ts2 ts1
  | Tsig_class clds -> 
      let env = 
        concat_map (fun cl_decl ->
          map (fun id -> id, pdot path id)
            [ cl_decl.ci_id_class;
              cl_decl.ci_id_class_type;
              cl_decl.ci_id_object;
              cl_decl.ci_id_typesharp ]) clds @ env
      in
      env,
      concat_map (class_description env path) clds
  | Tsig_class_type cltys ->
      let env = 
        concat_map (fun cl_decl ->
          map (fun id -> id, pdot path id)
            [ cl_decl.ci_id_class;
              cl_decl.ci_id_class_type;
              cl_decl.ci_id_object;
              cl_decl.ci_id_typesharp ]) cltys @ env
      in
      env,
      concat_map (class_type_declaration env path) cltys
  | (Tsig_typext _|Tsig_attribute _) -> assert false (* CR jfuruse: not yet *)
 
and ty_signature_item env ty_env loc path sgitem = 
  let open Types in
  match sgitem with
  | Sig_value (id, vd) -> 
      let path = pdot path id in (* do not add ! *)
      env,
      [ { path; loc= loc, `Unknown; kind= Value vd.val_type; env } ]
  | Sig_typext (id, ext_const, _rec) -> 
      let path = pdot path id in 
      env,
      [ { path; loc= loc, `Unknown; kind= Exception (type_of_extension_constructor ext_const); env } ]
  | Sig_module (id, {md_type=mty}, _) -> 
      let path = pdot path id in
      (id, path) :: env,
      { path; loc= loc, `Unknown; kind= Module; env }
      :: ty_module_type env ty_env loc path mty
  | Sig_modtype (id, _mtyd) ->
      let path = pdot path id in
      (id, path) :: env,
      [ { path; loc= loc, `Unknown; kind= ModType; env } ]
  | Sig_type (id,ty_tydecl, _) -> 
      let env = (id, pdot path id) :: env in
      env,
      ty_type_declaration env loc path id ty_tydecl
  | Sig_class (id, cd, _) -> 
      let env = (id, pdot path id) :: env in
      env,
      ty_class_declaration env loc path id cd
  | Sig_class_type (id, ctd, _) -> 
      let env = (id, pdot path id) :: env in
      env, 
     ty_class_type_declaration env loc path id ctd

and module_expr env path (mexp : module_expr) =
  (* let env = mexp.mod_env in *)
  match mexp.mod_desc with
  | Tmod_structure str -> structure env path str 
  | Tmod_ident _ -> ty_module_type env !!!(mexp.mod_env) mexp.mod_loc path mexp.mod_type
  | Tmod_functor (id, _loc, _amty, mexp) ->
      (* module P(ID:amty) = mexp  =>  P(ID).x = mexp.x  for each x in mexp *)
      let path_result, _path_inside = p_functor path id in
      (* { pxacks=(); doc=(); path = path_result; loc= loc.loc; kind= Module; alias = None; } :: *)
      (* { pxacks=(); doc=(); path = path_inside; loc = loc.loc; kind = Module; flag = `Exists; alias= None } :: *)
      (* in_mty (module_type path_inside amty) :: *)
      module_expr env path_result mexp
  | Tmod_apply (_mexp1, _mexp2, _module_coercion) ->
      (* module P = Mexp1(Mexp2)   => P.x = Mexp1(Mexp2).x for each x in mexp *)
      ty_module_type env !!!(mexp.mod_env) mexp.mod_loc path mexp.mod_type
      (* @ module_expr (Pdot (path, "_functor_", -1)) mexp1 
         @ module_expr (Pdot (path, "_arg_", -1)) mexp2 *)
  | Tmod_constraint (mexp, _ty_mty, _module_type_constraint, _module_coercion) ->
      module_expr env path mexp
      (* CR jfuruse: this is not correct. We look mexp, then constrain the result by ty_mty *)
  | Tmod_unpack (_e, ty_mty) -> 
      ty_module_type env !!!(mexp.mod_env) mexp.mod_loc path ty_mty

(*
and _module_constraint items items_constraint = 
  (* CR jfuruse: We must use the locations obtained from items *)
  items @ exists items_constraint

and _mod_constraint path mtc =
  match mtc with
  | Tmodtype_implicit -> []
  | Tmodtype_explicit mty ->
      module_type path mty
*)

and module_type env path mty =
  match mty.mty_desc with
  | Tmty_signature sg -> signature env path sg
  | Tmty_functor (id, _loc, _amty, rmty) -> 
      let path_result, path_inside = p_functor path id in
      (* { pxacks=(); doc=(); path= path_result; loc= loc.loc; kind= Module; alias= None } ::
         { pxacks=(); doc=(); path= path_inside; loc= loc.loc; kind= Module; alias= None } :: *)
      (* in_mty (module_type path_inside amty) :: *)
      let env = (id, path_inside) :: env in
      module_type env path_result rmty
  | Tmty_with (_mty', _constraints) -> 
      (* 
         module_type path mty' ... but must be constrained. Otherwise it provides wrong info:

         sig
           type 'a x = ...
           include Monad_intf.T with type 'a t := 'a x      <--- 'a t in Monad_intf.T must be replaced by 'a x
         end

         So far, we give up scraping from mty'. Rather, we scrape mty.mty_type, which should provide correct data
      *)
      ty_module_type env !!!(mty.mty_env) mty.mty_loc path mty.mty_type
  | Tmty_ident (_p, _)      -> 
      ty_module_type env !!!(mty.mty_env) mty.mty_loc path mty.mty_type
  | Tmty_typeof _mexp -> ty_module_type env !!!(mty.mty_env) mty.mty_loc path mty.mty_type

  | Tmty_alias _ -> assert false (* CR jfuruse: not yet *)
      
and ty_module_type env ty_env loc path (mty : Types.module_type) =
  let open Types in
  match Mtype.scrape ty_env mty with
  | Mty_ident p -> 
      !!% "Failed to scrape %s@.  @[%a@]@." 
        (Path.name p)
        Xenv.Summary.format ty_env;
      [] (* failed to scrape *) (* CR jfuruse: we should print out warning *)
  | Mty_signature sg -> ty_signature env ty_env loc path sg
  | Mty_functor (id, _amty, rmty) -> 
      let path_result, path_inside = p_functor path id in
      let env = (id, path_inside) :: env in
      ty_module_type env ty_env loc path_result rmty
  | Mty_alias _ -> assert false

and type_declaration env path loc tyid td =
  let open Types in
  (* env is already enriched *)
  let path' = pdot path tyid in
  { path= path'; loc= loc, `Direct; env;
    kind = Type (td.typ_type.type_params,
                 td.typ_type.type_manifest,
                 match td.typ_kind with 
                 | Ttype_abstract  -> `Abstract
                 | Ttype_variant _ -> `Variant
                 | Ttype_record _  -> `Record
                 | Ttype_open -> assert false (* CR jfuruse: not yet *)) } 
  :: match td.typ_kind with
  | Ttype_open -> assert false (* CR jfuruse: not yet *)
  | Ttype_abstract -> []
  | Ttype_variant vars (* (Ident.t * string loc * core_type list * Location.t) list *) ->
      (* Typedtree.Ttype_variant misses GADT return type,
         so we try to retrienve it from Types.Type_variant. *)
      map2 (fun
        {cd_name= {Asttypes.loc}} {Types.cd_id=id; cd_args=tyargs; cd_res=gadt_tyopt (* cd_loc ??!? CR jfuruse *) } -> 
          let path = pdot path id in
          { path; loc= loc, `Direct; env;
            kind= Constr (type_of_constr tyid td.typ_type.Types.type_params
                            tyargs gadt_tyopt) })
        vars
        (match td.typ_type.Types.type_kind with
        | Types.Type_variant vars -> vars
        | _ -> assert false)
        
  | Ttype_record fields
      (* (Ident.t * string loc * mutable_flag * core_type * Location.t) list *) ->
      map (fun {ld_id=id; ld_name={Asttypes.loc}; ld_type= ty} ->
        let path = pdot path id in
        { path; loc= loc, `Direct; env; 
          kind= Field (type_of_field tyid td.typ_type.Types.type_params ty.ctyp_type) }
      ) fields

and ty_type_declaration env loc path tyid td = 
  let open Types in
  let path' = pdot path tyid in
  { path= path'; loc= loc, `Direct; env; 
    kind= Type (td.type_params,
                td.type_manifest,
                match td.type_kind with
                | Type_open -> assert false
                | Type_abstract -> `Abstract
                | Type_variant _ -> `Variant
                | Type_record _ -> `Record) }
  :: 
  match td.type_kind with
  | Type_open -> assert false (* CR jfuruse: not yet *)
  | Type_abstract -> []
  | Type_variant vars (* (Ident.t * type_expr list * type_expr option) list *) ->
      map (function
        | {Types.cd_id=id; cd_args= tyargs; cd_res= tyopt (* gadt return *)} -> 
            let path = pdot path id in
            { path; loc= loc, `Direct; env;
              kind= Constr (type_of_constr tyid td.type_params tyargs tyopt) } 
      ) vars

  | Type_record (fields, _rec_repr) -> (*  (Ident.t * mutable_flag * type_expr) list * record_representation *)
      map (fun {Types.ld_id=id; ld_type=ty} -> (* CR jfuruse: mutables *)
        let path = pdot path id in
        { path; loc= loc, `Direct; env; 
          kind= Field (type_of_field tyid td.type_params ty) }) fields

and class_declaration env path0 cl_decl =
    (* env is already enriched *)
    let loc = cl_decl.ci_id_name.loc in
    let ids = [ cl_decl.ci_id_class;
                cl_decl.ci_id_class_type;
                cl_decl.ci_id_object;
                cl_decl.ci_id_typesharp ] in
    let path = pdot path0 (hd ids) in
    { path; loc= loc, `Direct; env; kind= Class } :: class_expr env path cl_decl.ci_expr

and ty_class_declaration env loc path0 id cd =
    let open Types in
    let path = pdot path0 id in
    { path; loc= loc, `Unknown; env; kind= Class } :: ty_class_type env loc path cd.cty_type

and class_type_declaration env path0 cl_decl =
    let loc = cl_decl.ci_id_name.loc in
    let ids = [ cl_decl.ci_id_class;
                cl_decl.ci_id_class_type;
                cl_decl.ci_id_object;
                cl_decl.ci_id_typesharp ] in
    let path = pdot path0 (hd ids) in
    [ { path; loc= loc, `Direct; env; kind= ClassType } ]

and ty_class_type_declaration env loc path0 id _ctd =
    let path = pdot path0 id in
    [ { path; loc= loc, `Unknown; env; kind= ClassType } ]

and class_description env path0 cl_decl = (* sig [[[class t : t1 -> object method m : t end]]] end *)
    let loc = cl_decl.ci_id_name.loc in
    let ids = [ cl_decl.ci_id_class;
                cl_decl.ci_id_class_type;
                cl_decl.ci_id_object;
                cl_decl.ci_id_typesharp ] in
    let path = pdot path0 (hd ids) in
    { path; loc= loc, `Direct; env; kind= Class } :: class_type env path cl_decl.ci_expr

and class_type env path clty = (* sig class t : t1 ->[[[object method m : t end]]] end *)
    match clty.cltyp_desc with
    | Tcty_constr (_path, {loc}, _ctys) -> ty_class_type env loc path clty.cltyp_type
    | Tcty_arrow (_, _, clty) ->
        (* CR jfuruse: this should be remembered as class creation
           function ? *)
        class_type env path clty
    | Tcty_signature csig -> class_signature env path csig

and ty_class_type env loc path clty =
    let open Types in
    match clty with
    | Cty_constr (_path, _types, clty) -> ty_class_type env loc path clty (* CR jfuruse: not sure! *)
    | Cty_arrow (_, _, clty) ->
        (* CR jfuruse: this should be remembered as class creation
           function ? *)
        ty_class_type env loc path clty
    | Cty_signature csig -> ty_class_signature env loc path csig

and class_signature env path csig =
    concat_map (class_type_field env path) csig.csig_fields

and ty_class_signature env loc path csig =
  let open Types in
  (* inheritanace is expanded, I guess. *)
  let (all_vars : (string  * mutable_flag * virtual_flag * type_expr) list) = Vars.fold (fun l (m, v, t) all -> (l, m, v, t) :: all) csig.csig_vars [] in
  let ((fields : (string * field_kind * type_expr) list), _) = Ctype.flatten_fields (Ctype.object_fields csig.csig_self) in
  
  flip map all_vars (fun (name, _mutable_flag, virtual_flag, ty) ->
    let id = Ident.create_persistent name in
    let path = pdot path id in
    { path; loc= loc, `Unknown; env; kind= ClassField (virtual_flag, ty) })
  @ flip filter_map fields (function
    | name, _, _ when name = Btype.dummy_method -> None
    | (name, fk, ty) ->
      let id = Ident.create_persistent name in
      let path = pdot path id in
      let vf = match Btype.field_kind_repr fk with 
        | Fabsent -> assert false (* must not occur *)
        | Fvar { contents = Some _ } -> assert false (* must be repred *)
        | Fvar { contents = None } -> Virtual 
        | Fpresent -> Concrete
      in
      Some { path; loc= loc, `Unknown; env; kind= Method (Public (*?*), vf, ty) })
      
and class_type_field env path ctfield =
    let loc = ctfield.ctf_loc in
    match ctfield.ctf_desc with
    | Tctf_attribute _ -> assert false (* CR jfuruse: not yet *)
    | Tctf_inherit clty -> class_type env path clty
    | Tctf_val (name, _mutable_flag, virtual_flag, core_type) ->
        (* CR jfuruse: mutable flag is thrown away *)
        let id = Ident.create_persistent name in
        let path = pdot path id in
        [ { path; loc= loc,`Direct; env; kind= ClassField (virtual_flag, core_type.ctyp_type) } ]
    | Tctf_method (name, _, Virtual, _) when name = Btype.dummy_method -> []
    | Tctf_method (name, private_flag, Virtual, core_type) ->
        let id = Ident.create_persistent name in
        let path = pdot path id in
        [ { path; loc= loc, `Direct; env; kind= Method (private_flag, Virtual, core_type.ctyp_type) } ]
    | Tctf_method (name, _, Concrete, _) when name = Btype.dummy_method -> []
    | Tctf_method (name, private_flag, Concrete, core_type) ->
        let id = Ident.create_persistent name in
        let path = pdot path id in
        [ { path; loc= loc, `Direct; env; kind= Method (private_flag, Concrete, core_type.ctyp_type) } ]
    | Tctf_constraint _ -> [] (* CR jfuruse: constraint? *)

and class_expr env path clexpr = match clexpr.cl_desc with
  | Tcl_ident (_, _, _) -> []
  | Tcl_structure clstr -> class_structure env path clstr
  | Tcl_fun (_, _, _, clexpr, _) -> class_expr env path clexpr
  | Tcl_apply (clexpr, _) -> class_expr env path clexpr
  | Tcl_let (_, _, _, clexpr) -> class_expr env path clexpr
  | Tcl_constraint (clexpr, 
                    _ (* class_type option *) (* CR jfuruse: required? *),
                    _, _, _concr) -> class_expr env path clexpr

and class_structure env path clstr = 
    concat_map (class_field env path) clstr.cstr_fields

and class_field env path clfield =
    match clfield.cf_desc with
    | Tcf_attribute _ -> assert false (* CR jfuruse: not yet *)
    | Tcf_inherit (_, _clexpr, _self, _fields1, _fields2) ->
        (* let loc = clfield.cf_loc in *)
        (* Inherited instance variables and concrete methods *)
        (* CR jfuruse: So it misses virt methods? *)
        (* CR jfuruse: TODO *)
        []

    | Tcf_val ({loc}, _, id, class_field_kind, _override) ->
        let path = pdot path id in
        let ty, virtual_ = match class_field_kind with
          | Tcfk_virtual cty -> cty.ctyp_type, Virtual
          | Tcfk_concrete (_ov, expr) -> expr.exp_type, Concrete
        in
        [ { path; loc= loc, `Direct; env; kind= ClassField (virtual_, ty) } ] 
    | Tcf_method ({txt=name; loc}, private_, class_field_kind) ->
        let path = pdot path (Ident.create name) (* CR jfuruse: ? *) in
        let ty, virtual_ = match class_field_kind with
          | Tcfk_virtual cty -> cty.ctyp_type, Virtual
          | Tcfk_concrete (_ov, expr) -> expr.exp_type, Concrete
        in
        (* ty contains class in the argument *)
        [ { path; loc= loc, `Direct; env; kind= Method (private_, virtual_, ty) } ] 
    | Tcf_constraint _ -> []
    | Tcf_initializer _ -> []

let reset_envs () = 
  Env.reset_cache ();
  Envaux.reset_cache ()

let structure = structure []
let signature = signature []

let ident_predef = Ident.create_persistent "{*predef*}"
let path_predef = Pident ident_predef

let get_predefined () =
  let open Env in
  let rec f st = function
    | Env_functor_arg _ -> assert false (* CR jfuruse: not yet *)
    | Env_type (sum, id, td) -> f (`Type (id,td)::st) sum
    | Env_empty -> st
    | Env_extension (sum, id, ec) -> f (`Exception (id,ec)::st) sum (* CR jfuruse: extension *)
    | Env_value     (sum, _, _)
    | Env_module    (sum, _, _)
    | Env_modtype   (sum, _, _)
    | Env_class     (sum, _, _)
    | Env_cltype    (sum, _, _)
    | Env_open      (sum, _) -> f st sum
  in
  f [] (Env.summary Env.initial_unsafe_string)
  |>
      concat_map (function
        | `Type (id, td) ->
            ty_type_declaration [] Location.none path_predef id td
        | `Exception (id, ec) ->
            let path = pdot path_predef id in
            [ { path; loc= Location.none, `Direct; kind= Exception (type_of_extension_constructor ec); env=[] } ])
