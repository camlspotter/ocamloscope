open Spotlib.Spot
open XSpotlib.Base
open List

open Stype_core
open Stype_hcons

let is_recursive ty = 
  let rec f = function
    | Link {contents = `Stub} -> assert false (* not well formed *)
    | Link {contents = `Linked _} -> raise Exit
    | Nil 
    | VariantClosed _
    | Any 
    | VarNamed _
    | UnivarNamed _
    | Var _
    | Univar _ -> ()
    | Arrow (_, t1, t2) -> f t1; f t2
    | Tuple ts 
    | Constr (_, ts) -> iter f ts
    | Object (fields, names) ->
        flip Option.iter fields (fun (fields, oc) ->
          iter (f ** snd) fields;
          match oc with
          | `Open t -> f t
          | `Closed -> ());
        flip Option.iter names (fun pts ->
          iter f & snd pts)
    | Alias (t, _) -> f t
    | Poly (t, ts) -> iter f (t :: ts)
    | Package (_, pts) -> iter (f ** snd) pts
    | Variant xrow ->
        Option.iter (iter f ** snd) xrow.xrow_name;
        begin match xrow.xrow_fields with
        | (`Exact tags | `Open tags) -> 
            flip iter tags (function
              | `Inherit t -> f t
              | `Tag (_, None) -> ()
              | `Tag (_, Some t) -> f t)
        | `Closed (tags, _presents) ->
            flip iter tags (function
              | `Inherit t -> f t
              | `Tag (_, _, ts) -> iter f ts)
        end;
        f xrow.xrow_more;
    | Attr (_, t) -> f t
  in
  try f ty; false with Exit -> true

open Asttypes
open Parsetree

let of_core_type cty =
  let closed_cntr = ref 0 in
  let var_cntr = ref 0 in
  let vars = Hashtbl.create 107 in
  let rec of_core_type cty = match cty.ptyp_desc with
    | Ptyp_any -> Any
    | Ptyp_var s ->
        (* CR jfuruse: Type expr has only one global scope currently. Not sure it is ok *)
        Hashtbl.find_or_add (fun s ->
          VarNamed (Hashtbl.hash s (* CR jfuruse: Baad tweeek *), s))
          vars s
    | Ptyp_arrow (l, cty1, cty2) ->
        let ty1 = of_core_type cty1 in
        let ty2 = of_core_type cty2 in
        Arrow (l, ty1, ty2)
    | Ptyp_tuple ctys -> 
        Tuple (map of_core_type ctys)
    | Ptyp_constr ({txt= lid}, ctys) -> 
        Constr ( {dt_path = Spath.of_longident lid ;dt_aliases= ref None}, map of_core_type ctys)
    | Ptyp_object (fields, closed) ->
        Object (
          Some (
            flip map fields (fun (s, _, cty) -> (s, of_core_type cty)),
            (match closed with 
            | Closed -> `Closed 
            | Open -> `Open (Var !++var_cntr))),
          None
        )
    | Ptyp_class ({txt=lid}, ctys) -> 
        Object (None, Some (Spath.of_longident lid, map of_core_type ctys))
    | Ptyp_alias (cty, s) -> Alias (of_core_type cty, s) 
    | Ptyp_variant (row_fields, closed_flag, labels_opt) -> 
        (* Parsetree.mli says: *)
        (* [ `A|`B ]         (flag = Closed; labels = None)
           [> `A|`B ]        (flag = Open;   labels = None)
           [< `A|`B ]        (flag = Closed; labels = Some [])
           [< `A|`B > `X `Y ](flag = Closed; labels = Some ["X";"Y"])

           impos             (flag = Open;   labels = Some _) 
         *)
        let more =
          match closed_flag, labels_opt with
          | Open, Some _ -> assert false
          | Closed, None -> VariantClosed !++closed_cntr
          | _ -> Var !++var_cntr
        in

        let inherits = filter_map (function
          | Rtag _ -> None
          | Rinherit cty -> Some (`Inherit (of_core_type cty))) row_fields
        in

        let sorted_rtags = 
          filter_map (function
            | Rtag (l, _attrs, b, ctys) -> Some (l, b, ctys)
            | _ -> None) row_fields
          |> sort (fun (l1, _, _) (l2, _, _) -> compare l1 l2)
        in

        let make_tag = function
          | (l, true, []) -> `Tag (l, None)
          | (l, false, [t]) -> `Tag (l, Some (of_core_type t))
          | _ -> assert false
        in
        
        let make_tag_full = function
          | (l, b, ts) -> `Tag (l, b, map of_core_type ts)
        in

        let xrow_fields = 
          match closed_flag, labels_opt with
          | Open, Some _ -> assert false
          | Open, None ->
              `Open (inherits @ map make_tag sorted_rtags)
          | Closed, None ->
              `Exact (inherits @ map make_tag sorted_rtags)
          | Closed, Some present ->
              `Closed (inherits @ map make_tag_full sorted_rtags, sort compare present)
        in

        Variant
        { xrow_fields;
          xrow_more = more;
          xrow_name = None
        }
  
    | Ptyp_poly ([], cty) ->  (* object method type like x : int is actually marked as a poly in core_type *)
        of_core_type cty
    | Ptyp_poly (vars, cty) -> 
        Poly (of_core_type cty,
              map (fun x -> of_core_type { ptyp_desc= Ptyp_var x;
                                           ptyp_loc = Location.none;
                                           ptyp_attributes = [];
                                         }) vars) 
    | Ptyp_package ptype ->
        let {txt=lid}, fields = ptype in
        Package (Spath.of_longident lid,
                 map (fun ({txt=lid}, cty) -> 
                   Spath.of_longident lid, of_core_type cty) fields)
    | Ptyp_extension _ -> assert false (* CR jfuruse: todo *)
  in
  of_core_type cty

(* CR jfuruse: unused *)
let _format_core_type_via_type_expr ppf cty = Stype_core.format_via_type_expr ppf & of_core_type cty

open Types
open Btype
open Ctype

let to_type_expr = Stype_core.to_type_expr

let of_type_expr pathconv ty =
  normalize_type Env.empty ty; (* Required to normalize variant Reither duped fields *)

  (* In one type_expr, id's must be unique *)
  let visited = Hashtbl.create 1023 in
  let var_cntr = ref 0 in
  let closed_cntr = ref 0 in

  let rec f ty = 
    let ty = repr ty in
    try 
      let ty = Hashtbl.find visited ty.id in

      let rec repr = function
        | Link { contents = `Linked ty } -> repr ty (* It is shared. No loop *)
        | (Link { contents = `Stub } as lty) -> lty (* It is a loop! `Stub will be replaced by a `Link later *)
        | ty -> ty
      in
      repr ty
    with Not_found ->

    let link = ref `Stub in
    Hashtbl.replace visited ty.id (Link link);
    let res = match ty.desc with
      | Tlink _ -> assert false
      | Tsubst _ -> assert false
      | Tvar None -> Var !++var_cntr
      | Tvar (Some s) -> VarNamed (!++var_cntr, Hcons.string s)
      | Tunivar None -> Univar !++var_cntr
      | Tunivar (Some s) -> UnivarNamed (!++var_cntr, Hcons.string s)
      | Tarrow (l, ty1, ty2, _) when Btype.is_optional l -> 
          begin match (repr ty1).desc with
          | Tconstr (p, [ty], _) when p = Predef.path_option ->
              (* CR jfuruse: option stamp number is a fake. *)
              Arrow (Hcons.string l, 
                     non_rec_hcons 
                     & Constr (non_rec_hcons_datatype {dt_path = Spath.(dot predef "option");
                                                        dt_aliases = ref None },
                               [f ty]), 
                     f ty2)
          | _ -> assert false
          end
      | Tarrow (l, _ty1, _ty2, _) when Btype.is_optional l -> assert false
      | Tarrow (l, ty1, ty2, _) -> Arrow (Hcons.string l, f ty1, f ty2)
      | Ttuple tys -> Tuple (map f tys)
      | Tconstr (path, tys, _) -> Constr (non_rec_hcons_datatype {dt_path = pathconv path; dt_aliases= ref None}, map f tys)
      | Tobject (fi, nm) -> (* of type_expr * (Path.t * type_expr list) option ref *)
          let fields, rest =
            let fields, rest = flatten_fields fi in
            let present_fields =
              fold_right (fun (n, k, t) l ->
                match field_kind_repr k with
                | Fpresent -> (Hcons.string n, f t) :: l
                | Fvar {contents = Some _} -> assert false (* this is removed by field_kind_repr *)
                | Fvar { contents = None } | Fabsent -> l)
                fields [] in
            sort (fun (n, _) (n', _) -> compare n n') present_fields,
            rest
          in
          (* let opened = if opened_object ty then `Open else `Closed in *)
          let opened = match (repr rest).desc with
            | Tvar _ | Tunivar _ | Tconstr _ -> `Open (f rest)
            | Tnil -> `Closed
            | _ -> assert false
          in
          let named = match !nm with
            | None -> None
            | Some (p, _ty :: tys) -> Some (pathconv p, map f tys)
            | Some (_p, []) -> assert false
          in
          Object (Some (fields, opened), named)
      | Tfield _ -> assert false
      | Tnil -> Nil
      | Tpoly (ty, tyl) -> Poly (f ty, map f tyl)
      | Tpackage (path, lids, typs) ->
          Package (pathconv path,
                   map2 (fun lid typ ->
                     Spath.of_longident lid,
                     f typ) lids typs)

      | Tvariant row ->
          let row = Btype.row_repr row in
          let xrow_more = 
            let ty = repr & Btype.row_more row in
            match ty.desc with
            | Tnil -> non_rec_hcons (VariantClosed !++closed_cntr)
            | _ -> 
                (* CR jfuruse: OCaml 4.01.0 seems to drop the named type variable of this part: 
                   [ `WithoutSuffix ] as 'tipo  
                   => [ `WithoutSuffix ] as 'a
                *)
                f ty 
          in
          let xrow_name = Option.map (fun (p,ts) -> pathconv p, map f ts) row.row_name in
          let present = 
            if row.row_closed then
              Some (filter_map (function
                | (l, Rpresent _) -> Some l
                | (_, (Reither _ | Rabsent)) -> None) row.row_fields)
            else None
          in
          let all_present = 
            Option.map (fun p -> length row.row_fields = length p) present
          in
          let xrow_fields =
            match row.row_closed, present, all_present with
            | false, None,   Some _ -> assert false
            | false, Some _, _ -> assert false
            | true, None, _ -> assert false
            | true, Some _, None -> assert false
            | false, None, None ->
                `Open (map (function
                  | l, Rpresent topt -> `Tag (Hcons.string l, Option.map f topt)
                  | _ -> assert false) row.row_fields)
            | true,  Some _, Some true -> 
                `Exact (map (function
                  | l, Rpresent topt -> `Tag (Hcons.string l, Option.map f topt)
                  | _ -> assert false) row.row_fields)
            | true, Some _, Some false ->
                `Closed (
                  map (function
                    | l, Rpresent None -> `Tag (Hcons.string l, true, [])
                    | l, Rpresent (Some t) -> `Tag (Hcons.string l, false, [f t])
                    | l, Reither (b, ts, _, _) ->
                        `Tag (Hcons.string l, b, map f ts)
                    | _ -> assert false) row.row_fields,
                  match present with
                  | None -> assert false
                  | Some ps -> map Hcons.string & sort compare ps
                )
          in
          Variant
          { xrow_fields;
            xrow_more;
            xrow_name }
    in
    let res = non_rec_hcons res in
    assert (match res with Link _ -> false | _ -> true);
    link := `Linked res;
    res
  in
  f ty

let to_string = Format.to_string Stype_core.format_via_type_expr
      
let of_type_expr pathconv ty =
  let xty  = of_type_expr pathconv ty in
  let ty'  = to_type_expr Spath.to_path xty in
  let xty' = of_type_expr Spath.of_path ty' in

  let sty  = to_string xty in
  let sty' = to_string xty' in

  if xty != xty' && not (is_recursive xty) then begin
    !!% "ERROR hcons @[@[%a@]@ => @[%a@];@ @[%a@]@]@." 
      Type_expr.format ty
      Stype_core.format_via_type_expr xty
      Stype_core.oformat xty;
    !!% "        and @[@[%a@]@ => @[%a@];@ @[%a@]@]@." 
      Type_expr.format ty'
      Stype_core.format_via_type_expr xty'
      Stype_core.oformat xty';
    assert false
  end else begin
(*
    !!% "OK hcons @[@[%a@]@ => @[%a@]@]@." format_type_expr ty Org.format xty; 
*)
    ()
  end;
  (* CR jfuruse: this is not enough. We somehow compare the printed result
     of ty and ty' *)
  if sty <> sty' then !!% "WARN@.%s@.&@.%s@." sty sty';
  xty
  
let of_type_exprs pathconv tys =
  match of_type_expr pathconv (newgenty(Ttuple tys)) with
  | Tuple tys -> tys
  | _ -> assert false
