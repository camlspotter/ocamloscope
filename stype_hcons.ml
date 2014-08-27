(* Type algebra, independent from type_expr and core_type.

   It is cumbersome, but we should have specif data types for our needs,
   type_expr and core_type could be very memory hungry.
*)

open Spotlib.Spot
open List

open Stype_core

module Datatypes = Hashcons.Make(struct
  type t = datatype
  let equal dt1 dt2 = dt1.dt_path == dt2.dt_path
  (* CR jfuruse: We ignore the alias information for now *)

  let hash {dt_path=p} = Hashtbl.hash p
  let name = "datatype"
end)

let non_rec_hcons_datatype = Datatypes.non_rec_hcons

module H = Hashcons.Make(struct
  type t = Stype_core.t
  let hash = Hashtbl.hash

  let name = "Stype.t"

  let forall ts1 ts2 f =
    if length ts1 <> length ts2 then false
    else for_all2 f ts1 ts2

  let option topt1 topt2 f =
    match topt1, topt2 with
    | None, None -> true
    | Some t1, Some t2 -> f t1 t2
    | _ -> false

  let equal t1 t2 = match t1, t2 with
    | Link t1', Link t2' when t1' == t2' -> 
        (* CR jfuruse: This never helps the loops *)
        true
    | Link _, _ | _, Link _ -> false
    | Nil, Nil
    | Any, Any -> true
    | VariantClosed i1, VariantClosed i2 -> i1 = i2
    | VarNamed (i,s), VarNamed (i',s') 
    | UnivarNamed (i,s), UnivarNamed (i',s') -> i = i' && s == s' (* string already hconsed *)
    | Var n, Var n'
    | Univar n, Univar n' -> n = n'
    | Arrow (l1, t11, t12), Arrow (l2, t21, t22) ->
        l1 == l2 && t11 == t21 && t12 == t22
    | Tuple ts1, Tuple ts2 -> forall ts1 ts2 (==) 
    | Constr ({dt_path=p1}, ts1), Constr ({dt_path=p2}, ts2) -> 
        p1 == p2 && forall ts1 ts2 (==) 
    | Object (fields1, names1), Object (fields2, names2) ->
        option fields1 fields2 (fun (fields1, attr1) (fields2, attr2) ->
          forall fields1 fields2 (fun (s1,t1) (s2, t2) -> s1 == s2 && t1 == t2)
          && match attr1, attr2 with
             | `Closed, `Closed -> true
             | `Open t1, `Open t2 -> t1 == t2
             | _ -> false)
(*
        && option names1 names2 (fun (_p1, _ts1) (_p2, _ts2) -> false)
          (* CR jfuruse: TODO *)
          (* #a -> #a is never shared in OCaml. Avoid (#a as 'a) -> 'a *)
*)
        && option names1 names2 (fun (p1, ts1) (p2, ts2) ->
          p1 == p2 && forall ts1 ts2 (==))
    | Alias (t1, s1), Alias (t2, s2) -> t1 == t2 && s1 == s2
    | Poly (t1, ts1), Poly (t2, ts2) -> t1 == t2 && forall ts1 ts2 (==) 
    | Package (p1, lts1), Package (p2, lts2) ->
        p1 == p2 && forall lts1 lts2 (fun (l1, t1) (l2, t2) -> l1 == l2 && t1 == t2) 
    | Variant xrow1, Variant xrow -> 
        let tag_comp t1 t2 = match t1, t2 with
          | `Inherit t1, `Inherit t2 -> t1 == t2
          | `Tag (tag1, topt1), `Tag (tag2, topt2) ->
              tag1 == tag2 && option topt1 topt2 (==)
          | _ -> false
        in
        let tag_full_comp t1 t2 = match t1, t2 with
          | `Inherit t1, `Inherit t2 -> t1 == t2
          | `Tag (tag1, b1, ts1), `Tag (tag2, b2, ts2) ->
              tag1 == tag2 && b1 = b2 && forall ts1 ts2 (==)
          | _ -> false
        in
        let fields = match xrow1.xrow_fields, xrow.xrow_fields with
          | `Exact tags1, `Exact tags2 -> 
              forall tags1 tags2 tag_comp
          | `Open tags1, `Open tags2 -> 
              forall tags1 tags2 tag_comp
          | `Closed (tag_fulls1, present1), `Closed (tag_fulls2, present2) ->
              forall present1 present2 (==)
              && forall tag_fulls1 tag_fulls2 tag_full_comp
          | _ -> false
        and more = xrow1.xrow_more == xrow.xrow_more
        and name = option xrow1.xrow_name xrow.xrow_name (fun (p1,ts1) (p2,ts2) ->
          p1 == p2 && forall ts1 ts2 (==))
        in
        fields && more && name

(*
        (res |- fun _ ->
          if not res then begin
            !!% "Variant debug @[@[%a@]@ and@ @[%a@]@]@." (format None) t1 (format None) t2;
            !!% "  %b %b %b %b %b@." fields more closed fixed name;
            iter2 (fun f1 f2 ->
              if not (field_comp f1 f2) then
                !!% "  ??? %s %b@." (fst f1) (fst f1 == fst f2)) xrow1.xrow_fields xrow.xrow_fields;
            assert false
          end
         )   
*)
    | _ -> false
end)

let rec_hcons_datatype { dt_path=p; dt_aliases=r } = 
  Datatypes.non_rec_hcons { dt_path = Spath.rec_hcons p; dt_aliases = r }

(* CR jfuruse: this is incredibly inefficient since it hconsgrep
   all the nodes 
*)
let rec_hcons =

  let nodes = ref [] in

  let rec rec_hcons ty = 
    nodes +::= ty;
    match ty with
    | Link { contents = `Stub } -> assert false
    | Link { contents = `Linked ty' } ->
        if memq ty' !nodes then ty
        else rec_hcons ty' (* no loop ? *)
    | Nil -> ty
    | VariantClosed _ -> H.non_rec_hcons ty
    | Any -> ty
    | VarNamed (n, s) -> 
        let s = Hcons.string s in
        H.non_rec_hcons & VarNamed (n, s)
    | UnivarNamed (n, s) ->
        let s = Hcons.string s in
        H.non_rec_hcons & UnivarNamed (n, s)
    | Var _ | Univar _ -> H.non_rec_hcons & ty
    | Arrow (s, t1, t2) ->
        let s = Hcons.string s in
        let t1 = rec_hcons t1 in
        let t2 = rec_hcons t2 in
        H.non_rec_hcons & Arrow (s, t1, t2)
    | Tuple ts -> 
        let ts = map rec_hcons ts in
        H.non_rec_hcons & Tuple ts
    | Constr (dt, ts) -> 
        let dt = rec_hcons_datatype dt in
        let ts = map rec_hcons ts in
        H.non_rec_hcons & Constr (dt, ts)
    | Object (fields_co_opt, alias_opt) ->
        let fields_co_opt = 
          flip Option.map fields_co_opt & fun (fields, co) ->
            let fields = map (fun (s,t) -> Hcons.string s, rec_hcons t) fields in
            let co = match co with
              | `Closed -> `Closed
              | `Open t -> `Open (rec_hcons t)
            in
            fields,co
        in
        let alias_opt = 
          flip Option.map alias_opt & fun (p, ts) ->
            Spath.rec_hcons p, map rec_hcons ts
        in
        H.non_rec_hcons & Object (fields_co_opt, alias_opt)
    | Alias (t, s) -> H.non_rec_hcons & Alias (rec_hcons t, Hcons.string s)
    | Variant xrow -> H.non_rec_hcons & Variant (rec_hcons_xrow xrow)
    | Poly (t, ts) -> H.non_rec_hcons & Poly (rec_hcons t, map rec_hcons ts)
    | Package (p, pp_t_lst) ->
        H.non_rec_hcons & Package (Spath.rec_hcons p,
                                   map (fun (p, t) ->
                                     Spath.rec_hcons p, rec_hcons t) pp_t_lst)
    | Attr _ -> ty (* It is only for printing and hconsing is not required. *)
  
  and rec_hcons_xrow xrow =
    let hcons_tags ts =
      let inherits = 
        (* CR jfuruse: we do not sort them *)
        filter_map (function 
          | `Inherit t -> Some (`Inherit (rec_hcons t)) 
          | _ -> None) ts
      in
      let tags =
        filter_map (function 
          | `Tag (t, topt) -> Some (`Tag (Hcons.string t, Option.map rec_hcons topt))
          | _ -> None) ts
      in
      inherits @ tags
    in
    let hcons_tag_fulls ts =
      let inherits = 
        (* CR jfuruse: we do not sort them *)
        filter_map (function 
          | `Inherit t -> Some (`Inherit (rec_hcons t)) 
          | _ -> None) ts
      in
      let tags =
        filter_map (function 
          | `Tag (t, b, ts) -> Some (`Tag (Hcons.string t, b, map rec_hcons ts))
          | _ -> None) ts
      in
      inherits @ tags
    in
    let xrow_fields = match xrow.xrow_fields with
      | `Exact tags -> `Exact (hcons_tags tags)
      | `Open tags  -> `Open  (hcons_tags tags)
      | `Closed (tag_fulls, present) ->
          `Closed (hcons_tag_fulls tag_fulls, map Hcons.string (sort compare present))
    in
    let xrow_more = rec_hcons xrow.xrow_more in
    let xrow_name = Option.map (fun (p, ts) -> Spath.rec_hcons p, map rec_hcons ts) xrow.xrow_name in
    { xrow_fields; xrow_more; xrow_name; }
      
  in
  rec_hcons
  
let non_rec_hcons = H.non_rec_hcons

(* CR jfuruse: this slow down rec_hcons x2 *)
let rec_hcons ty =
  let ty' = rec_hcons ty in
  let ty'' = rec_hcons ty' in
  if ty' == ty'' then ty'
  else begin
    !!% "Oops rec_hcons failure %a@." Stype_core.format_via_type_expr ty; ty'
  end
