open Spotlib.Spot
open List

include Stype_core
include Stype_print
include Stype_conv
include Stype_hcons

(* CR jfuruse: Do we need to build new types for each subst?
   We can delay the subst until we see Vars right? *)
(* subst only happens at search so we do not need hcons *)
let subst params types target =
  let bindings = combine params types in
  let visited = ref [] in
  let rec subst t = 
    try 
      match !(assq t !visited) with
      | `Linked t -> t
      | _ -> assert false
    with Not_found ->
      let res = ref (`Linked Nil) in (* dummy *)
      visited +::= (t, res);
      let t = match t with
        | Nil | VariantClosed _ | Any | UnivarNamed _ | Univar _ -> t
        | Link {contents = `Linked t} ->
            begin 
              try Link (assq t !visited) with Not_found ->
                (* It is possible 
                   type 'a frag = [ `A of 'a ]
                   type t = 'a frag as 'a
                *)
  	        (*
                  !!% "subst: Link but not looped?!?!@.";
                  !!% "  target=@[%a@]@." Stype_print.format target;
                  !!% "  target=@[%a@]@." Stype_print.oformat target;
                  !!% "  types=[@[%a@]]@." (Format.list ";@ " Stype_print.format) types;
                  !!% "  types=[@[%a@]]@." (Format.list ";@ " Stype_print.oformat) types;
                  !!% "  t=@[%a@]@." Stype_print.format t;
                  !!% "  t=@[%a@]@." Stype_print.oformat t;
                *)
                subst t
            end; 
        | Link {contents = `Stub} -> assert false
        | (Var _ | VarNamed _ as var) ->
            begin try assq var bindings
            with Not_found -> var
            end
        | Arrow (lab, t1, t2) ->
            let t1 = subst t1 in
            let t2 = subst t2 in
            Arrow (lab, t1, t2)
        | Tuple ts -> Tuple (map subst ts)
        | Constr (dt, ts) -> Constr (dt, map subst ts)
        | Object (meths, inherits) ->
            let meths = flip Option.map meths & fun (meths, close_open) ->
              map (fun (s,t) -> s, subst t) meths,
              match close_open with
              | `Closed -> `Closed
              | `Open t -> `Open (subst t)
            in
            let inherits = flip Option.map inherits & fun (p, tys) ->
              p, map subst tys
            in
            Object (meths, inherits)
        | Alias _ -> assert false
        | Variant xrow ->
            Variant {
              xrow_fields = begin
                let subst_tag = function
                  | `Tag (t, topt) -> `Tag (t, Option.map subst topt)
                  | `Inherit t -> `Inherit (subst t)
                in
                let subst_tag_full = function
                  | `Tag (t, b, ts) -> `Tag (t, b, map subst ts)
                  | `Inherit t -> `Inherit (subst t)
                in
                match xrow.xrow_fields with
                | `Exact tags -> `Exact (map subst_tag tags)
                | `Open tags -> `Open (map subst_tag tags)
                | `Closed (tag_fulls, present) ->
                    `Closed (map subst_tag_full tag_fulls, present)
              end;
              xrow_more = subst xrow.xrow_more; (* I hope it is correct *)
              xrow_name = flip Option.map xrow.xrow_name (fun (p, ts) ->
                p, map subst ts)
            }
        | Poly (t, vars) ->
            iter (fun var ->
              match var with
              | UnivarNamed _ | Univar _ -> ()
              | _ -> assert false) vars;
            Poly (subst t, vars)
        | Package (p, constraints) ->
            Package (p, map (fun (lid,t) -> lid, subst t) constraints)
        | Attr _ -> assert false (* this is only for printing *)
      in
      res := `Linked t;
      t
  in
  subst target

let get_arrows ty = 
  let rec get_arrows acc ty = match ty with
    | Arrow (l, t, t') -> get_arrows ((l,t)::acc) t'
    | _ -> rev acc, ty
  in
  get_arrows [] ty
    
let rec size_type = function
  | Link _ -> 1 (* loop *)
  | Nil -> 0
  | VariantClosed _ -> 0
  | Attr (_, t) -> size_type t
  | Any -> 0
  | (Var _ | VarNamed _ | Univar _ | UnivarNamed _) -> 0
  | Arrow (_, t1, t2) ->
      size_type t1
      + size_type t2
      + 1
  | Tuple tys -> sum & map size_type tys
  | Constr (_, tys) -> 1 + sum (map size_type tys)
  | Object (Some (cfts, _), _) -> sum & map size_field_type cfts
  | Object (None, Some (_, ts)) -> 1 + sum (map size_type ts)
  | Object (None, None) -> assert false
  | Alias (cty, _) -> size_type cty
  | Variant xrow -> size_row_fields xrow.xrow_fields
  | Poly (cty, vars) -> length vars + size_type cty
  | Package (_, constraints) -> size_package_type_constraints constraints
    
and size_field_type (_,t) = size_type t
    
and size_row_fields fs = 1 + match fs with
  | `Exact tags | `Open tags -> sum (map size_tag tags)
  | `Closed (tag_fulls, present) -> length present + sum (map size_tag_full tag_fulls)

and size_tag = function
  | `Tag (_, None) -> 1
  | `Tag (_, Some t) -> 1 + size_type t
  | `Inherit t -> 1 + size_type t

and size_tag_full = function
  | `Tag (_, _, ts) -> sum (map size_type ts)
  | `Inherit t -> 1 + size_type t

and size_package_type_constraints lctys =
  1 + sum (map (size_type ** snd) lctys)

(** Used for Grouping. looks do not care about hcons *)
let short_look t = 
  let nodes = ref [] in
  let stubs = ref [] in 
  let rec f ex t = 
    let r = ref None in
    nodes +::= (t, r);
    f_ ex t |- fun t' -> r := Some t'
  and f_ ex t =
    match t with
    | Link { contents = `Stub } -> assert false
    | Link { contents = `Linked t' } -> 
        if List.mem_assq t' !nodes then begin
          let stub = ref `Stub in
          stubs +::= (t', stub);
          Link stub
        end else f ex t'
    | Arrow (l, ty1, ty2) when Btype.is_optional l ->
        begin match ty1 with
        | Constr ( { dt_path = Spath.SPdot(Spath.SPpredef, "option") } as dt, [ty1]) -> Arrow(l, Constr (dt, [f ex ty1]), f ex ty2)
        | Attr _ -> Arrow (l, ty1, f ex ty2)
        | _ -> assert false
        end
    | Arrow (l, t1, t2) -> Arrow (l, f ex t1, f ex t2)
    | Tuple ts -> Tuple (map (f ex) ts)
    | Constr ( ({ dt_path; dt_aliases = { contents = Some (Some (ats, t)) } } as dt), ts) when not (memq dt ex) && not (Spath.is_attred dt_path) -> 
        f (dt :: ex) & subst ats ts t
    | Constr (dt, ts) -> 
        let dt' = { dt with dt_path = Spath.short_look dt.dt_path } in
        Constr (dt', map (f ex) ts)
    | Object (fieldsopt, namedopt) ->
        let fieldsopt = fieldsopt |> Option.map (fun (lts, row) ->
          map (fun (l,t) -> l, f ex t) lts,
          (match row with
           | `Closed -> `Closed
           | `Open t -> `Open (f ex t)))
        in
        let namedopt = namedopt |> Option.map (fun (p, ts) ->
          Spath.short_look p, map (f ex) ts)
        in
        Object (fieldsopt, namedopt)
    | Alias (t, s) -> Alias (f ex t, s)
    | Variant x -> Variant (xrow ex x)
    | Poly (t, ts) -> Poly (f ex t, map (f ex) ts)
    | Package (p, pts) -> Package (Spath.short_look p, 
                                   map (fun (p, t) -> Spath.short_look p, f ex t) pts)
    | Attr (_, t) -> f ex t
    | (Nil
      | VariantClosed _
      | Any
      | VarNamed _
      | UnivarNamed _
      | Var _
      | Univar _) -> t

  and xrow ex x =
    { xrow_fields = begin 
        let short_tags = map (function
          | `Tag (l,topt) -> `Tag (l, Option.map (f ex) topt)
          | `Inherit t -> `Inherit (f ex t))
        in
        let short_tag_fulls = map (function
          | `Tag (l,b,ts) -> `Tag (l, b, map (f ex) ts)
          | `Inherit t -> `Inherit (f ex t))
        in
        match x.xrow_fields with
        | `Exact tags -> `Exact (short_tags tags)
        | `Open  tags -> `Open  (short_tags tags)
        | `Closed (tags, present) -> `Closed (short_tag_fulls tags, present)
      end;
      xrow_more = f ex x.xrow_more;
      xrow_name = x.xrow_name |> Option.map (fun (p, ts) ->
                                   Spath.short_look p,
                                   map (f ex) ts)
    }
  in
  try
    f [] t 
    |- fun _ ->
      let _t0 = t in
      let nodes = !nodes in
      let stubs = !stubs in
      iter (fun (t, stub) -> 
        try
          match assq t nodes with
          | {contents = Some t'} -> stub := `Linked t'
          | _ -> assert false
        with
        | Not_found ->
            (* !!% "ERROR: short_look Not_found: %a@." Stype_print.format (Tuple [t0; t]); *)
            !!% "ERROR: short_look Not_found: maybe non self-contained type?@.";
            raise Not_found
      ) stubs
  with
  | Not_found -> t
