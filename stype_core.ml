(* Type algebra, independent from type_expr and core_type.

   It is cumbersome, but we should have specif data types for our needs,
   type_expr and core_type could be very memory hungry.
*)

open Spotlib.Spot
open List

module OCaml = Ocaml
module OCaml_conv = Ocaml_conv

type 'a loop = 'a
(* Hack for conv(ocaml) with looped values *)

(* Unfortunately we have no easy way to handle looped value with meta_conv framework. *)
let ocaml_of_loop _ocaml_of_a _a = OCaml.String "loop"

type t = 
  | Link of [ `Linked of t loop | `Stub ] ref
  | Nil
  | VariantClosed of int
  | Any
  | VarNamed of int * string (* "" means anonymous *)
  | UnivarNamed of int * string (* "" means anonymous *)
  | Var of int
  | Univar of int
  | Arrow of string * t * t (* We ignore commutatibity *)
  | Tuple of t list
  | Constr of datatype * t list
  | Object of ((string * t) list * [ `Closed | `Open of t]) option (* This is always Some unless it is created by Ptyp_class *)
              * (Spath.t * t list) option
      (* Named class type: (t,..,t) P.ident

         | Class of Path.t * t list * string list
         for (t,..,t) #P.ident [> name .. name ] 

         #P.ident [> name] is deprecated syntax, so we simply ignore it.
      *)
  | Alias of t * string (** t as 'a, Only from [of_core_type] *)
  | Variant of xrow
  | Poly of t * t list
  | Package of Spath.t * (Spath.t * t) list
      (* S with type M.t = cty and ... *)
  | Attr of attr * t (** only used for printing *)

and attr = 
  [ `Ref of t ]

and xrow = {
  xrow_name : (Spath.t * t list) option; (* named type *)
  xrow_fields : xrow_fields;
  xrow_more : t; (** tvar for more *)
}

and xrow_fields = [
  | `Exact  of tag list               (** [ `A ] *)
  | `Open   of tag list               (** [> `A ] *)
  | `Closed of tag_full list * string list (** [< `A | `B | > `A ] *)
]

and tag = [
  | `Tag of string * t option
  | `Inherit of t
]

and tag_full = [
  | `Tag of string * bool (* zero arity exists *) * t list 
        (** false, []   :  impos
            true,  []   :  `A
            false, [t]  :  `A of t
            false, [ts] :  `A of t & t ..
            true,  [ts] :  `A of & t & t ..
        *)
  | `Inherit of t
]

and datatype = { dt_path : Spath.t;
                 dt_aliases : (t loop list * t loop) option option ref
               }

[@@deriving conv{ocaml_of}]

let oformat = Ocaml.format_with ocaml_of_t

let rec is_option = function
  | Constr ( { dt_path=Spath.SPdot (Spath.SPpredef, "option") }, [t2]) -> Some t2
  | Attr (_, t) -> is_option t
  | _ -> None

open Types
open Btype

exception Unsupported

module HXTypes = Hashtbl.Make(struct
  type _t = t
  type t = _t
  let equal t1 t2 = t1 == t2
  let hash = Hashtbl.hash
end)

let to_type_expr ?(anon_parameter_as_constr=false) pathfix ty =
  (* In one type_expr, id's must be unique *)
  let visited = HXTypes.create 1023 in
  let rec f ty = 
    try HXTypes.find visited ty with Not_found ->
    let link = newgenvar ~name:"link" () in
    HXTypes.replace visited ty link;
    let rec g = function
      | Attr (_, t) -> g t
      | Link { contents = `Stub } -> 
          Tconstr (Path.Pident (Ident.create "STRANGE!"), [], ref Mnil)
            (* assert false *)
      | Link { contents = `Linked ty } -> Tlink (f ty)
      | Alias _ -> raise Unsupported
      | Nil -> Tnil
      | VariantClosed _ -> Tnil
          
      | Any -> Tvar (Some "_")
      | Var _ -> Tvar None
      | VarNamed (_, "_") when anon_parameter_as_constr ->
          (* For printing _ parameter as "_". 
             This is not good if you really want type_expr *)
          Tconstr (Path.Pident (Ident.create "_"), [], ref Mnil)
      | VarNamed (_, s) -> Tvar (Some s)
      | Univar _ -> Tunivar None
      | UnivarNamed (_, s) -> Tunivar (Some s)
      | Arrow (l, ty1, ty2) when Btype.is_optional l ->
          let rec strip_attr = function
            | Attr (_, t) -> strip_attr t
            | t -> t
          in
          begin match strip_attr ty1 with
          | Constr ( { dt_path = p }, [ty1]) 
              when 
                begin match Spath.strip_attr p with
                | SPdot(Spath.SPpredef, "option") -> true
                | _ -> false
                end ->
              Tarrow (l, Predef.type_option (f ty1), f ty2, Cok (* ? *))
          | ty1' -> 
              !!% "Error: %a@." (Ocaml.format_with ocaml_of_t) ty1';
              assert false
          end
      | Arrow (l, ty1, ty2) -> Tarrow (l, f ty1, f ty2, Cok (* ? *))
      | Tuple tys -> Ttuple (map f tys)
      | Constr ({dt_path = path}, tys) -> Tconstr (pathfix path, map f tys, ref Mnil)
      | Object (None, _) -> assert false
      | Object (Some (fields, opened), nm) ->
          let fields = 
            fold_left (fun st (name, ty) ->
              newgenty (Tfield (name, Fpresent, f ty, st)))
              (match opened with
              | `Open ty -> f ty
              | `Closed -> newgenty Tnil)
              fields
          in
          let nm = match nm with
            | None -> None
            | Some (p, typs) ->
                Some (pathfix p, newgenvar () (* dummy: this info is lost but not used for printing *)
                         :: map f typs)
          in
          Tobject (fields, ref nm)
      | Poly (t, vars) -> Tpoly (f t, map f vars)
      | Package (p, pp_t_list) -> 
          Tpackage (pathfix p, map (Spath.to_longident ** fst) pp_t_list,
                    map (f ** snd) pp_t_list)

      | Variant xrow ->
          let fields = 
            match xrow.xrow_fields with
            | (`Exact tags | `Open tags) ->
                (* [ `A | `B ] *)
                map (function
                  | `Tag (t, topt) -> t, Rpresent (Option.map f topt)
                  | `Inherit _ -> raise Unsupported) tags
            | `Closed (tags, present) ->
                map (function
                  | `Inherit _ -> raise Unsupported
                  | `Tag (t, true, []) when mem t present ->
                      t, Rpresent None
                  | `Tag (t, false, [ty]) when mem t present ->
                      t, Rpresent (Some (f ty))
                  | `Tag (t, _, _) when mem t present -> assert false
                  | `Tag (t, b, ts) -> (* t is not in present *)
                      t, Reither (b, map f ts, false, ref None)) tags
          in
          Tvariant {
            row_fields = fields;
            row_more = f xrow.xrow_more;
            row_bound = ();
            row_closed = 
              begin match xrow.xrow_fields with
              | `Exact _ | `Closed _ -> true
              | `Open _ -> false
              end;
            row_fixed = 
              (* not used for printing *)
              begin match xrow.xrow_more with
              | Var _ | VarNamed _ | VariantClosed _ -> false
              | Univar _ | UnivarNamed _ | Constr _ -> true
              | _ -> assert false
              end;
            row_name = Option.map (fun (p,ts) -> pathfix p, map f ts) xrow.xrow_name;
          }
    in
    let res = g ty in
    link.desc <- res;
    link
  in
  f ty

(* CR jfuruse: unused *)
let _to_type_exprs pathfix tys =
  match (to_type_expr pathfix (Tuple tys)).desc with
  | Ttuple tys -> tys
  | _ -> assert false

let pathfix_for_printing opened p = 
  let open Path in
  match p with
  | Pdot (p, s, _) when opened = Some p && mem s Xpredef.predefined_types -> p
  | _ ->
      let rec fix = function
        | Pdot (Pident id, s, _n) when Ident.name id = "{predef}" -> Pident (Ident.create s)
        | Pdot (p, s, _n) when opened = Some p -> Pident (Ident.create s)
        | Pdot (p, s, n) -> Pdot (fix p, s, n)
        | (Pident _ as pid) -> pid
        | Papply (p1, p2) -> Papply(fix p1, fix p2)
      in
      fix p
  
let format_via_type_expr ppf ty = 
  let ty = to_type_expr ~anon_parameter_as_constr:true Spath.to_path ty in
  Type_expr.format ppf ty

let to_string_via_type_expr = Format.to_string format_via_type_expr
