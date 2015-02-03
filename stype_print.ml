open Spotlib.Spot
open List
open Stype_core

module OCaml = Ocaml
module OCaml_conv = Ocaml_conv

let oformat = Stype_core.oformat

type label = string [@@deriving conv{ocaml}]

type o = 
  | Otyp_alias   of o * string
  | Otyp_any
  | Otyp_arrow   of label * o * o
  | Otyp_class   of bool * Spath.t * o list
  | Otyp_constr  of Spath.t * o list
  | Otyp_link    of o
  | Otyp_module  of Spath.t * Spath.t list * o list
  | Otyp_object  of (string * o) list * bool option
  | Otyp_poly    of string list * o
  | Otyp_stuff   of string
  | Otyp_tuple   of o list
  | Otyp_var     of bool * string
  | Otyp_variant of bool 
                    * [ `Ovar_fields of (string * bool * o list) list (* tag *) * o list (* inherit *)
                      | `Ovar_name of Spath.t * o list ] 
                    * bool 
                    * string list option
  | Otyp_attr    of attr * o

and attr = 
  [ `Ref of t ]

[@@deriving conv{ocaml_of}]

let rec repr = function
  | Link {contents= `Linked ty} -> repr ty
  | Link {contents= `Stub} -> assert false
  | ty -> ty

(* static = it has no Either thing *)
let static_row row = 
  match row.xrow_fields with
  | `Exact _ -> true
  | `Open _ -> false
  | `Closed (tags, present) -> List.length tags = List.length present

(* This scans the added fields and find the terminal.
   The terminal can be a variable or nil (closed?)
*)
let row_more row = row.xrow_more

(* If the type has a row var, it returns it, otherwise, it returns the type itself.
   [ `A ] ==> [ `A ]
   [> `A ] (as 'a) == > 'a
*)
let proxy ty =
  match ty with
  | Variant row when not (static_row row) -> row_more row
  | Object (None, None) -> assert false
  | Object (None, Some _) -> ty
  | Object (Some (_, `Open t), _named) -> t
  | _ -> ty

let aliasable ty =
  match ty with
  | Var _ | VarNamed _ | Univar _ | UnivarNamed _ | Poly _ -> false
  | Constr (_p, _) ->
      (* 
         (match best_type_path p with (_, Nth _) -> false | _ -> true)
      *)
      true
  | _ -> true

let rec iter_row f row =
  begin match row.xrow_fields with
  | `Exact tags | `Open tags ->
      flip List.iter tags (function
        | `Tag (_, topt) -> Option.iter f topt
        | `Inherit t -> f t)
  | `Closed (tags, _present) ->
      flip List.iter tags (function
        | `Tag (_, _, ts) -> List.iter f ts
        | `Inherit t -> f t)
  end;
  match row.xrow_more with
  | Variant row -> iter_row f row
  | _ -> Misc.may (fun (_,l) -> List.iter f l) row.xrow_name

(* ctype *)
let opened_object ty =
  match ty with
  | Object (Some (_, `Open _t), _) -> true
  | _ -> false

(* Print a type expression *)

let names = ref ([] : (t * string) list)
let name_counter = ref 0
let named_vars = ref ([] : string list)

let reset_names () = names := []; name_counter := 0; named_vars := []
let add_named_var ty =
  match ty with
    VarNamed (_, name) | UnivarNamed (_, name) ->
      if List.mem name !named_vars then () else
      named_vars := name :: !named_vars
  | _ -> ()

let visited_objects = ref ([] : t list)
let aliased = ref ([] : t list)
let delayed = ref ([] : t list)

let add_delayed t =
  if not (List.memq t !delayed) then delayed := t :: !delayed

let is_aliased ty = List.memq (proxy ty) !aliased

(* So proxy is the point where the types are aliased *)  
let add_alias ty =
  let px = proxy ty in
  if not (is_aliased px) then begin
    aliased := px :: !aliased;
    add_named_var px
  end

let namable_row row =
  row.xrow_name <> None &&
  match row.xrow_fields with
  | `Exact _ | `Open _ -> true
  | `Closed (tags, _present) ->
      List.for_all (function
        | `Inherit _ -> true
        | `Tag (_, false, [_])
        | `Tag (_, true, []) -> true
        | _ -> false) tags

let rec mark_loops_rec visited ty =
  let ty = repr ty in
  let px = proxy ty in
  if List.memq px visited && aliasable ty then add_alias px else
    let visited = px :: visited in
    match ty with
    | Attr (_, ty) -> mark_loops_rec visited ty
    | Alias (ty, _s) -> mark_loops_rec visited ty
    | Any -> ()
    | Var _ | VarNamed _ -> add_named_var ty
    | Arrow(_, ty1, ty2) ->
        mark_loops_rec visited ty1; mark_loops_rec visited ty2
    | Tuple tyl -> List.iter (mark_loops_rec visited) tyl
    | Constr(_p, tyl) ->
(*
        let (p', s) = best_type_path p in
        List.iter (mark_loops_rec visited) (apply_subst s tyl)
*)
        List.iter (mark_loops_rec visited) tyl
    | Package (_, ptyl) ->
        List.iter (fun (_, ty) -> mark_loops_rec visited ty) ptyl
    | Variant row ->
        if List.memq px !visited_objects then add_alias px else
         begin
          if not (static_row row) then
            visited_objects := px :: !visited_objects;
          match row.xrow_name with
          | Some(_p, tyl) when namable_row row ->
              List.iter (mark_loops_rec visited) tyl
          | _ ->
              iter_row (mark_loops_rec visited) row
         end
    | Object (fi, nm) ->
        if List.memq px !visited_objects then add_alias px else
         begin
          if opened_object ty then
            visited_objects := px :: !visited_objects;
          begin match nm with
          | None ->
              let fields = match fi with
                | None -> assert false
                | Some (fields, _) -> fields
              in
              List.iter
                (fun (_, ty) ->
                  mark_loops_rec visited ty)
                fields
          | Some (_, l) ->
              List.iter (mark_loops_rec visited) l
          end
        end
(*
    | Field(_, kind, ty1, ty2) when field_kind_repr kind = Fpresent ->
        mark_loops_rec visited ty1; mark_loops_rec visited ty2
    | Tfield(_, _, _, ty2) ->
        mark_loops_rec visited ty2
*)
    | Nil -> ()
    | VariantClosed _ -> ()
(*
      | Link {contents= `Linked ty} -> mark_loops_rec visited ty
*)
    | Link _ -> assert false
    | Poly (ty, tyl) ->
        List.iter (fun t -> add_alias t) tyl;
        mark_loops_rec visited ty
    | Univar _ | UnivarNamed _ -> add_named_var ty

let mark_loops ty =
  (* normalize_type Env.empty ty; *) (* I bet things are all normalized *)
  mark_loops_rec [] ty

let reset_loop_marks () =
  visited_objects := []; aliased := []; delayed := []

let reset () =
  (* unique_names := Ident.empty; *)
  reset_names (); reset_loop_marks ()

let reset_and_mark_loops ty =
  reset (); mark_loops ty

let rec new_name () =
  let name =
    if !name_counter < 26
    then String.make 1 (Char.chr(97 + !name_counter))
    else String.make 1 (Char.chr(97 + !name_counter mod 26)) ^
           string_of_int(!name_counter / 26) in
  incr name_counter;
  if List.mem name !named_vars
  || List.exists (fun (_, name') -> name = name') !names
  then new_name ()
  else name

let remove_names tyl =
  names := List.filter (fun (ty,_) -> not (List.memq ty tyl)) !names

let name_of_type t =
  (* We've already been through repr at this stage, so t is our representative
     of the union-find class. *)
  try List.assq t !names with Not_found ->
    let name =
      match t with
      | VarNamed (_, name) | UnivarNamed (_, name) ->
          (* Some part of the type we've already printed has assigned another
           * unification variable to that name. We want to keep the name, so try
           * adding a number until we find a name that's not taken. *)
          let current_name = ref name in
          let i = ref 0 in
          while List.exists (fun (_, name') -> !current_name = name') !names do
            current_name := name ^ (string_of_int !i);
            i := !i + 1;
          done;
          !current_name
      | _ ->
          (* No name available, create a new one *)
          new_name ()
    in
    (* Exception for type declarations *)
    if name <> "_" then names := (t, name) :: !names;
    name

let check_name_of_type t = ignore(name_of_type t)

let is_optional = Btype.is_optional

let tree_of_path (spath : Spath.t) = spath

let rec tree_of_typexp sch ty =
  let ty = repr ty in
  let px = proxy ty in
  (* CR jfuruse: t is maximally shared, so this may print something too aliased? *)
  if List.mem_assq px !names && not (List.memq px !delayed) then
   let mark = is_non_gen sch ty in
   Otyp_var (mark, name_of_type px) 
  else

  let pr_typ () =
    match ty with
    (* CR jfuruse: so far, only the leaf nodes are attred in the outcometree *)  
    | Attr (r, (Var _ | Univar _ | VarNamed _ | UnivarNamed _ as ty)) -> 
        Otyp_attr (r, tree_of_typexp sch ty)
    | Attr (_r, ty) -> tree_of_typexp sch ty (* CR jfuruse: TODO *) 
    | Any -> Otyp_any
    | Alias (ty, s) -> Otyp_alias (tree_of_typexp sch ty, s)
    | VarNamed (_, "_") ->
        (* Workaround for GADT parameter type *)
        Otyp_constr (Spath.nhc_ident "_", [])
    | Var _ | VarNamed _ ->
        Otyp_var (is_non_gen sch ty, name_of_type ty)
    | Arrow(l, ty1, ty2) ->
        let pr_arrow l ty1 ty2 =
          let lab =
            if l <> "" || is_optional l then l else ""
          in
          let t1 =
            if is_optional l then
              match is_option ty1 with
              | Some ty -> tree_of_typexp sch ty
              | _ -> Otyp_stuff "<hidden>"
            else tree_of_typexp sch ty1 in
          Otyp_arrow (lab, t1, tree_of_typexp sch ty2) in
        pr_arrow l ty1 ty2
    | Tuple tyl ->
        Otyp_tuple (tree_of_typlist sch tyl)
    | Constr(p, tyl) ->
        Otyp_constr (tree_of_path p.dt_path, tree_of_typlist sch tyl)
    | Variant row ->
        let closed = 
          match row.xrow_fields with
          | `Closed _ | `Exact _ -> true
          | _ -> false
        in
        let all_present = 
          match row.xrow_fields with
          | `Closed (tags, present) ->
              not (exists (function `Inherit _ -> true | _ -> false) tags)
              && length (filter (function `Tag _ -> true | _ -> false) tags) = length present
          | _ -> true
        in
        let present = 
          match row.xrow_fields with
          | `Closed (_tags, present) -> present
          | _ -> []
        in
        begin match row.xrow_name with
        | Some (p, tyl) when namable_row row ->
            let id = tree_of_path p in
            let args = tree_of_typlist sch tyl in
            if closed && all_present then
              Otyp_constr (id, args)
            else
              let non_gen = is_non_gen sch px in
              let tags =
                if all_present then None else Some present in
              Otyp_variant (non_gen, `Ovar_name(id, args),
                            closed, tags)
        | _ ->
            let non_gen = not (closed && all_present) && is_non_gen sch px in
            let fields_tag, fields_inherit = tree_of_row_fields sch row.xrow_fields in
            let tags = if all_present then None else Some present in
            Otyp_variant (non_gen, `Ovar_fields (fields_tag, fields_inherit), closed, tags)
        end
    | Object (fi, nm) ->
        tree_of_typobject sch (`Fields fi) nm
    | Nil (* | Tfield _ *) | VariantClosed _ ->
        tree_of_typobject sch `Nil None
    | Link {contents = `Linked ty} ->
        Otyp_link (tree_of_typexp sch ty) (* loop found *)
    | Link _ -> assert false
    | Poly (ty, []) ->
        Otyp_poly ([], tree_of_typexp sch ty)
    | Poly (ty, tyl) ->
        (*let print_names () =
          List.iter (fun (_, name) -> prerr_string (name ^ " ")) !names;
          prerr_string "; " in *)
        if tyl = [] then Otyp_poly ([], tree_of_typexp sch ty) else begin
          let old_delayed = !delayed in
          (* Make the names delayed, so that the real type is
             printed once when used as proxy *)
          List.iter add_delayed tyl;
          let tl = List.map name_of_type tyl in
          let tr = Otyp_poly (tl, tree_of_typexp sch ty) in
          (* Forget names when we leave scope *)
          remove_names tyl;
          delayed := old_delayed; tr
        end
    | Univar _ | UnivarNamed _ ->
        Otyp_var (false, name_of_type ty)
    | Package (p, ntyl) ->
        let n = List.map (fun (n, _) -> n) ntyl in 
        Otyp_module (p, n, tree_of_typlist sch (List.map snd ntyl))
  in
  if List.memq px !delayed then delayed := List.filter ((!=) px) !delayed;
  if is_aliased px && aliasable ty then begin
    check_name_of_type px;
    Otyp_alias (pr_typ (), name_of_type px) end
  else pr_typ ()

and tree_of_row_fields sch = function
  | (`Open tags | `Exact tags) ->
      filter_map (function
        | `Tag (l, None) -> Some (l, true, [])
        | `Tag (l, Some t) -> Some (l, false, [tree_of_typexp sch t])
        | _ -> None) tags,
      filter_map (function
        | `Inherit t -> Some (tree_of_typexp sch t)
        | _ -> None) tags
  | `Closed (tags, _present) ->
      filter_map (function
        | `Tag (_l, false, []) -> assert false
        | `Tag (l, b, ts) -> Some (l, b, map (tree_of_typexp sch) ts)
        | _ -> None) tags,
      filter_map (function
        | `Inherit t -> Some (tree_of_typexp sch t)
        | _ -> None) tags

and tree_of_typlist sch tyl =
  List.map (tree_of_typexp sch) tyl

and tree_of_typobject sch fi nm =
  begin match nm with
  | None ->
      let pr_fields fi =
        match fi with
        | `Nil -> ["?Nil?", Otyp_any], None 
        (* assert false *) (* CR jfuruse: very strange but we have this ... *)
(*
0 : Some (OPAM cow.0.10.1/_build/lib/markdown.mli File "lib/markdown.mli", line 17, characters 5-15):
    type <<t>> =
      [ `Data of string
        | `El of ((string * string) * ((string * string) * string) list)
                 * [ `Data of string
                     | `El of ((string * string)
                               * ((string * string) * string) list)
                               * < ?Nil? : _ > list ]        <-- this should printed as 'a
                     frag list ]
        list
    Packs: cow

  I guess this is a bug around type alias expansion.
*)
        | `Fields None -> assert false
        | `Fields (Some (fields, rest)) ->
            let present_fields = fields in
            let sorted_fields =
              List.sort (fun (n, _) (n', _) -> compare n n') present_fields in
            tree_of_typfields sch rest sorted_fields 
      in
      let (fields, rest) = pr_fields fi in
      Otyp_object (fields, rest)
  | Some (p, tyl) ->
      (* let non_gen = is_non_gen sch ty in *)
      let non_gen = false in
      let args = tree_of_typlist sch tyl in
      Otyp_class (non_gen, tree_of_path p, args)
  end

and is_non_gen _sch _ty = false
  (* sch && is_Tvar ty && ty.level <> generic_level *)

and tree_of_typfields sch rest = function
  | [] ->
      let rest =
        match rest with
        | `Open _ -> Some true (* no mono version *)
        | `Closed -> None
      in
      ([], rest)
  | (s, t) :: l ->
      let field = (s, tree_of_typexp sch t) in
      let (fields, rest) = tree_of_typfields sch rest l in
      (field :: fields, rest)

open Printer
open Printer.OCaml

let ( ^-> ) = 
  let mbinop assoc lev sep = 
    binop assoc lev ~op:(space ++ string sep ++ string " ") 
  in
  mbinop Right 0.9 "->"

let ty_app = binop Left 100.0 ~op:space (* CR jfuruse: contiguous spaces must be contracted *)

let tree spath o = 

  let rec f o =

    let list' first sep pprs end_ = 
      reset (* level 0.0 *)
      & 
        match pprs with
        | [] -> first ++ end_
        | _ -> 
            list 1.0 (* not to have parens: 0.0 < 1.0 *) cut 
              (List.mapi (fun n ppr -> 
                (if n = 0 then first else sep) 
                (* each ppr's level is reset to 0.0. CRv2 jfuruse: should be configurable? *)
                ++ level 0.0 ppr) pprs) ++ end_
    in
    match o with
    | Otyp_attr (_attr, o) -> attr `Bold & f o (* CR jfuruse: todo *)
    | Otyp_alias (o, n) -> ty_as (f o) (string ("'" ^ n))
    | Otyp_any -> string "_"
    | Otyp_arrow ("", o1, o2) -> 
       binop Right 0.9 ~op:(space ++ string "->" ++ string " ") 
             (f o1) (f o2)
    | Otyp_arrow (l, o1, o2) -> 
       (string (l ^ ":") ++ f o1) ^-> f o2
    | Otyp_class (_, p, []) -> string "#" ++ spath p
    | Otyp_constr (p, []) ->  spath p
    | Otyp_class (_, p, os) -> box 2 & ty_app (typarams os) (string "#" ++ spath p)
    | Otyp_constr (p, os) -> box 2 & ty_app (typarams os) (spath p)
    | Otyp_link _o -> string "LINK"
    | Otyp_module (p, [], []) -> 
        parens "(" ")" & string "module " ++ spath p 
    | Otyp_module (p, ps, os) -> 
        parens "(" ")" & string "module " ++ spath p ++ space 
                         ++ list' (string "with ") (string "and  ")
                              (List.map2 (fun p o -> 
                                string "type " ++ spath p ++ string " = " ++ f o) ps os)
                              nop
    | Otyp_object (los, None) ->  list' (string "< ") (string "; ") (List.map field los) (string " >")
    | Otyp_object (los, Some true) ->  list' (string "< ") (string "; ") (List.map field los @ [string ".."]) (string " >")
    | Otyp_object (los, Some false) ->  list' (string "< ") (string "; ") (List.map field los @ [string "_.."]) (string " >")
    | Otyp_poly ([], o) -> f o
    | Otyp_poly (vs, o) -> 
        (* t -> ('a . t), so . is Right assoc and weaker than -> (0.9) *)
        (* as (0.6) is stronger than . *)
        let mbinop assoc lev sep = binop assoc lev ~op:(string sep ++ space) in
        mbinop Right 0.55 "." (list 1.0 space (List.map (fun v -> string & "'" ^ v) vs)) (box 0 (f o))
    | Otyp_stuff s -> string s
    | Otyp_tuple os -> ty_tuple & List.map f os
    | Otyp_var (non_gen, s) -> string & "'" ^ (if non_gen then "_" else "") ^ s
    | Otyp_variant ( non_gen, fields_names, closed, tags ) ->
        let present = match tags with
            | None | Some [] -> nop
            | Some l -> list' (space ++ string "> ") space (List.map (fun t -> string & "`" ^ t) l) nop
        in
        let row_field (l, opt_amp, tyl) = 
          string ("`" ^ l) 
          ++ match opt_amp, tyl with
             | false, [] -> assert false
             | true, [] -> nop
             | true, tyl -> string " of" ++ box 0 (list' (string " & ") (space ++ string "& ") (List.map (box 0 *< f) tyl) nop)
             | false, tyl -> string " of " ++ box 0 (list' (string "") (space ++ string "& ") (List.map (box 0 *< f) tyl) nop)
        in
        let fields = match fields_names with
          | `Ovar_fields (fields_tag, fields_inherit) -> 
              List.map row_field fields_tag @ List.map f fields_inherit
          | `Ovar_name (id, []) -> [ spath id ]
          | `Ovar_name (id, tyl) -> [ box 2 & ty_app (typarams tyl) (spath id) ]
        in
        let direction, sep = 
          match closed, tags with
          | true, None  -> "[ ",  "| "
          | true, _     -> "[< ", "| "
          | false, None -> "[> ", "| "
          | false, Some [] -> "[> ", "| " (* CR jfuruse: strange... *)
          | false, _    -> "[? ", "| "
        in
        (if non_gen then string "_" else nop)
        ++ list' (string direction) (space ++ string sep) fields nop
        ++ present ++ string " ]"
  
  and typarams = function
    | [] -> assert false
    | [o] -> f o
    | os -> 
        let tuple = list 0.5 (* weaker than ty_as *) (string "," ++ space) in
        parens "(" ")" & tuple & List.map f os
  
  and field (l, o) = string (l ^ " : ") ++ box 0 (f o)
  in
  f o

let print spath xty = 
  reset_and_mark_loops xty;
  let o = tree_of_typexp true xty in
  tree spath o

let format_gen spath ppf xty =
  reset_and_mark_loops xty;
  let o = tree_of_typexp true xty in
  Printer.format (tree spath) ppf o

let format = format_gen Spath.print

let to_string = Format.to_string format

let show t = Format.to_string (format_gen (fun t -> Spath.print ~packages:`Exact ~predef:true t)) t

let read str = 
  Util.ParseError.catch 
    (fun lexbuf ->
      let ty = Pattern_escape.unescape_core_type & XParser.poly_type Lexer.token lexbuf in
      !!% "core_type: %a@." Pprintast.core_type ty;
      let sty = Stype_conv.of_core_type ty in
      !!% "stype: %a@." format sty;
      `Ok sty)
    (Lexing.from_string & Pattern_escape.escape_query str)

let cannot_read = function
  | Arrow ("", _, Poly _) -> true
  | VarNamed (_, "_") -> true   (* _ is printed as '_ *)
  | _ -> false
