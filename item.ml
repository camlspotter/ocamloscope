(* CR jfuruse: Elem or Item ?*)
open Spotlib.Spot
open List

(* open Asttypes *)
open Ocaml_conv

open Stype_core

type virtual_flag = Asttypes.virtual_flag = Virtual | Concrete with conv(ocaml)
type private_flag = Asttypes.private_flag = Private | Public with conv(ocaml)

type 'typ kind = 
  | Class
  | ClassType
  | ClassField of virtual_flag * 'typ
  | Constr     of 'typ
  | Exception  of 'typ
  | Field      of 'typ
  | Method     of private_flag * virtual_flag * 'typ
  | ModType
  | Module
  | Type       of 'typ list (** type params *) * 'typ option * [ `Abstract | `Record | `Variant ]
  | Value      of 'typ
  | Package    of OCamlFind.Package.t * string list (** Ex. [ "Dbm" ] *)
with conv(ocaml)

let name_of_kind = function
  | Class        -> "class"
  | ClassType    -> "class type"
  | ClassField _ -> "class val"
  | Constr _     -> "constr"
  | Exception _  -> "exception"
  | Field _      -> "field"
  | Method _     -> "method"
  | ModType      -> "module type"
  | Module       -> "module"
  | Type _       -> "type"
  | Value _      -> "val"
  | Package _    -> "package"

let types_of_kind = function
  | ClassField (_, ty)
  | Constr ty 
  | Exception ty
  | Field ty
  | Value ty
  | Method (_, _, ty) -> [ty]
  | Type (tys, Some ty, _) -> ty :: tys
  | Type (tys, None, _) -> tys
  | Class
  | ClassType 
  | ModType
  | Module
  | Package _ -> []

type ('packs, 'path, 'loc, 'doc, 'typ)  record = {
    packs : 'packs;
    path  : 'path;
    loc   : 'loc;
    doc   : 'doc;
    kind  : 'typ kind;
  } with conv(ocaml)

type ('a, 'b) result_t = [ `Ok of 'a | `Error of 'b ] with conv(ocaml) (* = Result.t *)

let rebind_error x = function
  | `Ok v -> `Ok v
  | `Error s -> `Error (Meta_conv.Error.Primitive_decoding_failure s, x, [`Node x])

type spath_t = Spath.t
let ocaml_of_spath_t l = Ocaml_conv.ocaml_of_string & Spath.show l
let spath_t_of_ocaml ?trace x = 
  let open Result in
  Ocaml_conv.string_of_ocaml ?trace x >>= fun y ->
      rebind_error x (Spath.read y)
let _spath_t_of_ocaml_exn = Ocaml_conv.exn spath_t_of_ocaml

type stype_t = Stype.t
let ocaml_of_stype_t l = Ocaml_conv.ocaml_of_string & Stype_print.show l
let stype_t_of_ocaml ?trace x =
  let open Result in
  Ocaml_conv.string_of_ocaml ?trace x 
  >>= fun y -> rebind_error x (Stype_print.read y)

let _stype_t_of_ocaml_exn = Ocaml_conv.exn stype_t_of_ocaml

type t = (OCamlFind.Packages.t,
	  spath_t, 
	  Loc.t option, 
	  (OCamlDoc.t option, unit) result_t,
	  stype_t) record
with conv(ocaml)

(* do not hcons itself: It is unlikely we have duped kinds throughout items *)
let rec_hcons_k p = function
  | ClassField (vf, ty) -> ClassField (vf, Stype.rec_hcons ty)
  | Constr ty -> Constr (Stype.rec_hcons ty)
  | Exception ty -> Exception (Stype.rec_hcons ty)
  | Field ty -> Field (Stype.rec_hcons ty)
  | Method (pf, vf, ty) -> Method (pf, vf, Stype.rec_hcons ty)
  | Type (tys, tyopt, attr) -> 
      let tys = map Stype.rec_hcons tys in
      let tyopt = Option.map Stype.rec_hcons tyopt in
      let {Stype_core.dt_aliases= alias} = Stype_hcons.rec_hcons_datatype {dt_path= Spath.rec_hcons p; dt_aliases= ref None} in
      begin 
        let new_alias = 
          match tyopt with
          | None -> None
          | Some ty -> 
              (*
                !!% "LOG: data type %a has an alias!@." (Spath.format ()) p;
              *)
              Some (tys, ty)
        in
        match !alias with
        | None -> alias := Some new_alias
        | Some _ -> 
            !!% "@[<2>WARNING: data type %a has more than one aliases!@." 
              (Spath.format ()) p;
(* CR jfuruse:
WARNING: data type {core_kernel#74}.Core_kernel.Std.Hashtbl_intf.Hashable.t has more than one aliases!

include Std_kernel
include Std_common

Both Std_kernel and Std_common has Hashtbl_intf.Hashable.t I guess. Shadowing must be implemented.
*)
            alias := Some new_alias
      end;
      Type (tys, tyopt, attr)
  | Value ty -> Value (Stype.rec_hcons ty)
  | (Class | ClassType | ModType | Module as k) -> k
  | Package (p, paths) -> Package (p, map Hcons.string paths)

let rec_hcons i =
  let p = Spath.rec_hcons i.path in
  { i with 
    path= p;
    loc= Option.map Loc.rec_hcons i.loc;
    kind= rec_hcons_k p i.kind; 
  }

let format_gen ?(dont_omit_opened=false) ppf { packs; path; loc; doc; kind } =
  let open Format in

  let rec get_opened = function
    | Spath.SPdot (p, _) -> Some p
    | SPattr (_, p) -> get_opened p
    | _ -> None
  in
  let opened = 
    if dont_omit_opened then None
    else
      match kind with
      | Method _ -> 
          (* P.cls.m : P is opened *)
          Option.bind (get_opened path) get_opened
      | _ -> get_opened path
  in

  let format_stype = Stype.format_gen (fun x -> Spath.print ?opened x) in
  let format_stype_param ppf = function
    | VarNamed (_, "_") -> string ppf "_"
    | ty -> format_stype ppf ty
  in
  let format_packs ppf ps =
    fprintf ppf "@[Packs:@ @[%a@]@]"
      (Format.list ",@ " Format.string)
      (OCamlFind.Packages.to_strings ps)
  in
  let format_doc ppf = function
    | `Ok None -> ()
    | `Ok (Some info) -> fprintf ppf "Doc: @[%a@]" OCamlDoc.format info
    | `Error () -> fprintf ppf "Doc: failed"
  in
  match kind with
  | Value ty ->
      fprintf ppf  "@[<v>%a:@ val @[%a@ : @[%a@]@]@ %a@ %a@]"
        (Format.option Loc.format) loc
        (Spath.format ()) path
        format_stype ty
        format_packs packs
        format_doc doc

  | Exception ty ->
      fprintf ppf  "@[<v>%a:@ exception @[%a@ : @[%a@]@]@ %a@ %a@]"
        (Format.option Loc.format) loc
        (Spath.format ()) path
        format_stype ty
        format_packs packs
        format_doc doc

  | Module ->
      fprintf ppf  "@[<v>%a:@ module %a@ %a@ %a@]"
        (Format.option Loc.format) loc
        (Spath.format ()) path
        format_packs packs
        format_doc doc

  | ModType ->
      fprintf ppf "@[<v>%a:@ module type %a@ %a@ %a@]"
        (Format.option Loc.format) loc
        (Spath.format ()) path
        format_packs packs
        format_doc doc

  | ClassType ->
      fprintf ppf  "@[<v>%a:@ class type %a@ %a@ %a@]"
        (Format.option Loc.format) loc
        (Spath.format ()) path
        format_packs packs
        format_doc doc

  | Type (params, tyop, k) ->
      fprintf ppf "@[<v>%a:@ @[<v2>type %a%a %t@]@ %a@ %a@]"
        (Format.option Loc.format) loc
        (fun ppf -> function
          | [] -> ()
          | [param] -> format_stype_param ppf param; string ppf " "
          | params ->
              fprintf ppf "(@[%a@]) "
                (Format.list ",@ " format_stype) params) params
        (Spath.format ()) path
        (fun ppf ->
          match tyop, k with
          | None, `Abstract -> pp_print_string ppf "(* abstract *)"
          | None, `Record -> pp_print_string ppf "= { .. }"
          | None, `Variant -> pp_print_string ppf "= .. | .."
          | Some ty, `Abstract ->
              fprintf ppf "=@ @[%a@]" format_stype ty
          | Some ty, `Record ->
              fprintf ppf "= @[%a@] =@ { .. }" format_stype ty
          | Some ty, `Variant ->
              fprintf ppf "= @[%a@] =@ .. | .." format_stype ty)
        format_packs packs
        format_doc doc

  | Constr ty ->
      fprintf ppf  "@[<v>%a:@ constr @[%a@ : @[%a@]@]@ %a@ %a@]"
        (Format.option Loc.format) loc
        (Spath.format ()) path
        format_stype ty
        format_packs packs
        format_doc doc

  | Field ty ->
      fprintf ppf  "@[<v>%a:@ field @[%a@ : @[%a@]@]@ %a@ %a@]"
        (Format.option Loc.format) loc
        (Spath.format ()) path
        format_stype ty
        format_packs packs
        format_doc doc

  | Class ->
      fprintf ppf  "@[<v>%a:@ class %a@ %a@ %a@]"
        (Format.option Loc.format) loc
        (Spath.format ()) path
        format_packs packs
        format_doc doc

  | ClassField (v, ty) ->
      fprintf ppf  "@[<v>%a:@ class val%s @[%a@ : @[%a@]@]@ %a@ %a@]"
        (Format.option Loc.format) loc
        (match v with Virtual -> " virtual" | Concrete -> "")
        (Spath.format ()) path
        format_stype ty
        format_packs packs
        format_doc doc

  | Method (p, v, ty) ->
      fprintf ppf  "@[<v>%a:@ method%s @[%a@ : @[%a@]@]@ %a@ %a@]"
        (Format.option Loc.format) loc
        (match p, v with
        | Private,  Virtual -> " private virtual"
        | Private,  Concrete -> " private"
        | Public,  Virtual -> " virtual"
        | Public,  Concrete -> ""
        )
        (Spath.format ()) path
        format_stype ty
        format_packs packs
        format_doc doc

  | Package (_p, _mods) -> (* CR jfuruse: todo *)
      fprintf ppf  "@[<v>%a:@ package %a@]"
        (Format.option Loc.format) loc
        (Spath.format ()) path

let format = format_gen ~dont_omit_opened:false
