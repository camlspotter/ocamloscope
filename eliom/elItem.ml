(* eliom item renderer *)

open Spotlib.Spot
open Eliom_content.Html5.F
open ElMisc
(* CR jfuruse: We cannot open List, since it corrides with Eliom at too many places *)
open Item

let rec print = 
  let open Printer.Token in
  function
  | String s -> [ !$ s ]
  | Box (_, t) -> print t
  | VBox (_, t) -> print t
  | Cut -> []
  | Space -> [ !$ " " ]
  | Flush -> []
  | Seq ts -> [ span & List.concat_map print ts ]
  | NOP -> []
  | Attr (`Bold, t) -> [ span_class "high" & print t ]

(*
let comment s = span_class "comment" [ !$% "(* %s *)" s ]
*)

let ppath
      ?remove_package_names
      ?just_postfix
      ?opened
      p = 
  print (Spath.print ?remove_package_names
                     ?just_postfix
                     ?opened
                     p
                     Noassoc 0.0)

(*
let name_ppath p = div_class "name" & ppath p
*)

let type_expr ?opened ty =
  let ppath_token p = Spath.print ~packages:`Nick ?opened p in
  div_class "type"
  & print
  & Stype.print ppath_token ty Noassoc 0.0

(* CR jfuruse: should be done in the printer level *)
let type_params params = 
  div &
  match params with
  | [] -> []
  | [param] -> [ type_expr param; !$ " " ]
  | params -> 
     !$ "(" 
     :: List.(intersperse (!$ ", ") & map type_expr params) 
     @ [ !$ ") " ]

let packages ps =
  div_class "packages" 
    ( (* !$ "Packages: "
    ::*)
      (List.intersperse (!$ ", ")
        & List.map (fun name -> 
          ElServices.Package.a [ !$ name ] name)
        & List.sort compare & OCamlFind.Packages.to_strings ps))

let odoc info =
  match info with
  | `Ok infoopt -> 
      let infostring = 
        match infoopt with
        | None -> ""
        | Some info -> OCamlDoc.to_string info
      in
      div_class "info" 
        [ !$ infostring ]
  | `Error _ ->
      div_class "info" [ !$ "ocamldoc failed" ]

(* grouped layout *)

let space = !$ " "

let colon_type opened ty = 
  div [ span_class "sep" [ !$ ":" ]
      ; space
      ; type_expr ?opened ty
      ]

let equal_type opened ty = 
  div [ span_class "sep" [ !$ "=" ]
      ; space
      ; type_expr ?opened ty
      ]


let kind = function
  | Value _ -> []
  | k -> [ !$ (Item.name_of_kind k); space ] 

(* CR jfuruse: short look is already calculated at grouping,
               therefore we should not calculate it here again. *)
let entry_group_head p k =

  let html_postfix = 
    span_class "path" 
    & ppath (*~just_postfix:true*) p
  in

(*
  let rec opened = function
    | Spath.SPdot (p, _) -> Some p
    | Spath.SPattr (_, p) -> opened p
    | _ -> None
  in

  let colon_type = colon_type & opened p in
  let equal_type = equal_type & opened p in
*)

  let colon_type = colon_type None in
  let equal_type = equal_type None in

  let ent x = div_class "group_head" [ div_class "line" (kind k @ x) ] in

  match k with
  | Class ->
      ent [ html_postfix
          ]
  | ClassType ->
      ent [ html_postfix
          ]
  
  | ClassField (_virtual_flag, ty) ->
      ent [ html_postfix
          ; space
          ; colon_type ty
          ]
              
  | Constr ty -> (* CR jfuruse: private? *)
      ent [ html_postfix
          ; space
          ; colon_type ty
          ]
  
  | Exception ty ->
      ent [ html_postfix
          ; space
          ; colon_type ty
          ]
  
  | Field ty ->
      ent [ html_postfix 
          ; space
          ; colon_type ty
          ]
        
  | Method (_private_flag, _virtual_flag, ty) ->
      ent [ html_postfix
          ; space
          ; colon_type ty
          ]

  | ModType ->
      ent [ html_postfix
          ]

  | Module ->
      ent [ html_postfix
          ]
        
  | Type (params, Some ty, _k) ->
      ent [ type_params params
          ; space
          ; html_postfix
          ; space
          ; equal_type ty
          ]
  
  | Type (params, None, _k) ->
      ent [ type_params params
          ; space
          ; html_postfix
          ]
  
  | Value ty ->
      ent [ html_postfix
          ; space
          ; colon_type ty
          ]
  
  | Package _ ->
      ent [ html_postfix
          ]
(*
          ; div_class "info" [ !$ (Option.default (OCamlFind.Package.find_var "description" pack) (fun _ -> "no description")) ]
*)

let entry_group_body_elem (_id, i) =
  (* CR jfuruse: Now the code is in item.ml *)
  let rec get_opened = function
    | Spath.SPdot (p, _) -> Some p
    | SPattr (_, p) -> get_opened p
    | _ -> None
  in
  let opened = match i.kind with
    | Method _ -> 
        (* P.cls.m : P is opened *)
        Option.bind (get_opened i.path) get_opened
    | _ -> get_opened i.path
  in
  let colon_type = colon_type opened in
  let equal_type = equal_type opened in

  let path =
      span_class "path"
      & ppath ~remove_package_names:true i.path
  in

  let ent x = div_class "line" (kind i.kind @ x) in

  let to_source = match i.loc with
    | None -> div []
    | Some l -> 
        match Loc.id l with
        | None -> div []
        | Some (p,md5,l) ->
            div_class "source-link" [ ElServices.Source.a ~fragment:(ElSource.line_id l) [ !$ "src" ] (p, (md5, l)) ]
  in

  let main = match i.kind with
    | Class ->
        ent [ path 
            ; to_source
            ]
              
    | ClassType ->
        ent [ path
            ; to_source
            ]
  
    | ClassField (_virtual_flag, ty) ->
        ent [ path
            ; space
            ; colon_type ty
            ; to_source
            ]
              
    | Constr ty -> (* CR jfuruse: private? *)
        ent [ path
            ; space
            ; colon_type ty
            ; to_source
            ]
  
    | Exception ty ->
        ent [ path
            ; space
            ; colon_type ty
            ; to_source
            ]
  
    | Field ty ->
        ent [ path
            ; space
            ; colon_type ty
            ; to_source
            ]
        
    | Method (_private_flag, _virtual_flag, ty) ->
        ent [ path
            ; space
            ; colon_type ty
            ; to_source
            ]
    | ModType ->
        ent [ path
            ; to_source
            ]
    | Module ->
        ent [ path
            ; to_source
            ]
        
    | Type (params, Some ty, _k) ->
        ent [ type_params params
            ; path
            ; space
            ; equal_type ty
            ; to_source
            ]
  
    | Type (params, None, _k) ->
        ent [ type_params params
            ; path
            ; space
            ; to_source
            ]
  
    | Value ty ->
        ent [ path
            ; space
            ; colon_type ty
            ; to_source
            ]
  
    | Package _ ->
        ent [ path
            ]
(*
          ; div_class "info" [ !$ (Option.default (OCamlFind.Package.find_var "description" pack) (fun _ -> "no description")) ]
*)
  in
  div_class "item" [ main
                   ; odoc i.doc
                   ]

let entry_group_body_per_package (ps, iis) =

  div_class "group_packages" 
    [ packages ps
    ; div_class "items" & List.map entry_group_body_elem iis
    ]

type is = (Item.t (** short look *) * int (** look_length *)
           * (OCamlFind.Packages.t * (int * Item.t) list) list) 

let entry_group dist (is : is) =
  let (short_look, _, xs) = is in
  let id = List.(fst & hd & snd & hd xs) in
  let id = !% "group_id_%d" id in
  let ents = List.(sum (map (length *< snd) xs)) in
  [ entry_group_head short_look.path short_look.kind
  ; div_class "count" 
      [ ElMisc.toggle_display id
          [ ElMisc.triangle
          ; !$ " "
          ; !$ (if ents = 1 then "1 entry" else !% "%d entries" ents) 
          ]
      ; !$ " "
      ; span_class "distance" [ !$ !% "distance: %d" dist ]
      ]
  ; div ~a: [ a_id id; a_style "display:none;" ] & List.map entry_group_body_per_package xs 
  ]

let group (dist, ((_, _, xs) as is : is)) =
  assert (xs <> []);
  tr [ td_class "entry" (entry_group dist is)
     ]
