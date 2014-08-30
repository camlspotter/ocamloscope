(* Display info of an OCamlFind package *)

open Spotlib.Spot
open Eliom_content.Html5.F (* provides functions to create HTML nodes *)
open ElMisc
open Item

module OCP = OCamlFind.Package

let service = ElServices.Package.service

let opam_link p = 
  if p.OPAM.base then !$ (OPAM.name_of_package p)
  else
    let service = Eliom_service.external_service
      ~prefix:"http://opam.ocaml.org"
      ~path: [ "pkg"; p.OPAM.name ; !% "%s.%s" p.OPAM.name p.OPAM.version ]
      (* ~keep_nl_params: .. *)
      ~get_params:Eliom_parameter.unit
      ()
    in
    a ~service [ !$ (OPAM.name_of_package p) ] ()

let notfound name = 
  [ p_class "status" [ !$% "%S: no such OCamlFind package in DB" name ]
  ]

let ambiguous name = 
  [ p_class "status" [ !$% "%S: the query is ambiguous" name ]
  ]

let package search_time group_time pack mpaths =
  let opam = 
    let top_ocamlfind_name = <:s<\..*/>> pack.OCP.name in
    ~~ List.find_map_opt ElLoad.ocamlfind_opam_table ~f:(fun (p, opam) ->
      if p.OCP.name = top_ocamlfind_name then Some opam
      else None)
  in
  let find_var k = match OCP.find_var k pack with
    | None -> "none"
    | Some s -> s
  in
  let name = pack.OCP.name in
  let version = find_var "version" in
  opam,
  [ p_class "status" [ !$% "(%0.4f secs) (%0.4f secs for search, %0.4f secs for grouping)" (search_time +. group_time) search_time group_time]

  ; 
    let trs = 
       [ tr [ td_class "package_left" [ !$ "OCamlFind Package:" ]
            ; td [ span_class "package" [ !$ name ]
                 ; !$% " (%s)" version
                 ]
            ]

       ; tr [ td [ !$ "OPAM package:" ]
            ; td [ match opam with 
                   | None -> !$ "<not found>"
                   | Some None -> !$ "<unknown>"
                   | Some (Some p) -> opam_link p ]
            ]
         
       ; tr [ td [ !$ "Description:" ]
            ; td [ !$ (find_var "description") ]
            ]

       ; tr [ td [ !$ "Requires:" ]
            ; td & match OCP.requires pack with
                   | Some [] -> [ !$ "none" ]
                   | None -> [ !$ "none" ] (* CamlIDL has no Requires: and it needs noting. *)
                   | Some packs -> 
                       List.intersperse !$" "
                       & List.map (fun pack ->
                         ElServices.Package.a [ !$ pack ] pack) packs
            ]

       ; tr [ td [ !$ "Modules:" ]
            ; td & List.intersperse (!$ " ")
                     (List.map (fun p -> !$ p) mpaths)
            ]
       ]
    in
    table (List.hd trs) (List.tl trs)

  ]


let query name = 
  let ppath = Spath.nhc_ident name in
  let res = 
    match
      Query.query ElLoad.items (Some [ { Query.Query.kind= Some `ExactPackage;
                                         path= Some ppath;
                                         type_= None;
                                         dist0= true }]) 
    with
    | `EmptyQuery | `Error | `Funny -> notfound name
    | `Ok (_, [_dist, (_, _, [_pkgs, [_id, i]])], search_time, group_time, _size) ->
        begin match i.kind with
        | Item.Package (pack, paths) ->
            (* CRv2 jfuruse: We have many thing to do to have real Eliom! ml => eliom and etc. *)
            let _opam, tree = package search_time group_time pack paths in
(*
            ignore {unit{
(*
              Dom_html.window##alert (Js.string !%"OPAM package name is %s" (match opam with None -> "none" | Some o -> o.OPAM.name))
*)
              () }};
*)
            tree
        | _ -> assert false
        end
    | `Ok (_, [], _, _, _) -> 
        notfound name
    | `Ok (_, xs, _, _, _) -> 
        let is = 
          List.(flip concat_map xs (fun (_, (_, _, ys)) ->
            flip concat_map ys (fun (_, zs) -> map snd zs)))
        in
        !!% "Ambig: %a@."
          Format.(list " " string)
          & List.map (fun i -> Path.name & Spath.to_path i.path) is;
        ambiguous name
  in
  Lwt.return
  & html
    oco_head
  & body & div_class "search" [ oco_logo
                              ; oco_form None ]
           :: res

let () = 
  prerr_endline "Registering /package service...";
  Eliom_registration.Html5.register
    ~service & fun arg () ->
      query arg

