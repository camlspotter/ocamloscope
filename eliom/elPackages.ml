open Spotlib.Spot
open Eliom_content.Html5.F (* provides functions to create HTML nodes *)
open ElMisc

let the_table = 
  let oc_name p = 
    let ocamlfind_name = p.OCamlFind.Package.name in
    match OCamlFind.Package.version p with
    | Some "[distributed with Ocaml]" -> ocamlfind_name
    | Some ver -> ocamlfind_name ^ "." ^ ver
    | None -> ocamlfind_name
  in
  let oc_compare oc1 oc2 = compare oc1.OCamlFind.Package.name oc2.OCamlFind.Package.name in
  let trs = 
    ElLoad.db.Load.PooledDB.ocamlfind_opam_table
    |> List.sort (fun (oc1,op1) (oc2,op2) ->
      match Option.map (fun x -> x.OPAM.name) op1, Option.map (fun x -> x.OPAM.name) op2 with
      | Some "base", Some "base" -> oc_compare oc1 oc2
      | Some "base", _ -> -1
      | _, Some "base" -> 1
      | None, None-> oc_compare oc1 oc2
      | _, None -> -1
      | None, _ -> 1
      | _ -> 
          match compare op1 op2 with
          | 0 -> oc_compare oc1 oc2
          | c -> c)
    |> List.map (fun (ocamlfind, opam) ->
      tr [ td [ ElServices.Package.a 
                  [ !$ (oc_name ocamlfind) ] 
                  ocamlfind.OCamlFind.Package.name ]
         ; td [ match opam with 
                | Some s -> ElPackage.opam_link s
                | None -> !$ "<unknown>"]
         ])
    |> function
        | [] -> [ tr [ td [!$ "OOPS" ]; td [!$ "NO RESULT?!?!"] ] ]
        | trs -> trs
  in
  table ~a: [ a_class [ "packages" ] ]
    ~thead:(thead [ tr [ td [ !$ "OCamlFind" ]
                       ; td [ !$ "OPAM" ]
                       ]
                  ])
    trs
    

let packages = 
  Lwt.return 
  & html 
      ElMisc.oco_head
      & body [ ElMisc.oco_logo
             ; the_table
             ]
  
let () = 
  prerr_endline "Registering /packages service...";
  Eliom_registration.Html5.register
    ~service:ElServices.Packages.service & fun () () -> packages
