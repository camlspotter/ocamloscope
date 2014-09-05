open Spotlib.Spot
open Eliom_content.Html5.F (* provides functions to create HTML nodes *)
open ElMisc

let service = ElServices.Search.service

let hlink_index es =
  a ~service es None

let hlink_query ?pos string es =
  a ~service es
    (Some (string, pos)) (* q=query+p=3 *)

let example string = hlink_query string [ !$ string ]

let examples = 
  [ h2 [ !$ "Examples" ]
  ; ul ~a: [ a_class [ "examples" ] ]
    [ li [ example "'a list -> 'b list" ]
    ; li [ example "string list -> string" ]
    ; li [ example "concat" ]
    ; li [ example "(+)" ]
    ; li [ example "float : _" ]
(*    ; li [ example "* : float" ] *)
(*
    ; li [ example "{stdlib}._" ]
    ; li [ example "{stdlib}.*" ]
*)
    ; li [ example "constr Ok" ]
    ; li [ example "(_) : _" ]
    ]
  ]

let _packages = a ~service:ElServices.Packages.service [ !$ "Packages" ]

let index = Lwt.return 
  & html oco_head
  & body & [ oco_logo
           ; oco_form None
           ]
         @ examples
         @ [ p [ ElServices.Packages.a [ !$ "Packages" ] ()
               ; !$ " "; issues ] ]

let notfound _qs = 
  [ p_class "status" [ !$ "No match" ]
  ]

let funny = 
  [ p_class "status" [ !$ "You made a Funny query" ]
  ]

let error = 
  [ p_class "status" [ !$ "Invalid query" ] 
  (* ; examples  *)
  ]

let query q _nopt = 
      Lwt.return 
      & html 
        oco_head
      & body & div_class "search" [ oco_logo
                                  ; oco_form (Some q) ]
             :: []
  
let () = 
  Eliom_registration.Html5.register
    ~service & fun argopt () ->
      match argopt with
      | None -> index
      | Some (q,nopt) -> query q nopt (* q *)

let packages = 
  Lwt.return 
  & html 
      ElMisc.oco_head
      & body & List.map id [ ElMisc.oco_logo
                           ]

let () = ignore & Ident.create "hello"
  
let () = 
  Eliom_registration.Html5.register
    ~service:ElServices.Packages.service & fun () () -> packages
let () = 
  Eliom_registration.Html5.register
    ~service:ElServices.Package.service & fun _s () -> packages
