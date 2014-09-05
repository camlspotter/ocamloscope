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
  div_class "examples"
    [ h2 [ !$ "Examples" ]
    ; ul ~a: [ a_class [ "examples" ] ]
      [ li [ example "'a list -> 'b list" ]
      ; li [ example "string list -> string" ]
      ; li [ example "int -> int -> int" ]
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

let packages = a ~service:ElServices.Packages.service [ !$ "Packages" ]

let index = Lwt.return 
  & html oco_head
  & body & [ oco_logo
           ; oco_form None
           ; div_class "center400" 
               [ examples
               ; p_class "help" [ ElServices.Packages.a [ !$ "Packages" ] ()
                                ; !$ " "; issues ] 
               ]
           ]

let queries qs = !$ ("Queries: " ^ (String.concat ", " & List.map Query.Query.to_string qs))

let status groups results search_time group_time cache_hit_status qs = 
  let id = "extra_status" in
  div_class "status" [ div_class "counts" [ ElMisc.toggle_display id 
                                              [ !$ (!% "%d groups of %d results" groups results)
                                              ; !$ " "
                                              ; ElMisc.triangle 
                                              ]
                                          ]
                     ; div ~a: [ a_id id; a_style "display:none;" ]
                         [ div [ !$ !% "%0.4f secs (%0.4f secs for search, %0.4f secs for grouping)" (search_time +. group_time) search_time group_time ]
                         ; div 
                             [ !$ (match cache_hit_status with
                                 | `Ok true -> "cache hit"
                                 | `Ok false -> "no cache hit"
                                 | `Error `Checksum_failure -> "cache cleared due to invariant check failure"
                                 | `Error `Shrink_too_many -> "too many shrink")
                             ]
                         ; div [ queries qs ]
                         ]
                     ]

type groups = 
    (int (** dist *) 
     * (Item.t (** short look *) * int (** look_length *)
        * (OCamlFind.Packages.t * (int * Item.t) list) list)
    ) list

let found f qs q search_time group_time cache_hit_status gs page ~size =
  status (List.length gs) size search_time group_time cache_hit_status qs
  :: ElPager.pager 
         ~item:f
         ~here:(fun n -> div_class "here" [ !$ (string_of_int n) ])
         ~next:(fun n -> hlink_query ~pos:n q [ div_class "next" [ !$ "Next" ] ])
         ~prev:(fun n -> hlink_query ~pos:n q [ div_class "prev" [ !$ "Prev" ] ])
         ~goto:(fun n -> hlink_query ~pos:n q [ div_class "goto" [ !$ (string_of_int n) ] ])
         List.(map (fun g -> g, 1) gs)
         page

let notfound qs = 
  [ p_class "status" [ !$ "No match" ]
  ; p [ queries qs ]
  ]

let funny = 
  [ p_class "status" [ !$ "You made a Funny query" ]
  ]

let error = 
  [ p_class "status" [ !$ "Invalid query" ] 
  (* ; examples  *)
  ]

let query q nopt = 
  match CachedQuery.search ElLoad.db 0 q with
  | `EmptyQuery, _ -> index
  | res ->
      Lwt.return 
      & html 
        oco_head
      & body & div_class "search" [ oco_logo
                                  ; oco_form (Some q) ]
             :: (match res with
                 | `EmptyQuery, _ -> assert false
                 | `Ok (qs, [],_,_,_), _ -> notfound qs
                 | `Error, _ -> error
                 | `Funny, _ -> funny
                 | `Ok (qs, groups, search_time, result_time, size), cache_hit_status -> 
                     found ElItem.group qs q search_time result_time cache_hit_status groups (Option.default nopt & fun () -> 1) ~size )

let () = 
  prerr_endline "Registering search service...";
  Eliom_registration.Html5.register
    ~service & fun argopt () ->
      match argopt with
      | None -> index
      | Some (q,nopt) -> query q nopt (* q *)
