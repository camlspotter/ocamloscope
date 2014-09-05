(* Eliom search result pager *)

open Spotlib.Spot
open List.Infix
open Eliom_content.Html5.F

let items_per_page = 30
let pager_nav_max_pages = 10

let pager_nav ~total pos =
  assert (total > 0);
  let pos = 
    if pos < 0 then begin Ocsigen_messages.warning "WARNING: ElPager.pager_nav: pos < 0"; 0 end
    else if total < pos then begin Ocsigen_messages.warning "WARNING: ElPager.pager_nav: total < pos"; total end
    else pos
  in
  let first_page = max 1 & pos - pager_nav_max_pages / 2 in
  let last_page = min total & first_page + pager_nav_max_pages - 1 in
  (if first_page > 1 then [`First] else [])
  @ (if pos > 2 then [`Prev] else [])
  @ List.map (fun i -> if i = pos then `Here else `Goto i) (first_page--last_page)
  @ (if pos = total then [] else [`Next])

let splits_by per = 
  let rec aux (rev_cur,rev_st) filled = function
    | [] -> 
        List.rev (
          if rev_cur = [] then rev_st
          else List.rev rev_cur :: rev_st
        )
    | (i,size)::xs ->
        let rev_cur = i :: rev_cur in
        let filled = filled + size in
        if filled > per then aux ([], List.rev rev_cur :: rev_st) 0 xs
        else aux (rev_cur, rev_st) filled xs
  in
  aux ([], []) 0
  

let pager ~item ~next ~prev ~here ~goto item_sizes =
  assert (item_sizes <> []);
  let pages = splits_by items_per_page item_sizes in
  let num_pages = List.length pages in
  fun n ->
    let group = try List.nth pages (n-1) with _ -> List.hd pages in
    let pager_nav = pager_nav ~total:num_pages n in
    let trs = List.map item group in
    [ ul ~a:[ a_class [ "jump" ] ]
        ( List.map (function
            | `First -> li [ goto 1 ]
            | `Here -> li [ here n ]
            | `Next -> li [ next (n+1) ]
            | `Prev -> li [ prev (n-1) ]
            | `Goto i -> li [ goto i ]) pager_nav )

    ; table ~a:[ a_class [ "pager" ] ] (List.hd trs) (List.tl trs)
        (* CR jfuruse: this is the approach for non-empty list, but looks slightly stupid.
           any good tool around?
        *)
    ; ul ~a:[ a_class [ "jump" ] ]
        ( List.map (function
            | `First -> li [ goto 1 ]
            | `Here -> li [ here n ]
            | `Next -> li [ next (n+1) ]
            | `Prev -> li [ prev (n-1) ]
            | `Goto i -> li [ goto i ]) pager_nav )
    ]

      
      
    
    
