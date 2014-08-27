open Spotlib.Spot
open List
open Item

let count items =
  let total = Array.length items in
  let by_pack = Hashtbl.create 101 in
  flip Array.iter items (fun i ->
    let packs = OCamlFind.Packages.to_strings i.packs in
    let packs =
      unique & map (fun s -> 
        try String.sub s 0 (String.index s '.') with _ -> s) packs
    in
    flip iter packs & fun p ->
      incr & Hashtbl.find_or_add (fun _ -> ref 0) by_pack p);
  total, 
  sort (fun e1 e2 -> compare (snd e2) (snd e1)) 
    & map (fun (k,v) -> k, !v) & Hashtbl.to_list by_pack

let report items =
  let total, by_pack = count items in
  let (!%) = Format.printf in
  !% "Items: %d@." total;
  flip iter by_pack & fun (p, n) ->
    !% "  %s: %d@." p n
