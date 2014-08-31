(* local source service *)

open Spotlib.Spot
open List

let tbl = Hashtbl.create 1023

let scan ds =
  Unix.Find.find ds ~follow_symlink:true ~f:(fun path ->
    let is_source =
      match Filename.split_extension path#base with
      | _, ".eliom" -> true
      | _, s when String.length s >= 3 && String.take 3 s = ".ml" -> true
      | _ -> false
    in
    if is_source then 
      Hashtbl.add (* not replace *)
        tbl path#base (path#path, ref None)) (* digest is got lazily *)

let find base ~digest =
  let cands = Hashtbl.find_all tbl base in
  flip find_map_opt cands (function
    | (p, ({contents = Some (Some d)} as r)) ->
        if digest = d then begin
          try 
            let d = Digest.file p in
            if digest <> d then raise Exit;
            Some p 
          with
          | _ -> r := Some None; None
        end else None
    | (_, {contents = Some None (* err *) }) -> None
    | (p, r) ->
        try 
          let d = Digest.file p in
          r := Some (Some d);
          if digest = d then Some p else None
        with _ -> r := Some None; None)
  
