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
  flip find_map_opt cands & function
    | (p, ({contents = Some (Some d)} as r)) when digest = d ->
        begin match Digest.file p with
        | exception _ ->
            r := Some None; None
        | d -> 
            if digest <> d then begin
              r := Some None; None
            end else Some p
        end
    | (_, {contents = Some (Some _)}) (* different digest *) -> None
    | (_, {contents = Some None (* err *) }) -> None
    | (p, r) ->
        match Digest.file p with
        | exception _ ->
            r := Some None; None
        | d -> 
            r := Some (Some d);
            if digest = d then Some p else None

