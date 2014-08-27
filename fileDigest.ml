(** OCamlFind and OPAM packages have no clear connection between them,
    and therefore we must find out which OCamlFind package is provided
    from which OPAM package.
*)

open Spotlib.Spot

let (!!) = Lazy.force

type tbl = (string, Digest.t Lazy.t * string) Hashtbl.t

let scan ?exclude_dir dirs = 
  let tbl = Hashtbl.create 107 in
  Unix.Find.find dirs ~f:(fun path ->
    begin match exclude_dir with
    | Some f -> if f path#path then Unix.Find.prune ()
    | _ -> ()
    end;
    match Filename.split_extension path#base with
      | _body, (".cmi" | ".cmo" | ".cmx" | ".cmt" | ".cmti" | ".ml" | ".mli" | ".mll" | ".mlp" | ".mly" | ".eliom") ->
          let p = path#path in
          Hashtbl.add tbl path#base (lazy (Digest.file p), path#dir)
      | _ -> ());
  tbl

let find tbl ~base ~digest =
  Hashtbl.find_all tbl base 
  |> List.filter_map (fun (d, dir) -> 
       if !!d = digest then Some dir else None)

let find tbl ~base ~digest =
  !!% "finding build file %s %s@." base (Digest.to_hex digest);
  find tbl ~base ~digest
  |- !!% "  @[<2>found: %s @[%a@]@]@." base Format.(list ",@," string)

let find_by_base tbl base =
  Hashtbl.find_all tbl base |> List.map snd 

