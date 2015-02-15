open Spotlib.Spot

let opam_build_dir = 
  let prefix = OPAM.get_prefix () |- !!% "OPAM prefix: %s@." in
  prefix ^/ "build" |- !!% "OPAM build dir: %s@."

let () =
  let open ElConfig in
  match config.ocamlc_source_dir with
  | None -> 
      failwith "You need ocamlc_source_dir"
  | Some d -> 
      !!% "Scanning local source files...@.";
      Source.scan [ d; opam_build_dir ];
      !!% "Scanned local source files.@."

let db = Load.PooledDB.create & Load.load_items ()

