open Spotlib.Spot

let opam_build_dir = 
  let prefix = OPAM.get_prefix () |- !!% "OPAM prefix: %s@." in
  prefix ^/ "build" |- !!% "OPAM build dir: %s@."

(* Scans source files under .opam/xxx/build and ocamlc source code dir *)
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

let source_available =
  let items = db.Load.PooledDB.items in
  let f = memoize & fun (p, md5) ->
    Source.find (Filename.basename p) ~digest:md5
  in
  XSpotlib.Base.timed_message (!% "Checking local source availability (%d items)" (Array.length items))
    (fun () -> Array.init (Array.length items)
      (fun j ->
        let i = items.(j) in
        let path = Option.do_;
          l <-- i.Item.loc;
          (p,md5,_l) <-- Loc.id l;
          f (p, Digest.from_hex md5)
          (* CR jfuruse: Loc.id converts md5 to hex then here we make it back.
             Inefficient. *)
        in
        path <> None)) ()
  |- fun a ->
    !!% "Items with local source: %d@."
    & Array.fold_left (fun st b -> if b then st + 1 else st) 0 a
