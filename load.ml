open Spotlib.Spot
open List

module Ty = Types
module P = Printtyp
open Cmt_format
open Item
open Ocaml_conv

let remove_cache path =
  if File.Test._e path then
    try Sys.remove path with _ -> failwithf "Failed to remove an obsolete cache file: %s" path

let final_cache_path = Conf.data_dir ^/ "oco.bin"

module OCP = OCamlFind.Package

let get_doc docs path loc desc = 
  match docs with
  | `Error e -> 
      if Conf.show_ocamldoc_message then
        !!% "@[<2>Error: Load.get_doc:@ %a@]"  (Ocaml.format_with OCamlDoc.ocaml_of_error) e;
      `Error ()
  | `Ok docs -> 
      (* CR jfuruse: package_path is already inside path.
         It complicates the process here *)
      let path_name =
        (* CR jfuruse: does it work for binops especially ( * ) ? *)
        Xpath.name & Xpath.remove_package_path path 
      in
    
      (* !!% "OCAMLDOC PATHNAME=%s@." path_name; *)
      (* CR jfuruse: inefficient! *)
      let rec find = function
        | [] -> None
        | (path_name', { Odoc_types.loc_impl; loc_inter }, info, kind) :: xs ->
            let kind = match desc, kind with
              | Item.Class  , `Class
              | Exception _ , `Exception
              | ModType     , `Module_type
              | Module      , `Module
              | Type _      , `Type
              | Value _     , `Value -> true
              | _ -> false
            in
            if
              kind
              && (
                (* Locations rarely agree *)
                (Some loc = loc_impl || Some loc = loc_inter)
                || path_name = path_name'
              )
            then Some info
            else find xs
      in
      `Ok (find docs)

let rec load root find_packages path =
  Extract.reset_envs ();

  let cmt = match Cmt_format.read path with
    | _, None -> assert false
    | _, Some cmt -> cmt
  in

  (* cmt.cmt_sourcefile is the actual source compiled, but 
     if the real source is .mll, then cmt_sourcefile is .ml and it is often automatically deleted! *)
  let source_path loc = 
    (* CR jfuruse: Location.none means two things
       1. The compiler does not know the location
       2. It is package info
       We cannot distinguish these two currently.
    *)
    if loc = Location.none then None
    else begin
      let f1 = loc.Location.loc_start.Lexing.pos_fname in
      let f2 = loc.Location.loc_end.Lexing.pos_fname in
      assert (f1 == f2);
      Some (cmt.cmt_builddir ^/ f1, loc)
    end
  in

  (* Workaround of include path
     This is not perfect at all. But giving the dir of cmti 
     to the loading path seems to be fair.

     CR jfuruse:
     [cmt.cmt_loadpath] contains the build directory, but in relative.
     We need to make them absolute.
  *)
  let cmt_loadpath = map (fun s -> cmt.cmt_builddir ^/ s) cmt.cmt_loadpath in
  Config.load_path := Filename.dirname path :: cmt_loadpath;

  let root = Path.Pdot (root, cmt.cmt_modname, 0) in

  let find_package = memoize & fun modname ->
    match Cmfile.find_cmi_file modname with
    | None -> 
        !!% "%a: No cmi file found for %s@." Printtyp.path root modname;
        None
    | Some s ->
        match find_packages ~file_path:s with
        | (Some _ as res) -> res
        | None -> 
            !!% "%a: no OCamlFind package found for %s@." Printtyp.path root s;
            None
  in 

  (* CR jfuruse: If Result.t is found in {spotlib},  currently we make {spotlib}.Result.t
     but actually it should be {spotlib}.Spot.Result.t. The linked modules must have higher priority.
  *)
  let pathconv = Pathfix.fix_path find_package in

  let add_ocamldoc = get_doc (OCamlDoc.docs_of_cmt cmt) in

  (* CR jfuruse: Fusion by hand required: map f & map g & map h xs => map (f . g . h) xs *)
  match cmt.cmt_annots with
  | Implementation str -> 
      let items = Extract.structure root str in
      { Extract.path= root; loc= Location.none (* This should be the file itself *); env=[]; kind= Module } :: items
      |> map (fun { Extract.path; loc; env; kind } -> 
        let kind = Pathfix.convert_kind (pathconv env) kind in
        Spath.of_path path, source_path loc, kind, add_ocamldoc path loc kind)

 |  Interface sg -> 
      let items = Extract.signature root sg in  
      { Extract.path= root; loc= Location.none (* This should be the file itself *); env=[]; kind= Module } :: items
      |> map (fun { Extract.path; loc; env; kind } -> 
        let kind = Pathfix.convert_kind (pathconv env) kind in
        Spath.of_path path, source_path loc, kind, add_ocamldoc path loc kind)

  | Packed (sg, paths) ->
      (* sg and paths must be coupled! *)

      (* We try to flush out unused signature info by GC *)
      let sg_ids = map (function
        | Ty.Sig_module (id, _, _) -> id
        | _ -> assert false) sg
      in

      combine sg_ids paths
      |> concat_map (fun (id, path) ->
        let path_no_ext = Filename.chop_extension path in
        let modname = String.capitalize & Filename.basename path_no_ext in
        assert (Ident.name id = modname);
        let cmti = cmt.cmt_builddir ^/ path_no_ext ^ ".cmti" in
        let cmt  = cmt.cmt_builddir ^/ path_no_ext ^ ".cmt" in
        match
          if File.Test._f cmti then Some cmti 
          else if File.Test._f cmt then Some cmt
          else None
        with
        | None ->
            !!% "Warning: %s: Submodule: either %s or %s is not found@."
              (Xpath.name root)
              cmti cmt;
            []
        | Some path ->
            load root find_packages path)

   | _ -> assert false

let load_predefined () : Item.t list =
  let items = Extract.get_predefined () in
  map (fun { Extract.path; kind } -> 
    let kind = Pathfix.convert_kind (fun p ->
      match p with
      | Pdot (_, s, _) -> Spath.(dot predef s)
      | Pident id -> Spath.(dot predef (Ident.name id))
      | _ -> assert false) kind 
    in
    let build_ocamldoc k = Odoc_info.(
      { i_desc = Some [Raw ("Predefined " ^ name_of_kind k)];
        i_authors = [];
        i_version = None;
        i_sees = [];
        i_since = None;
        i_before = [];
        i_deprecated = None;
        i_params = [];
        i_raised_exceptions = [];
        i_return_value = None;
        i_custom = []
      })
    in
    (* name_of_kind k *)
    { packs = OCamlFind.Packages.of_strings ["stdin"]; (* CR jfuruse: not quite stdin *)
      path = Spath.of_path path;
      loc = None;
      doc = `Ok (Some (build_ocamldoc kind));
      kind; }) items

let load packages find_packages path =
  let packs = OCamlFind.Packages.of_strings & map (fun x -> x.OCP.name) packages in
  let root = Spath.package_path & OCamlFind.Packages.to_id packs in
  let add_packages (path, loc, kind, doc) = 
    { packs; path; loc; kind; doc; } 
  in
  try 
    !!% "Loading %s (%s)...@." path (Xpath.name root);
    let items = load root find_packages path in
    !!% "Loaded %d items@." (* path *) (length items);
    map add_packages items
  with
  | (Env.Error e as exn) -> !!% "Env.Error: %a@." Env.report_error e; raise exn
  
let load packages find_packages path =

  Extract.reset_envs ();

  load packages find_packages path |- fun _ -> Extract.reset_envs ()

type dump_file = {
  top_package : OCP.t;
  packages : OCamlFind.Packages.t;
  opam : OPAM.package option;
  items : Item.t list
} with conv(ocaml_of)

module Make(A : sig end) = struct
  module O = OPAM.Make(struct end)

  let guess_build_dir mpath =
    let exts = [".cmti"; ".cmt"; ".cmi"; ".cmo"; ".cmx"] in (* no source files since they can be copied *)
    let paths = filter_map (fun ext -> Module_path.file ext mpath) exts in
    concat_map (fun path ->
      let base = Filename.basename path in
      let digest = Digest.file path in
      O.guess_build_dir ~base ~digest) paths
    |> unique

  (* Load one *.cmt/cmti file *)
  let load_module find_packages ps mpath =
    let open Option in
    let path = 
      let open Module_path in
      file ".cmti" mpath
      >>=! fun () -> file ".cmt" mpath
      >>=! fun () ->
        let modname = modname mpath in
        let build_dirs = guess_build_dir mpath in
        let rec f = function
          | [] -> None
          | d::ds ->
              let mpath = of_string (d ^/ modname) in
              file ".cmti" mpath 
              >>=! fun () -> file ".cmt" mpath
              >>=! fun () -> f ds
        in
        f build_dirs
    in
    match path with
    | Some path -> load ps find_packages path
    | None -> !!% "No cmti/cmt file found for %s@." (Module_path.to_string mpath); []

  (* no memoization *)
  let normalize_opam_source path =
    match OPAM.package_dir_of path with
    | Some (_sw, pack, path) ->
        `OPAM (fold_left (^/) pack path)
    | None ->
        (* For code from non OPAM, we cannot provide the source *)
        `None 

  (* cmt file may not record its dir as exactly the same path name as oco -c <dir> *)
  let normalize_ocamlc_source () =
    let find = OCamlc.find_ocaml_source_root () in
    fun path ->
      match find (Filename.dirname path) with
      | Some (_root, paths) ->
          `OCamlc (fold_left1 (fun st x -> st ^/ x) paths ^/ Filename.basename path)
      | None ->
          `None
    
  let dump_package_group ~reset package_modules_list mpath_packages_tbl find_packages top_package ps =

      let pname = OCP.name top_package in

      (* pa_include of eliom creates location with fname
         like ".../sigs/eliom_reg_simpl.mli 809344"
      *)
      let path_fix f =
        if File.Test._e f then Some f
        else 
          match try Some (String.rindex f ' ') with _ -> None with
          | None -> None
          | Some n ->
              let f = String.sub f 0 n in
              if File.Test._e f then Some f
              else None
      in

      let normalize_source = 
        let normalize =
          if OCamlFind.Package.is_distributed_with_ocaml top_package then
            normalize_ocamlc_source ()
          else
            normalize_opam_source
        in
        memoize & fun path ->
          let d = Option.map Digest.file (path_fix path) in
          d, normalize path
      in

      let normalize_location (path0, loc) =
        try
          (* CR jfuruse: path is only useful for debugging *)
          let d, pack = normalize_source path0 in
          Loc.create d pack loc
        with
        | (Sys_error _ as e) -> 
            !!% "normalize_location Strange! %S@." path0;
            raise e
      in

      let path = Conf.data_dir ^/ "oco_" ^ pname ^ ".bin" in
      if reset then remove_cache path;

      (* Util.with_ocamled_cache ~encoder: ocaml_of_dump_file ~decoder: dump_file_of_ocaml path & fun () -> *)
      Util.with_marshaled_cache path & fun () ->

	remove_cache final_cache_path;
  
        (* OPAM package which provides the top package *)
        let opam =
          let mpaths = 
            unique & concat_map (fun (p, mods) ->
              let pn = OCP.name p in 
              if exists (fun p' -> OCP.name p' = pn) ps then
                (* CR jfuruse: ml_path is ignored! *)
                map (fun (pkg, _ml_path, _) -> pkg) mods.OCamlFind.reachable_tops
              else []) package_modules_list 
          in
          let base_digests = filter_map (fun mpath -> 
            let path = Module_path.file ".cmi" mpath in
            Option.bind path (fun path ->
              try
                Some (Filename.basename path,
                      lazy (Digest.file path))
              with
              | _ -> 
                  !!% "Digest failed: %s@." path;
                  None)) mpaths
          in
          !!% "@[<2>OPAM scan %s:@ @[%a@]@]@."
            (OCP.name top_package)
            Format.(list "@ " string)
            (map fst base_digests);
          if base_digests = [] then begin
            !!% "WARNING: OCamlFind package %s has no base_digests therefore cannot be gussed its OPAM package@." (OCP.name top_package);
          end;
          (match O.guess_package top_package base_digests with
          | `Found name -> Some name
          | `Maybe (name, _) -> Some name
          | `Base b -> Some b
          | `NotFound | `Ambiguous -> None)
            |- fun res -> 
              !!% "OCamlFind package %s is provided by OPAM package %a@."
                (OCP.name top_package)
                Format.(option (fun ppf p -> string ppf p.OPAM.name)) res
(* CR jfuruse: this is too fragile for deriving-ocsigen

finding build file deriving_Show.cmi eee296967ce86703f0514ab2afd23c9e
  found: deriving_Show.cmi /.../.opam/system/build/deriving-ocsigen.0.5/lib,
                           /.../.opam/system/build/deriving.0.6.2/_build/lib
OCamlFind package deriving-ocsigen is provided by OPAM package None
*)

        in
  
        (* Packages are items *)
        let items_package = 
          flip filter_map package_modules_list & fun (p, { OCamlFind.targets = mpath_digest_list }) -> 
            if not & mem p ps then None
            else
              let packs = OCamlFind.Packages.of_strings [p.OCP.name] in
              let path = 
                (* CR jfuruse: we need a tool function for this *) 
                Spath.of_path 
                & Spath.package_path 
                & OCamlFind.Packages.to_id packs
              in
              let mpaths = 
                map (String.capitalize ** Filename.basename ** Module_path.to_string ** fst) mpath_digest_list
              in
  
              Some { packs;
		     path;
		     loc = None;
		     doc = `Ok None;
		     kind = Package (p, mpaths);
                   }
        in
  
        (* Scan things inside modules *)
        let items_contents =
          let is = ref [] in
          flip Hashtbl.iter mpath_packages_tbl & (fun mpath ps ->
            match unique & map OCP.top_name ps with
            | [] -> assert false
            | (_::_::_ as ns) ->
                !!% "ERROR: more than one OCamlFind top package names found: %s"
                  & String.concat " " ns;
                assert false
            | [n] ->
                if n <> pname then ()
                else is := load_module find_packages ps mpath @ !is);
          !is
        in
        
        let items_of_pname = items_package @ items_contents in
  
        let loc_fix item = 
          let loc = Option.map normalize_location item.Item.loc in
          { item with Item.loc = loc }
        in

        (* We need to carry the full details of packages *)
        (* CR jfuruse: we need to define a tool function *)
        let ps' = OCamlFind.Packages.of_strings 
          & map (fun p -> p.OCamlFind.Package.name) ps 
        in
        { top_package; packages= ps'; opam; items= map loc_fix items_of_pname }

  let prepare () =
    let ocamlfind = OCamlFind.init () in
    let packages = OCamlFind.get_packages ocamlfind in
  
    let stdlib_dir = OCamlFind.get_stdlib_dir ocamlfind in
  
    let package_modules_list = packages |> map (fun p ->
      p, OCamlFind.get_modules (Lazy.force O.all_build_table) ~stdlib_dir p)
    in
          
    let find_packages = OCamlFind.find_packages package_modules_list in
  
    (* Which packages a module path belongs to? *)
    (* This is used as the target list of module scanning, so reachable tops are not appropriate. *)
    let mpath_packages_tbl : (Module_path.t, OCP.t list) Hashtbl.t = 
      let tbl = Hashtbl.create 107 in
      package_modules_list |> iter (fun (p, { OCamlFind.targets }) ->
        targets |> iter (fun (mpath, _) ->
          Hashtbl.alter tbl mpath (function
            | None -> Some [p]
            | Some ps -> Some (p::ps))));
      tbl
    in
  
    let group = OCP.group packages in
    
    packages, group, package_modules_list, find_packages, mpath_packages_tbl
  
  let dump_items () =
    let _packages, group, package_modules_list, find_packages, mpath_packages_tbl =
      prepare ()
    in
    
    let (), secs = flip Unix.timed () & fun () -> 
  
      let to_be_dumped x =
	match Conf.args with
	| [] -> `IfNotExists
	| xs -> if mem x xs then `Yes else `No
      in

      group |> Hashtbl.iter (fun pname ps -> 

	match to_be_dumped pname with
	| `No -> ()
	| (`Yes | `IfNotExists) as x -> 
            let top_package = find (fun p -> OCP.name p = pname) ps in
    
            let _ : dump_file = 
              dump_package_group 
		~reset:(x = `Yes)
		package_modules_list 
		mpath_packages_tbl 
		find_packages 
		top_package 
		ps
	    in
	    ())
    in
  
    !!% "dumped in %f secs@." secs
  
end

let dump_items () =
  let module D = Make(struct end) in
  D.dump_items ()

type db = {
  items : Item.t array;
  ocamlfind_opam_table : (OCP.t * OPAM.package option) list;
  (** List of the top OCamlFind packages
      and the OPAM package which installed it if exists.

      Note: it lists only the top packages.
  *)
}

(* Load a data file *)
let load_dumped_package_group path : dump_file =
  if Conf.show_cache_loading then !!% "Loading %s...@." path;
  try
(*
    begin match Ocaml.load_with_exn dump_file_of_ocaml path with
    | [x] -> x
    | _ -> failwithf "load_dumped_package_group: %s has more than one element" path
    end
*)
  with_ic (open_in_bin path) & fun ic -> input_value ic
  |- fun { top_package=pack; opam=opamopt; items } -> 
        if Conf.show_cache_loading then begin
          !!% "%s %a %d items@."
            (* path *)
            pack.OCP.name
            (Format.option OPAM.format_package) opamopt 
            (length items)
        end
  with
  | Ocaml.Load_error e as exn -> 
      !!% "@[<2>load error: %s: @ @[%a@]@]@." 
	path
	Ocaml.format_load_error e;
      raise exn

(* Load the data files already dumped *)
let load_dumped_items () =
  let items, ocamlfind_opam_table = 
    let items = ref & load_predefined () in
    let ocamlfind_opam_table = ref [] in
    Unix.Find.find ~follow_symlink:true [Conf.data_dir] ~f:(fun p ->
      p#base 
      |! <:m<^oco_.*\.bin$>> ->
          let { top_package; opam; items=items_of_pname } = load_dumped_package_group p#path in
          ocamlfind_opam_table := (top_package, opam) :: !ocamlfind_opam_table; 
          items := items_of_pname @ !items
      | _ -> ());
    !items, !ocamlfind_opam_table
  in
  !!% "%d entries loaded@." (length items);

  (* debug *)
  ocamlfind_opam_table |> iter (fun (p, opam_opt) ->
    !!% "%s : %a@." 
      p.OCP.name
      (Format.option OPAM.format_package) opam_opt);

  { items = Array.of_list items;
    ocamlfind_opam_table = ocamlfind_opam_table }

let () =
  let open Gc in
  let c = get () in
  set { c with max_overhead = 100 }


let load_items () =
  let res, (stat_before, stat_after) = Gc.with_compacts load_dumped_items () in
  !!% "DB words: %.2fMb@." 
    (float (stat_after.Gc.live_words - stat_before.Gc.live_words)
     /. float (1024 * 1024 / (Sys.word_size / 8)));
  res

let hcons res =
  !!% "HashConsing...@.";
  let hcons_things (res : db) = flip Unix.timed () & fun () -> 
    let res = { res with items = Array.map Item.rec_hcons res.items } in
    Hashcons.report ();
    Hashcons.clear_all_tables ();
    res
  in
  Gc.print_stat stderr; flush stderr;
  let (res, secs), (words_before, words_after) = XSpotlib.Gc.with_big_compacts hcons_things res in
  Gc.print_stat stderr; flush stderr;
  !!% "HashConsing done in %f secs@." secs;
  !!% "Hcons words: %.2fMb@." 
    (float (words_after - words_before)
     /. float (1024 * 1024 / (Sys.word_size / 8)));
  res

let load_items () =
  if File.Test._f final_cache_path then begin

    if Conf.show_cache_loading then !!% "Loading %s...@." final_cache_path;
    with_ic (open_in_bin final_cache_path) input_value
    |- fun ( { items; _ } : db ) -> 
      !!% "%s : %d entries loaded@."
	final_cache_path
        (Array.length items)

  end else begin

    !!% "No %s found. Building...@." final_cache_path;
    load_items () 
    |> hcons
    |- fun res ->
      with_oc (open_out_bin final_cache_path) (fun oc -> 
        Marshal.(to_channel oc res [Compat_32]))

  end

let load_items () =
  load_items () 
  |- fun _ ->
    let words = XSpotlib.Gc.used_words () in
    !!% "words: %d@." words;
    Gc.print_stat stderr; flush stderr

    

