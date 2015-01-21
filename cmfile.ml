open Spotlib.Spot
open List

open Config
open Cmo_format
open Cmx_format
open Cmi_format

(* CM* signature digest *)
module CMIDigest : sig
  type t [@@deriving conv{ocaml}]
  val to_string : t -> string
  val of_cu	: compilation_unit -> t
  val of_ui	: unit_infos -> t
  val of_cmi	: cmi_infos -> t
end = struct

  type t = string

  open Ocaml_conv
  let ocaml_of_t = ocaml_of_string *< Digest.to_hex
  let t_of_ocaml ?(trace=[]) o = 
    Result.(bind (string_of_ocaml ~trace o) & fun s ->
      try return & Digest.from_hex s with exn -> `Error (`Exception exn, o, trace)
    )
  let t_of_ocaml_exn = Ocaml_conv.exn t_of_ocaml
      
  let to_string x = Digest.to_hex x

  let of_cu  cu  = try from_Some & assoc cu.cu_name   cu.cu_imports     with _ -> assert false
  let of_ui  ui  = try from_Some & assoc ui.ui_name   ui.ui_imports_cmi with _ -> assert false
  let of_cmi cmi = try from_Some & assoc cmi.cmi_name cmi.cmi_crcs      with _ -> assert false
end

(* Load a CM* file. *)
let load filename =
  with_ic (open_in_bin filename) & fun ic ->
    let len_magic_number = String.length cmo_magic_number in
    let magic_number = really_input_string ic len_magic_number in
  
    if magic_number = cmo_magic_number then begin
      let cu_pos = input_binary_int ic in
      seek_in ic cu_pos;
      let cu = (input_value ic : compilation_unit) in
      Some (`CMO cu)
    end 
    else if magic_number = cma_magic_number then begin
      let toc_pos = input_binary_int ic in
      seek_in ic toc_pos;
      let toc = (input_value ic : library) in
      Some (`CMA toc)
    end else if magic_number = cmi_magic_number then begin
      let cmi = Cmi_format.input_cmi ic in
      Some (`CMI cmi)
    end else if magic_number = cmx_magic_number then begin
      let ui = (input_value ic : unit_infos) in
      Some (`CMX ui)
    end else if magic_number = cmxa_magic_number then begin
      let li = (input_value ic : library_infos) in
      Some (`CMXA li)
    end else begin
      let pos_trailer = in_channel_length ic - len_magic_number in
      let _ = seek_in ic pos_trailer in
      let _ = really_input ic magic_number 0 len_magic_number in
      if magic_number = Config.exec_magic_number then Some `ByteExec
      else if Filename.check_suffix filename ".cmxs" then Some `CMXS
      else None
    end

(* CMI Digest of module path *)
let cmi_md5 mpath =
  let cmi = Module_path.file ".cmi" mpath in
  match cmi with
  | None -> 
      !!% "WARNING: No cmi file for %s@." (Module_path.to_string mpath);
      None
  | Some cmi ->
      (* CR jfuruse: It is clear we load a cmi. Why we use load? *)
      match load cmi with
      | None -> 
          !!% "WARNING: load failure of cmi file %s@." cmi;
          None
      | Some (`CMI x) -> Some (cmi, CMIDigest.of_cmi x)
      | _ -> assert false

open Cmt_format

let cmti_or_cmt mpath = 
  let open Option in
  Module_path.file ".cmti" mpath 
  >>=! fun () -> Module_path.file ".cmt" mpath

(** For an mpath with its cmi_digest, findout its corresponding cmti/cmt.
    cmti/cmt need not to be at the same directory of mpath: they can be in OPAM build directory.
*)
let find_cmti_or_cmt opam_build_table mpath cmi_digest =
  match cmti_or_cmt mpath with
  | Some path -> [path] (* if it is found locally, it is done *) 
  | None ->
      (* Not found locally. Let's find in build dir *)
      (* CR jfuruse: this sounds bit template *)
      let modname = Module_path.modname mpath in
      let modnames = [modname; String.uncapitalize modname] in
      let cmi_files =
        modnames
        |> concat_map (fun modname -> 
          let base = modname ^ ".cmi" in
          (* CR jfuruse: using OPAM here? *)
          map (fun d -> d ^/ base) (FileDigest.find_by_base opam_build_table base))
        |> filter (fun path ->
          match load path with
          | Some (`CMI cmi) -> CMIDigest.of_cmi cmi = cmi_digest
          | _ -> false)
      in
      match cmi_files with
      | [] -> 
          !!% "WARNING: No build file in OPAM build dir for %s %s@."
                (CMIDigest.to_string cmi_digest)
                (Module_path.to_string mpath);
          []
      | cmis -> 

          match 
            filter_map (fun cmi ->
              let build_mpath = Module_path.of_path cmi in
              cmti_or_cmt build_mpath) cmis
          with
          | [] ->
              !!% "WARNING: Build file(s) for %s is(are) found but no corresponding cmt/cmti file found@."
                (Module_path.to_string mpath);
              []
          | [cmt] -> [cmt]
          | cmts ->
              !!% "@[<2>Warning: More than one cmt/cmti files for %s are found.@,@[<v>%a@]@]@."
                (Module_path.to_string mpath)
                Format.(list "@," string) cmts;
              cmts

(* CR jfuruse: Ignore non existent cmt file issue *)
let rec get_packed opam_build_table path ml_path =
  let mpath = Module_path.of_path path in 
  match cmi_md5 mpath with
  | None ->
      (* cmi_md5 already prints the warning *)
      (* !!% "WARNING: get_packed: %s: no cmi found@." path; *)
      [mpath, rev ml_path, None]

  | Some (_cmi, digest) ->
      (mpath, rev ml_path, Some digest) ::
      match find_cmti_or_cmt opam_build_table mpath digest with
      | [] -> 
          !!% "WARNING: get_packed: %s: no cmti/cmt found@." path;
          []

      | cmt_paths ->
          flip concat_map cmt_paths & fun cmt_path ->
            let cmt = Cmt_format.read_cmt cmt_path in
            let dir = cmt.cmt_builddir in
            match cmt.cmt_annots with
            | Implementation _ -> []
            | Interface _ -> []
            | Packed (_, paths) -> 
                let paths = map (fun p -> dir ^/ p) paths in
                concat_map (fun path -> 
                  let modname = Module_path.(of_path path |> modname) in 
                  let ml_path = modname :: ml_path in
                  get_packed opam_build_table path ml_path) paths
            | _ -> assert false

let inaccessible_modules = Hashset.create 107
        
let load_link_unit opam_build_table resolver_in_ocamlfind_package filename = (* only for cma cmo cmx cmxa *)
    let mpath = Module_path.of_path filename in
    (* CMA and CMXA have no include path information, so we must find linked module location by CRC.
       And if they are out of the directory specified by META, the module cannot be accessed
       unless the user specifies -I explicitly.
    *)
    let get_mpath_md5s units =
      let find_mpath uname (* unit name like "List" *) ~digest =
        (* Let's find in the package directory first *)
        let mpath = Module_path.of_string & Filename.dirname filename ^/ uname in
        match Module_path.file ".cmi" mpath with
        | Some _ -> Some mpath
        | None -> 
            (* Oops not found. Try package's known cmi file set. *)
            match resolver_in_ocamlfind_package ~modname:uname ~digest with
            | Some mpath -> 
                (* CR jfuruse: we should mark the module as "it requres -I xxx" *) 
                !!% "Warning: load_link_unit: %s found but requires -I dir@." uname;
                Some mpath
            | None -> 
                Hashset.(
                  if mem inaccessible_modules (uname, digest) then begin
                    add inaccessible_modules (uname, digest);
                    !!% "Warning: load_link_unit: %s (%s) does not exist! (linked but inaccessible module)@." uname (CMIDigest.to_string digest);
                  end);
                (* CR jfuruse: we see multilple warnings for one module *)
                None
      in
      filter_map (fun (uname, digest) -> 
        let open Option in
        find_mpath uname ~digest >>= fun mpath -> return (mpath, digest)
      ) units
    in

    (* We cannot use Spath since it introduce circular deps *)
    let spath filename = [ Module_path.(of_path filename |> modname) ] in
    let pmpath mpath = [ Module_path.modname mpath ] in

    match load filename with
    | Some (`CMO cu) -> 
        [mpath, CMIDigest.of_cu cu],
        unique & get_packed opam_build_table filename & spath filename
    | Some (`CMX ui) -> 
        [mpath, CMIDigest.of_ui ui],
        unique & get_packed opam_build_table filename & spath filename
    | Some (`CMA lib) ->
        let mpath_md5s = unique & get_mpath_md5s (map (fun u -> u.Cmo_format.cu_name, CMIDigest.of_cu u) lib.Cmo_format.lib_units) in
        mpath_md5s,
        unique & concat_map (fun (mpath, _digest) -> 
          get_packed opam_build_table (Module_path.file ".cmi" mpath |> from_Some) 
            & pmpath mpath) mpath_md5s
    | Some (`CMXA lib) ->
        let mpath_md5s = unique & get_mpath_md5s (map (fun (u,_digest) -> u.Cmx_format.ui_name, CMIDigest.of_ui u) lib.Cmx_format.lib_units) in
        mpath_md5s,
        unique & concat_map (fun (mpath, _digest) -> 
          get_packed opam_build_table (Module_path.file ".cmi" mpath |> from_Some) 
            & pmpath mpath) mpath_md5s
    | Some `CMXS ->
        [], []
    | _ -> 
        (* Sometimes .o files are listed in archive. *)
        !!% "Warning: %s: strange file type@." filename;
        [], []


let load_archive opam_build_table resolver_in_ocamlfind_package filenames =
    (* filenames come from META's archive(...) values. 
       They may contains strange filenames *)
    let targets_and_tops_list = 
      (* CR jfuruse: this should be memoized by filenames *)
      map (fun filename ->
        try load_link_unit opam_build_table resolver_in_ocamlfind_package filename with Sys_error _ -> [], []) filenames
    in
    let targets = unique & concat_map fst targets_and_tops_list in
    let reachables = unique & concat_map snd targets_and_tops_list in

    (* Check md5 uniqueness *)
    let tbl = Hashtbl.create 107 in
    reachables |> iter (fun (mpath, _ml_path, md5opt) ->
      match Hashtbl.find_opt tbl mpath, md5opt with
      | None, _ -> Hashtbl.add tbl mpath md5opt
      | Some None, Some _ -> Hashtbl.replace tbl mpath md5opt
      | Some (Some md5), Some md5' -> assert (md5 = md5')
      | Some _, None -> ());

    targets, reachables

let cmi_without_value cmi_path =
  let open Cmi_format in
  let cmi_infos = read_cmi cmi_path in
  cmi_infos.cmi_sign |> for_all (function
    | Types.Sig_value _ 
    | Sig_module _
    | Sig_class _ -> false
    | Sig_type _
    | Sig_typext _
    | Sig_modtype _
    | Sig_class_type _ -> true)

let find_cmi_file name =
  try Some (Misc.find_in_path_uncap !Config.load_path (name ^ ".cmi"))
  with Not_found -> None

