open Spotlib.Spot
open List
open Ppx_orakuda.Regexp.Infix

(** I should use OPAM library but I still feel it is not stable enough. 
    So far, I use OPAM cli. 
*)

(* Guess OPAM package name from a path name.
   Your OPAM prefix must contain ".opam" directory component.
*)

let get_prefix () =
  let prefix =
    match Unix.Command.(
      shell "opam config var prefix" 
      |> get_stdout 
      |> must_exit_with 0
    ) with
    | [line] -> String.chop_eols line
    | _ -> assert false
  in
    
  let rec test = function
    | "." | "/" -> !!% "Your OPAM prefix does not contain .opam@."; assert false
    | path -> 
        if Filename.basename path = ".opam" then ()
        else test & Filename.dirname path
  in
  test prefix;
  prefix

let get_current_switch () =
  let open Unix.Command in
  shell "opam switch"
  |> get_stdout
  |> must_exit_with 0
  |> filter_map (fun s ->
    Option.map (fun x -> x#_1) (s =~ {m|^([^\s]+)\s+C\s+|m}))
  |> function
      | [sw] -> sw
      | [] -> assert false
      | _ -> assert false

type package = {
  name : string;
  version : string;
  desc : string;
  base : bool
} [@@deriving conv{ocaml}]

let name_of_package p = if p.base then p.name else p.name ^ "." ^ p.version
let format_package ppf p = 
  if p.base then Format.string ppf p.name else Format.fprintf ppf "%s.%s" p.name p.version

let get_installed () =
  Unix.Command.(
    shell "opam list -i" 
    |> get_stdout
    |> must_exit_with 0
  ) |> filter_map (fun line ->
    let line = String.chop_eols line in
    match () with
    | _ when [%p? Some _] <-- (line =~ {m|^Installed packages|m}) ->
        (* The first line. CR jfuruse: very fragile against OPAM change *)
        None
    | _ when Some x <-- (line =~ {m|^([^\s]+)\s+([^\s]+)\s+(.*)$|m}) ->
        !!% "%s %s@." x#_1 x#_2;
        let base = x#_2 = "base" in
        Some { name = x#_1; version = x#_2; desc = x#_3; base }
    | _ -> 
        !!% "opam list -i returned a strange line: %s@." line;
        None)

let split_dir path =        
  let rec split_dir ds = function
    | ("." | "/" as s) -> s::ds
    | path -> split_dir (Filename.basename path::ds) (Filename.dirname path) 
  in
  split_dir [] path

let package_dir_of path =
  let rev_ds = rev & split_dir path in
  let rec search st = function
    | blah :: "build" :: switch :: ".opam" :: _ -> Some (switch, blah, st)
    | x :: xs -> search (x :: st) xs
    | [] -> None
  in
  search [] rev_ds

(* We fix the OPAM root. *)

module Make(A : sig end) = struct

  let current_switch = get_current_switch ()

  let opam_build_dir = 
    let prefix = get_prefix () |- !!% "OPAM prefix: %s@." in
    prefix ^/ "build" |- !!% "OPAM build dir: %s@."

  let installed = get_installed () |- !!% "OPAM packages: %d found@." ** length

  let find_installed name = find (fun p -> p.name = name) installed

  let package_of path =
    let open Option in
    package_dir_of path
    >>= fun (sw, name, _) ->
        if sw <> current_switch then None
        else 
          match String.split1 (function '.' -> true | _ -> false) name with
          | None -> None
          | Some (name, ver) ->
              try 
                let p = find_installed name in
                if p.version <> ver then None
                else Some p
              with Not_found -> None
  
  
  let package_build_dir p = match p.version with
    | "base" -> `Base
    | _ -> `Found (opam_build_dir ^/ name_of_package p)
  
  let get_built_table p = 
    match package_build_dir p with
    | `Base -> `Base
    | `Found d -> `Found (FileDigest.scan [d] |- fun tbl -> !!% "%s %d built files@." p.name (Hashtbl.length tbl))
  
  let build_tables = map (fun p -> p, get_built_table p) installed
  
  let all_build_table = 
    lazy (
    let tbl = Hashtbl.create 107 in
    let ocamlc_build_tables = 
      assert (Conf.compiler_source_dir <> "");
      [ `Found (FileDigest.scan [Conf.compiler_source_dir]) ]
    in
    iter (function
      | `Base -> ()
      | `Found tbl' ->
          Hashtbl.iter (Hashtbl.add tbl) tbl') 
      (ocamlc_build_tables @ map snd build_tables);
    tbl |- fun _ -> !!% "OPAM all build table: %d entries@." (Hashtbl.length tbl)
    )
  
  let base = { name = "<base>";
               version = OCamlc.get_version ();
               desc = "base packages installed with the OCaml compiler";
               base = true; }
  
  let get_base_package oc_pkg = 
    match oc_pkg.OCamlFind.Package.name, OCamlFind.Package.is_distributed_with_ocaml oc_pkg with
    | "stdlib",   true -> Some base
    | "bigarray", true -> Some (find_installed "base-bigarray")
    | "threads",  true -> Some (find_installed "base-threads")
    | "unix",     true -> Some (find_installed "base-unix")
    | _,          true -> Some base
    | _ -> None
  
  let guess_build_dir ~base ~digest = 
    FileDigest.find (Lazy.force all_build_table) ~base ~digest
  
  let (!!) = Lazy.force
  
  (* CR jfuruse:
  
     BUG: Variable opam_pack points to directory name not package name,
     so if more than one built directories are found, guess_packages
     concludes ambigous.
  
     This should be moved to OPAM
  *)
  let guess_package ~installed =
    ~~ scani_left `NotFound installed ~f:(fun _ state (base, zdigest) ->
      match state with
      | `Found _ | `Ambiguous -> assert false (* it must already stop *)
      | `Maybe (opam_pack, n) -> 
          let digest = !!zdigest in
          begin match 
              guess_build_dir ~base ~digest 
              |> filter_map package_of
              |> unique
            with
          | [] -> `Continue state
          | [opam_pack'] -> 
              if opam_pack = opam_pack' then begin
                !!% "PACK FOUND +1 %s@." opam_pack.name;
                if n >= 4 then `Stop (`Found opam_pack)
                else `Continue (`Maybe (opam_pack, n+1));
              end else begin
                !!% "PACK AMBIGUOUS@.";
                `Stop `Ambiguous
              end
          | _ -> 
              !!% "PACK AMBIGUOUS@.";
              `Stop `Ambiguous
          end
      | `NotFound ->
          let digest = !!zdigest in
          begin match guess_build_dir ~base ~digest with
          | [] -> `Continue `NotFound
          | [dir] -> 
              let opam_pack = package_of dir |> from_Some in
              !!% "PACK FOUND %s@." opam_pack.name;
              `Continue (`Maybe (opam_pack, 1))
          | _ -> `Stop `Ambiguous
          end)
  
  let guess_package oc_pkg installed =
    match get_base_package oc_pkg with
    | Some n -> `Base n
    | None -> 
        (* CR jfuruse: This is stupid but due to the type of scani_left *)
        match guess_package ~installed with
        | `Ambiguous    -> `Ambiguous
        | `Found s      -> `Found s
        | `Maybe (s, n) -> `Maybe (s, n)
        | `NotFound     -> `NotFound
  
end
