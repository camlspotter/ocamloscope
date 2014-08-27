(* option filter *)

open Spotlib.Spot
open List
open Orakuda.Regexp.Infix

type opt =
  | Use0
  | Use1
  | Skip0
  | Skip1
  | AddIncDir of string 

let opts = [
  "-"                , Use1;  
  "--help"           , Use0;  
  "-I"               , Use1;  
  "-S"               , Skip0; 
  "-a"               , Skip0; 
  "-absname"         , Skip0; 
  "-annot"           , Skip0; 
  "-bin-annot"       , Skip0; 
  "-c"               , Skip0; 
  "-cc"              , Skip1; 
  "-cclib"           , Skip1; 
  "-ccopt"           , Skip1; 
  "-compact"         , Skip0; 
  "-config"          , Skip0; 
  "-custom"          , Skip0; 
  "-dalloc"          , Skip0; 
  "-dclambda"        , Skip0; 
  "-dcmm"            , Skip0; 
  "-dcombine"        , Skip0; 
  "-dinstr"          , Skip0; 
  "-dinterf"         , Skip0; 
  "-dlambda"         , Skip0; 
  "-dlinear"         , Skip0; 
  "-dlive"           , Skip0; 
  "-dllib"           , Skip1; 
  "-dllpath"         , Skip1; 
  "-dparsetree"      , Skip0; 
  "-dprefer"         , Skip0; 
  "-drawlambda"      , Skip0; 
  "-dreload"         , Skip0; 
  "-dscheduling"     , Skip0; 
  "-dsel"            , Skip0; 
  "-dspill"          , Skip0; 
  "-dsplit"          , Skip0; 
  "-dstartup"        , Skip0; 
  "-dtypes"          , Skip0; 
  "-ffast-math"      , Skip0; 
  "-for-pack"        , Skip1; 
  "-g"               , Skip0; 
  "-help"            , Use0; 
  "-i"               , Skip0; 
  "-impl"            , Use1; 
  "-inline"          , Skip1; 
  "-intf"            , Use1; 
  "-intf-suffix"     , Skip1; 
  "-intf_suffix"     , Skip1; 
  "-labels"          , Skip0; 
  "-linkall"         , Skip0; 
  "-make-runtime"    , Skip0; 
  "-make_runtime"    , Skip0; 
  "-modern"          , Skip0; 
  "-no-app-funct"    , Skip0; 
  "-noassert"        , Skip0; 
  "-noautolink"      , Skip0; 
  "-nodynlink"       , Skip0; 
  "-nolabels"        , Use0; 
  "-nopervasives"    , Skip0; 
  "-nostdlib"        , Skip0; 
  "-o"               , Skip1; 
  "-output-obj"      , Skip0; 
  "-p"               , Skip0; 
  "-pack"            , Skip0; 
  "-pp"              , Use1; 
  "-principal"       , Skip0; 
  "-rectypes"        , Use0; 
  "-runtime-variant" , Skip1; 
  "-shared"          , Skip0; 
  "-short-paths"     , Skip0;
  "-strict-sequence" , Skip0; 
  "-thread"          , AddIncDir "+threads";
  "-unsafe"          , Skip0; 
  "-use-prims"       , Skip1; 
  "-use-runtime"     , Skip1; 
  "-use_runtime"     , Skip1; 
  "-v"               , Skip0; 
  "-verbose"         , Skip0; 
  "-version"         , Skip0; 
  "-vmthread"        , AddIncDir "+vmthreads";
  "-vnum"            , Use0; 
  "-w"               , Skip1; 
  "-warn-error"      , Skip1; 
  "-warn-help"       , Skip0; 
  "-where"           , Skip0; 
]

let load_dump path =
  let modules = 
    with_ic (open_in_bin path) & fun ic ->
      Odoc_types.open_dump (input_value ic : Odoc_module.t_module list Odoc_types.dump)
  in
  modules

(* CR jfuruse: Packaged module *)
let run_doc cmt =
  let rec filter_opts = function
    | [] -> []
    | x :: xs when mem (x, Skip0) opts -> filter_opts xs
    | x :: xs when mem (x, Use0)  opts -> x :: filter_opts xs
    | x :: _x' :: xs when mem (x, Skip1) opts -> filter_opts xs
    | x ::  x' :: xs when mem (x, Use1) opts -> x :: x' :: filter_opts xs
    | "" :: _ -> assert false
    | x :: xs when x.[0] = '-' ->
        begin match assoc_opt x opts with 
        | Some (AddIncDir dir) -> "-I" :: dir :: filter_opts xs
        | _ -> 
           !!% "Unknown compiler option:%s@." x;
           assert false
        end
    | x :: xs -> 
        (* exclude object files *)
        match x =~ <:m<\.cm.*$>> with
        | Some _ -> filter_opts xs
        | _ -> x :: filter_opts xs
  in
  let filter_cmd = function
    | cmd :: args -> cmd :: filter_opts args
    | [] -> assert false
  in
  match filter_cmd & Array.to_list cmt.Cmt_format.cmt_args with
  | _com :: opts ->
      let dest = Filename.temp_file "ocamldoc" ".bin" in
      let command = 
        (* <:s<(ocamlc|ocamlopt)(\.opt)?/ocamldoc.opt>> com *)
        "ocamldoc.opt" :: opts
        @ [ (* "-v"; *) "-no-stop"; (* "-html"; *) "-dump"; dest ]
      in
(*
      !!% "Original %s@." & String.concat " // " (Array.to_list cmt.cmt_args);
      !!% "Running %s@." & String.concat " // " command;
*)
      (* CR jfuruse: killing the process during this leaves /tmp/ocamldocxxx.bin *)
      Unix.with_chdir ~at_failure:(fun _exn -> `Error (cmt.Cmt_format.cmt_builddir, command, `Chdir))
        cmt.Cmt_format.cmt_builddir (fun () ->
          match 
            Unix.Command.(
              execvp command |> fold ~init:[] ~f:(fun st -> function
                | `Out, `Read s -> 
                    if Conf.show_ocamldoc_message then 
                      print_endline & "OCamlDoc: " ^ String.chop_eols s;
                    s :: st
                | `Err, `Read s -> 
                    if Conf.show_ocamldoc_message then 
                      prerr_endline & "OCamlDoc: " ^ String.chop_eols s;
                    s :: st
                | _ -> st)
            )
          with
          | Unix.WEXITED 0, _ -> 
              Result.map_error (fun (`Exn e) -> (cmt.Cmt_format.cmt_builddir, command, `Load_dump (Exn.to_string e)))
              & Exn.(flip catch () & fun () ->
                XSpotlib.Exn.protect load_dump dest
                  ~final:(fun _ -> try_ignore Unix.unlink dest)
              )
          | (e, revlines) -> 
              Exn.try_ignore Unix.unlink dest; 
              `Error (cmt.Cmt_format.cmt_builddir, 
                      command, 
                      `Exec (e, rev revlines)))
  | _ -> assert false

open Odoc_types
open Odoc_type
open Odoc_value
open Odoc_class
open Odoc_exception
open Odoc_module

type t = Odoc_info.info

module OCaml = Ocaml
module OCaml_conv = Ocaml_conv
open OCaml_conv

type kind = [ `Type | `Value | `Class | `Class_type | `Exception | `Module | `Module_type ] 
with conv(ocaml)

type entry = string * location * info * kind 

let format_location ppf loc =
  let option f ppf = function
    | None -> Format.string ppf "NONE"
    | Some v -> f ppf v
  in
  Format.fprintf ppf "{impl:%a inter:%a}"
    (option Location.print_loc ) loc.loc_impl
    (option Location.print_loc ) loc.loc_inter

type ref_kind = Odoc_types.ref_kind = 
    RK_module
  | RK_module_type
  | RK_class
  | RK_class_type
  | RK_value
  | RK_type
  | RK_exception
  | RK_attribute
  | RK_method
  | RK_section of text
  | RK_recfield
  | RK_const

and text_element = Odoc_types.text_element =
  | Raw of string
  | Code of string
  | CodePre of string
  | Verbatim of string
  | Bold of text
  | Italic of text
  | Emphasize of text
  | Center of text
  | Left of text
  | Right of text
  | List of text list
  | Enum of text list
  | Newline
  | Block of text
  | Title of int * string option * text
  | Latex of string
  | Link of string * text
  | Ref of string * ref_kind option * text option
  | Superscript of text
  | Subscript of text
  | Module_list of string list
  | Index_list
  | Custom of string * text
  | Target of string * string

and text = text_element list with conv(ocaml)

let oformat ppf info =
  match info.i_desc with
  | None -> Format.string ppf "NO DESC"
  | Some text ->
      OCaml.format_with ocaml_of_text ppf text

let string_of_text = Odoc_info.string_of_text

let to_string info =
  match info.i_desc with
  | Some text -> string_of_text text
  | None -> ""

let format ppf info =
  match info.i_desc with
  | None -> ()
  | Some text -> 
      let open Format in
      let sfix s = <:s<[\s\n\r]+/ /g>> s in
      let rec fs ppf = iter (f ppf)
      and f ppf = function
        | Raw s
        | Code s
        | CodePre s 
        | Verbatim s -> string ppf & sfix s
        | Bold text
        | Italic text
        | Emphasize text
        | Center text
        | Left text
        | Right text -> fs ppf text
        | List texts
        | Enum texts ->
    	    fprintf ppf "@[<v>%a@]"
    	      (list "@ " (fun ppf text ->
    		fprintf ppf "* @[%a@]" fs text))
    	      texts
        | Newline -> pp_force_newline ppf ()
        | Block text -> fprintf ppf "@[%a@]" (list "@ " f) text
        | Title (_lev, _stropt, text) -> 
    	    fprintf ppf "@[=== @[%a@] ===@]" fs text;
	    pp_force_newline ppf ()
        | Latex s -> fprintf ppf "LaTeX{%s}" & sfix s
        | Link (url, text) -> fprintf ppf "[%a](%s)" fs text url
        | Ref (s, _ref_kind_opt, Some text) -> fprintf ppf "%s %a" (sfix s) fs text
        | Ref (s, _ref_kind_opt, None) -> string ppf & sfix s
        | Superscript text -> fprintf ppf "^{%a}" fs text
        | Subscript text -> fprintf ppf "_{%a}" fs text
        | Module_list ls -> list " " string ppf & map sfix ls
        | Index_list -> ()
        | Custom (tag, text) -> fprintf ppf "@@%s %a" (sfix tag) fs text
        | Target (str1, str2) -> fprintf ppf "%s %s" (sfix str1) (sfix str2)
      in
      fprintf ppf "@[%a@]" (list "@ " f) text

let oformat_entry ppf (name, loc, info, kind) =
  Format.fprintf ppf "@[<v>%a %s %a@ %a@]"
    (OCaml.format_with ocaml_of_kind) kind
    name
    format_location loc
    format info

let option name loc info k st = match info with
  | None -> st
  | Some info -> (name, loc, info, k) :: st

let ocaml_of_t t = ocaml_of_string (Odoc_info.info_string_of_info t)
let t_of_ocaml ?trace s = 
  match string_of_ocaml ?trace s with
  | `Ok s -> `Ok (Odoc_info.info_of_string s)
  | `Error e -> `Error e
let t_of_ocaml_exn = Ocaml_conv.exn t_of_ocaml

class fold = object
  inherit [entry list] Odoc_fold.ofold_t_type as super

  method option f st = function
    | None -> st
    | Some v -> f st v

  method list f = fold_left f

  method! t_type st t = let st = super#t_type st t in
    option t.ty_name t.ty_loc t.ty_info `Type st

  method! t_value st t = let st = super#t_value st t in
    option t.val_name t.val_loc t.val_info `Value st                         

  method! t_class st t = let st = super#t_class st t in
    option t.cl_name t.cl_loc t.cl_info `Class st                         

  method! t_exception st t = let st = super#t_exception st t in
    option t.ex_name t.ex_loc t.ex_info `Exception st

  method! t_module st t = let st = super#t_module st t in
    option t.m_name t.m_loc t.m_info `Module st

  method! t_module_type st t = let st = super#t_module_type st t in
    option t.mt_name t.mt_loc t.mt_info `Module_type st

end

let doc_analyze t_modules =
  let fold = new fold in
  fold#list fold#t_module [] t_modules (* |- iter (!!% "%a@." format_entry)  *)

type unix_process_status = Unix.process_status = 
  | WEXITED of int
  | WSIGNALED of int
  | WSTOPPED of int
with conv(ocaml)

type error = string list * [ `Chdir 
                           | `Exec of unix_process_status * string list
                           | `Load_dump of string ]
with conv(ocaml)

let docs_of_cmt cmt =
  match run_doc cmt with
  | `Ok docs -> `Ok (doc_analyze docs)
  | `Error (dir, command, e) ->
      !!% "Error: OCamlDoc %s@." 
        begin match e with
        | `Chdir -> !% "Chdir failed %s" dir
        | `Load_dump mes -> "Dump loading failed: " ^ mes
        | `Exec (_, log) -> !% "Exec failed: cd %s\n%s\n%s" dir (String.concat " " (List.map (fun s -> "\"" ^ s ^ "\"") command)) (String.concat "\n" log)
        end;
      (* CR jfuruse: We should record the tried command somewhere *)
      (* (String.concat " " command); *)
      `Error (command, e)
