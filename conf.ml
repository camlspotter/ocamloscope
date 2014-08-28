open Spotlib.Spot

module M = struct
  let dump = ref false
  let rev_args = ref []
  let compiler_source_dir = ref ""
  let show_ocamldoc_message = ref false
  let show_cache_loading = ref false
  let show_scanned_ocamlfind_module_list = ref false
  let show_stat = ref false
  let prof_match_types = ref false
  let data_dir = ref "data"

  let () = 
    let f = fun s -> rev_args +::= s in
    let open Arg in
    let spec = [ "-d", Set dump, "dump" 
               ; "-c", Set_string compiler_source_dir, "compiler source dir" 
               ; "--show-ocamldoc-message", Set show_ocamldoc_message, "show ocamldoc log"
               ; "--show-cache-loading", Set show_cache_loading, "show cache log"
               ; "--show-scanned-ocamlfind-module-list", Set show_scanned_ocamlfind_module_list, "show scanned OCamlFind modules"
               ; "--data-dir", Set_string data_dir, "data dir where oco_*.bin args are stored"
               ; "--show-stat", Set show_stat, "show db statistics, then exit"
               ; "--prof-match-types", Set prof_match_types, "profile type matchings"
               ] 
    in
    parse (spec @ Xtest.arg_specs) f "oco"
end

let dump = !M.dump
let args = List.rev !M.rev_args
let compiler_source_dir = !M.compiler_source_dir
let show_ocamldoc_message = !M.show_ocamldoc_message
let show_cache_loading = !M.show_cache_loading
let show_scanned_ocamlfind_module_list = !M.show_scanned_ocamlfind_module_list
let show_stat = !M.show_stat
let prof_match_types = !M.prof_match_types

let data_dir = !M.data_dir

let () = 
  if not (File.Test._d data_dir) then 
    failwithf "ERROR: directory '%s' does not exists" data_dir 

let () = 
  if dump && compiler_source_dir = "" then
    failwithf "ERROR: dump (-d) requires the compiler directory with -c"
    


