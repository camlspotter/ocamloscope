open Spotlib.Spot
open List

let get_version () = {qx|ocamlc -version|qx} |> snd |> hd |> Spotlib.Spot.String.chop_eols

let is_ocaml_source_root d =
  let files = {qx|ls ${d}|qx} |> snd |> map chop_eols in
  for_all (fun x -> mem x files) [ "ocamldoc"; "stdlib"; "otherlibs" ]

let find_ocaml_source_root () =
  let f d0 = 
    if Filename.is_relative d0 then invalid_arg "find_ocaml_source_root";
    let rec loop rev_st d = 
      if is_ocaml_source_root d then Some (d, List.rev rev_st)
      else if Filename.is_root d then begin
        !!% "find_ocaml_source_root: %s@." d0;
        None
      end else 
        let dir = Filename.dirname d in
        let base = Filename.basename d in
        loop (base :: rev_st) dir
    in
    loop [] d0
  in
  fun d -> 
    if not (File.Test._d d) then invalid_arg "find_ocaml_source_root must take a dir name";
    memoize f d

