open Spotlib.Spot
open Meta_conv.Open

(*
module SimpleXML = Simplexmlparser

module Xml = struct
  type t = SimpleXML.xml
  let element t attrs xs = SimpleXML.Element (t, attrs, xs)
  let pcdata s = SimpleXML.PCData s
  let deconstr = function
    | SimpleXML.PCData s -> Std_xml.PCData s
    | SimpleXML.Element (t, attrs, xs) -> Std_xml.Element (t, attrs, xs)
  let rec  format ppf = function
    | SimpleXML.PCData s -> Std_xml.format format ppf (Std_xml.PCData s)
    | SimpleXML.Element (t, attrs, xs) -> Std_xml.format format ppf (Std_xml.Element (t, attrs, xs))
end
module Xml_conv = Xml_conv.Make(Xml)

open Meta_conv.Open
open Xml_conv
*)
  
type t = {
(*
  ping_path : string;
  self_ping_host : string mc_option; (** Where to ping ex. "http://xxx" *)
*)
  ocamlc_source_dir : string mc_option;
} [@@deriving conv{ocaml}]

let default = {
  ocamlc_source_dir = None;
}

let config =
  try List.hd & from_Ok & Ocaml.load_with t_of_ocaml "oco_conf.ml"
  with
    | e -> prerr_endline "config load failed"; raise e
  
(*
let xls = Eliom_config.get_config ()

let config = match List.map t_of_xml xls with
  | [] -> default
  | [`Ok t] -> t
  | [`Error e] -> !!% "%a@." (Meta_conv.Error.format Xml.format) e; assert false
  | _ -> failwithf "Multiple OCO config records found"
*)

