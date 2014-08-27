(* OMAKE_SYNTAX(-syntax camlp4o -package orakuda.syntax,lwt.syntax,meta_conv.syntax) *)
open Spotlib.Spot

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

type t = {
  ocamlc_source_dir : string mc_option;
} with conv(xml)

let default = {
  ocamlc_source_dir = None;
}

let xls = Eliom_config.get_config ()

let config = match List.map t_of_xml xls with
  | [] -> default
  | [`Ok t] -> t
  | [`Error e] -> !!% "%a@." (Meta_conv.Error.format Xml.format) e; assert false
  | _ -> failwithf "Multiple OCO config records found"
