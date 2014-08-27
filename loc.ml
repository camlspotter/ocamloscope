open Spotlib.Spot
open Ocaml_conv

module Digest = struct
  include Digest
  let ocaml_of_t = ocaml_of_string *< to_hex
  let t_of_ocaml ?(trace=[]) o = 
    (* CR jfuruse: this is very hard to write. Meta_conv should be improved *)
    Result.(bind (string_of_ocaml ~trace o) & fun s ->
      try return & from_hex s with exn -> `Error (Meta_conv.Error.Exception exn, o, trace))
end

type lexing_position = Lexing.position = {
    pos_fname : string;
    pos_lnum : int;
    pos_bol : int;
    pos_cnum : int;
  } with conv(ocaml)

type location_t = Location.t = {
    loc_start: lexing_position;
    loc_end: lexing_position;
    loc_ghost: bool;
  } with conv(ocaml)

(** special location *)


type t = { digest : Digest.t option;
           pack   : [ `None | `OCamlc of string | `OPAM of string ];
           loc : location_t (* it contains path names... *)
         } with conv(ocaml)

let format ppf t = 
  let open Format in 
  match t.pack with
  | `None -> Location.print_loc ppf t.loc
  | `OCamlc s -> fprintf ppf "OCamlc %s %a" s Location.print_loc t.loc 
  | `OPAM s -> fprintf ppf "OPAM %s %a" s Location.print_loc t.loc 

module H = Hashcons.Make(struct
  type _t = t
  type t = _t
  let equal t1 t2 = t1 = t2
  let hash = Hashtbl.hash
  let name = "Loc.t"
end)

let rec_hcons t =
  H.non_rec_hcons { digest = Option.map Hcons.string t.digest;
                    pack = begin match t.pack with
                      | `None -> `None
                      | `OCamlc s -> `OCamlc (Hcons.string s)
                      | `OPAM s -> `OPAM (Hcons.string s)
                    end;
                    loc = Hcons.location t.loc }

let create digest pack loc = H.non_rec_hcons { digest; pack; loc }

let id l = 
  flip Option.map l.digest (fun d ->
    let md5 = Digest.to_hex d in
    let p = match l.pack with
      | `None -> ""
      | `OCamlc s -> !% "ocamlc/%s" s
      | `OPAM s -> s
    in
    let l = l.loc.Location.loc_start.Lexing.pos_lnum in
    p, md5, l)
