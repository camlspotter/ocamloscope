open Spotlib.Spot

module M : sig

  val with_marshaled_cache : string -> (unit -> 'a) -> 'a
  
  val with_ocamled_cache :
    encoder:('a -> Ocaml.t) 
    -> decoder:(?trace:Ocaml.t Meta_conv.Error.trace -> Ocaml.t -> ('a, Ocaml.t Meta_conv.Error.t) Meta_conv.Result.t) 
    -> string 
    -> (unit -> 'a) 
    -> 'a

end = struct
  let with_marshaled_cache path loader = 
  if File.Test._f path then begin
    (* CR jfuruse: ocamldoc style sanity check *)
    if Conf.show_cache_loading then !!% "Loading cache, %s ...@." path;
    with_ic (open_in_bin path) input_value
  end else begin
    loader () |- fun res -> with_oc (open_out_bin path) (fun oc -> output_value oc res)
  end

  (*
  encoder:('a -> Ocaml.t) ->
    decoder:(Ocaml.t -> ('a, Ocaml.t Meta_conv.Error.t) Meta_conv.Result.t) ->
    string ->
    (unit -> 'a list) -> [> ('a list, Ocaml.load_error) Meta_conv.Result.t ]
  *)
  let with_ocamled_cache
      ~encoder
      ~decoder:(decoder : ?trace:Ocaml.t Meta_conv.Error.trace -> Ocaml.t -> ('a, Ocaml.t Meta_conv.Error.t) Meta_conv.Result.t)
      path loader = 
    if File.Test._f path then begin
      (* CR jfuruse: ocamldoc style sanity check *)
      if Conf.show_cache_loading then !!% "Loading cache, %s ...@." path;
      match Ocaml.load_with_exn decoder path with
      | [x] -> x
      | _ -> failwithf "with_ocamled_cache: %s has more than one element" path
    end else begin
      let res = loader () in
      Ocaml.save_with encoder ~perm: 0o644 ~no_poly:true path [res];
      res
    end
end

include M

module Filename = struct
  include Filename
    
  let change_extension name ~ext =
    Filename.chop_extension name ^ "." ^ ext
end

module Result = struct
  let to_option = function
    | `Ok v -> Some v
    | `Error _ -> None
end

module ParseError = struct
  (* CR jfuruse: it is only available in 4.02 *)
(*
  let catch f v =
    try f v with
    | ( Parsing.Parse_error 
      | Syntaxerr.Escape_error
      | Syntaxerr.Error _ as e) -> 
        match Location.error_of_exn e with
        | None -> raise e
        | Some v -> `Error v

  let format = Location.report_error
*)

  open Lexing

  let string_of_position p =
    !% "%S, line %d, characters %d"
      p.pos_fname
      p.pos_lnum
      (p.pos_cnum - p.pos_bol)

  let catch f lexbuf =
    try f lexbuf with
    | Parsing.Parse_error ->
        `Error (string_of_position (Lexing.lexeme_start_p lexbuf) ^ ": Parse error") 
    | Syntaxerr.Escape_error ->
        `Error (string_of_position (Lexing.lexeme_start_p lexbuf) ^ ": Escape error") 
    | Syntaxerr.Error e ->
        `Error (Format.to_string Syntaxerr.report_error e)
end

