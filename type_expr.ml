open Spotlib.Spot

let format ppf ty = 
  Printtyp.reset_and_mark_loops ty; 
  Printtyp.type_expr ppf ty

let show = Format.to_string format

let read str = 
  Util.ParseError.catch 
    (fun lexbuf ->
      let ty = YParser.poly_type Lexer.token lexbuf in
      (* We need this to avoid 
         The type constructor list would escape its scope: "int list"
      *)
      Ctype.init_def(Ident.current_time());  (* CR jfuruse: current_time is necessary? *)
      `Ok (Typetexp.transl_type_scheme Env.initial_unsafe_string ty).Typedtree.ctyp_type)
    (Lexing.from_string str)


