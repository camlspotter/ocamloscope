open Spotlib.Spot

let escape s =
  let len = String.length s in
  let buf = Buffer.create & len * 2 in
  for i = 0 to len - 1 do
    let c = Char.code s.[i] in
    let h = Char.chr & c / 16 + Char.code 'A' in
    let l = Char.chr & c mod 16 + Char.code 'A' in
    Buffer.add_char buf h;
    Buffer.add_char buf l;
  done;
  Buffer.contents buf

let unescape s =
  let len = String.length s in
  if len mod 2 <> 0 then failwith "wrong length";
  let buf = Buffer.create & len / 2 in
  for i = 0 to len / 2 - 1 do
    let h = Char.code s.[i*2] - Char.code 'A' in
    let l = Char.code s.[i*2+1] - Char.code 'A' in
    if h < 0 || h > 15 then failwith "wrong char";
    if l < 0 || l > 15 then failwith "wrong char";
    let c = Char.chr (h * 16 + l) in
    Buffer.add_char buf c;
  done;
  Buffer.contents buf

let magic = "OCamlOScope__"

let escape_package n = magic ^ escape n

let rex = Pcre.regexp "{([^}]+)}"

let escape_query s =
  Pcre.substitute_substrings ~rex
    ~subst: (fun ss -> escape_package & Pcre.get_substring ss 1)
    s

let unescape_package n = 
  match String.is_prefix' magic n with
  | None -> n (* no need *)
  | Some s -> "{" ^ unescape s ^ "}"

let unescape_longident li = 
  let open Longident in
  let rec f = function
    | Lident s -> Lident (unescape_package s)
    | Ldot (t, s) -> Ldot (f t, unescape_package s)
    | Lapply (t1, t2) -> Lapply (f t1, f t2)
  in
  f li

let unescape_core_type ty =
  let open Ast_mapper in
  let open Location in
  let open Parsetree in
  let lloc l = {l with txt= unescape_longident l.txt} in
  let extend super = 
    let typ self ty = match ty.ptyp_desc with 
      | Ptyp_constr (l, ctys) ->
          super.typ self { ty with ptyp_desc = Ptyp_constr (lloc l, ctys) }
      | Ptyp_class (l, ctys) ->
          super.typ self { ty with ptyp_desc = Ptyp_class (lloc l, ctys) }
      | Ptyp_package ((l, ltys)) ->
          super.typ self { ty with ptyp_desc = Ptyp_package (lloc l, List.map (fun (l,ty) -> lloc l, ty) ltys) }
      | _ -> super.typ self ty
    in
    { super with typ }
  in
  let mapper = extend default_mapper in
  mapper.typ mapper ty
    
