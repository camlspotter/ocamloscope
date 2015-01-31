(** Packages + PATH *)

open Spotlib.Spot
open Path

(* We cannot forget the id numbers, because of the following examples:

   module type S = sig
     type t = ...
   end

   module S = struct
     type t = S.t = ...
   end

   Two S's define the two types [t] with the same name.
*)

type t = 
  | SPpredef
  | SPpack of OCamlFind.Packages.t (** can be linked into more than one package *) 
  | SPident of string
  | SPdot of t * string
  | SPapply of t * t
  | SPattr of attr * t (** only used for the match result *)

and attr = 
  [ `Pack of t * (OCamlFind.Packages.t * (string * string option))
  | `Ident of t * (string * string option)
  | `AfterDot of string * string option
  ]
[@@deriving conv{ocaml_of}]

type t_ = t

let rec strip_attr = function
  | SPdot (t, s) -> SPdot (strip_attr t, s)
  | SPapply (t1, t2) -> SPapply (strip_attr t1, strip_attr t2)
  | SPattr (_, t) -> strip_attr t
  | (SPpredef | SPpack _ | SPident _ as p) -> p

let rec is_attred = function
  | SPattr _ -> true
  | SPapply (t1, t2) -> is_attred t1 || is_attred t2
  | SPdot (t, _) -> is_attred t
  | SPpredef | SPpack _ | SPident _ -> false

module H = Hashcons.Make(struct
  type t = t_
  let name = "Spath.t"
  let equal p1 p2 = match p1, p2 with
    | SPpredef, SPpredef -> true
    | SPpack ps1, SPpack ps2 -> ps1 == ps2 (* already hconsed *)
    | SPident s1, SPident s2 -> s1 == s2
    | SPdot (t1, s1), SPdot (t2, s2) -> t1 == t2 && s1 == s2 (* string hconsed *)
    | SPapply (t11, t12), SPapply (t21, t22) -> t11 == t21 && t12 == t22
    | SPattr _, _ | _, SPattr _ -> assert false   
    | _ -> false
  let hash = Hashtbl.hash
end)

let non_rec_hcons = H.non_rec_hcons

let predef = SPpredef
let pack pks = non_rec_hcons & SPpack pks
let ident s = non_rec_hcons & SPident (Hcons.string s)
let dot t s = non_rec_hcons & SPdot (t, Hcons.string s)
let apply t1 t2 = non_rec_hcons & SPapply (t1, t2)
 
let nhc_pack pks = SPpack pks
let nhc_ident s = SPident s
let nhc_dot t s = SPdot (t, s)
let nhc_apply t1 t2 = SPapply (t1, t2)
let nhc_attr a t = SPattr (a, t) (* We do not need hconsing *)
 
let rec rec_hcons = function
  | SPpredef -> predef
  | SPpack pks -> pack & OCamlFind.Packages.hcons pks
  | SPident s -> ident s
  | SPdot (t, s) -> dot (rec_hcons t) s 
  | SPapply (t1, t2) -> apply (rec_hcons t1) (rec_hcons t2)
  | SPattr _ as p -> p (* not for hcons *)

let rec of_path = function
  | Pident id -> 
      let name = Ident.name id in
      begin match name with
      | "*predef*" | "{*predef*}" -> predef
      | _ when name.[0] = '{' ->
          (* It is an ident made from a package name *)
          pack (OCamlFind.Packages.of_id & String.(sub name 1 & length name - 2))
      | _ ->   
          ident name
      end
  | Pdot (t, n, _pos) -> dot (of_path t) n
  | Papply (t1, t2) -> apply (of_path t1) (of_path t2)

let package_path name =
  let name = "{" ^ name ^ "}" in
  let id = Ident.create_persistent name in
  Path.Pident id

let rec to_path = function
  | SPpack pks -> package_path & OCamlFind.Packages.to_id pks
  | SPident s -> Pident (Ident.create_persistent s)
  | SPdot (t, n) -> Pdot (to_path t, n, 0)
  | SPapply (t1, t2) -> Papply (to_path t1,
                                to_path t2)
  | SPpredef -> Pident ( Ident.create_persistent "{*predef*}" )
  | SPattr (_, t) -> to_path t

let rec look_same p1 p2 =
  if p1 == p2 then true
  else match p1, p2 with
  | SPpredef, SPpredef -> true
  | SPpack pks1, SPpack pks2 -> 
      (* CR jfuruse: This should be pks1 == pks2, but
         for types with non looped link(?), hcons does not touch
         Packages.t (and probably other parts...)
      *)
      pks1 = pks2 
  | SPident s1, SPident s2 -> s1 = s2
  | SPdot (p1, s1), SPdot (p2, s2) -> 
      s1 = s2 && look_same p1 p2
  | SPapply (p11, p12), SPapply (p21, p22) ->
      look_same p11 p21 && look_same p12 p22
  | SPattr (_, p1), p2
  | p1, SPattr (_, p2) -> look_same p1 p2
  | _ -> false

open Longident

(* CR jfuruse: Is hcons required? I think not *)
let rec of_longident = function
  | Lident ("*predef*" | "{*predef*}") -> SPpredef
  | Lident s when s <> "" && s.[0] = '{' -> 
      (* This does not work for {name} but only for {name#0} *)
      let id = String.(sub s 1 (length s - 2)) in
      if id <> "" && id.[0] = '[' then 
        let packs = String.(sub id 1 (length id - 2)) in
        non_rec_hcons 
        & SPpack (OCamlFind.Packages.of_strings 
                  & String.split (function ';' -> true | _ -> false) packs)
      else
        non_rec_hcons & SPpack (OCamlFind.Packages.of_id id)
  | Lident s -> non_rec_hcons & SPident (Hcons.string s)
(*
  | Ldot (Lident ("*predef*" | "{*predef*}"), s) -> 
      (* hack for option *)
      of_longident (Lident s)
*)
  | Ldot (t, s) -> non_rec_hcons & SPdot (of_longident t, Hcons.string s)
  | Lapply (t1, t2) -> non_rec_hcons & SPapply (of_longident t1, of_longident t2)

let rec to_longident = function
  | SPident s -> Lident s
  | SPpredef -> Lident "{*predef*}"
  | SPdot (p, s) -> Ldot (to_longident p, s)
  | SPapply (p1, p2) -> Lapply (to_longident p1, to_longident p2)
  | SPpack _ -> assert false
  | SPattr (_, p) -> to_longident p

open Printer

let print
      ?(packages=`ID)
      ?(remove_package_names=false) 
      ?(just_postfix=false)
      ?opened
      ?(predef=false)
      p0 = 
  let name s = string & match s with
    | "" -> assert false
    | "or" | "mod" | "land" | "lor" | "lxor" | "lsl" | "lsr" | "asr" ->
        "(" ^ s ^ ")"
    | _ when s.[0] = '_' || ('A' <= s.[0] && s.[0] <= 'Z') || ('a' <= s.[0] && s.[0] <= 'z')  -> s
    | _ when s.[0] = '*' -> "( " ^ s ^ " )"
    | _ -> "(" ^ s ^ ")"
  in
  let is_opened p = match opened with 
    | None -> false
    | Some opened -> look_same p opened 
  in
  (* CR jfuruse: code for attr is not looking good... *)
  let rec f p = match p with
    | SPpack pks ->
        begin match packages with
        | `ID -> 
            string (!% "{%s}" (OCamlFind.Packages.to_id pks))
        | `Nick -> 
            string (!% "{%s}" (OCamlFind.Packages.to_string_for_printing pks))
        | `Exact ->
            string (!% "{[%s]}" (String.concat ";" & OCamlFind.Packages.to_strings pks))
        end
    | SPident s -> name s
    | SPpredef -> string "{*predef*}"

    | SPdot (_, s) when just_postfix -> 
        name s
    | SPattr (`AfterDot _, SPdot (_, s)) when just_postfix -> 
        attr `Bold & name s

    | SPdot (SPpredef, s) when not predef -> (* {*predef*}.string => string *)
        name s
    | SPattr (`AfterDot _, SPdot (SPpredef, s)) when not predef -> 
        attr `Bold & name s

    | SPdot (p, s) when is_opened p && not (List.mem s Xpredef.predefined_types) -> 
        name s
    | SPattr (`AfterDot _, SPdot (p, s)) when is_opened p && not (List.mem s Xpredef.predefined_types) -> 
        attr `Bold & name s

    | SPdot (SPpack _, s) when remove_package_names -> 
        name s
    | SPattr (`AfterDot _, SPdot (SPpack _, s)) when remove_package_names -> 
        attr `Bold & name s

    | SPdot (t, n) -> 
        seq [ f t; string "."; name n ]
    | SPattr (`AfterDot _, SPdot (t, n)) -> 
        seq [ f t; string "."; attr `Bold & name n ]

    | SPattr ((`Pack _ | `Ident _), t) -> attr `Bold & f t

    | SPattr (`AfterDot _, _) -> assert false

    | SPapply (t1, t2) -> seq [ f t1; string "("; f t2; string ")" ]
  in
  f p0

let format
    ?packages
    ?remove_package_names
    ?just_postfix
    ?opened
    ?predef
    ()
    ppf p = 
  Printer.format (print ?packages ?remove_package_names ?just_postfix ?opened ?predef) ppf p

let show p = Format.sprintf "%a" (format ~packages:`Exact ~predef:true ()) p

let read s =
  Util.ParseError.catch 
    (fun lexbuf ->
      lexbuf
        |> XParser.pattern_longident Lexer.token
        |> of_longident
        |> fun x -> `Ok x)
    (Lexing.from_string s)

let is_bizarre_id = function
  | "*dummy method*" -> true
  | "" -> true
  | s when s.[0] = '#' -> 
      (* class declaration creates a type starts with #,
         which is not printed in the normal circumstances but 
         printed as "M.#cls" and is not parseable.
         
         It is DIFFERENT from what we see at [val x : #M.cls -> unit]. 
         This one is parsed nicely. 
      *)
      true 
  | _ -> false
  
let rec is_bizarre = function
  | SPdot (t, s) -> is_bizarre_id s || is_bizarre t 
  | SPapply (t1, t2) -> is_bizarre t1 || is_bizarre t2
  | SPattr (_, t) -> is_bizarre t
  | SPpredef | SPpack _ -> false
  | SPident s -> is_bizarre_id s

let test_read t =
  if not & is_bizarre t then
    let s = show t in
    match read s with
    | `Error e -> 
        !!% "Spath: %s: %S: %a@." e s (Ocaml.format_with ocaml_of_t) t;
        assert false
    | `Ok t' when t = t' -> 
        let t = rec_hcons t in
        let t' = rec_hcons t' in
        if t != t' then begin
          !!% "Spath: hcons: %S: %a@." s (Ocaml.format_with ocaml_of_t) t;
          assert false
        end
    | `Ok t' -> 
        !!% "Spath: %S => %S: %a@." s (show t') (Ocaml.format_with ocaml_of_t) t;
        assert false

let too_short_name = function
  | "" -> assert false
  | x when String.length x > 1 -> false
  | x ->
      match x.[0] with
      | 'a' .. 'z' | 'A' .. 'Z' -> true
      | _ -> false (* + is not short *)

let short_look p =
  let rec f1 p = match p with
    | SPident _ 
    | SPpredef -> p
    | SPpack pkgs -> SPident (OCamlFind.Packages.to_id pkgs)
    | SPdot (p, name) when too_short_name name ->
        (* [X.t] is not contracted to [t] *)
        SPdot (f1 p, name)
    | SPdot (p, name) -> 
        begin match f0 p with
        | None -> SPident name
        | Some p -> SPdot (p, name)
        end
    | SPapply (p1, p2) -> SPapply(f1 p1, f1 p2)
    | SPattr (`AfterDot x, SPdot (p, name)) -> 
        begin match f0 p with
        | None -> 
            let t = SPident name in
            SPattr (`Ident (t, x), t)
        | Some p ->
            SPattr (`AfterDot x, SPdot (p, name))
        end
    | SPattr _ -> p (* attributed node should be printed out *)
  and f0 p = match p with
    | SPident _ 
    | SPpredef
    | SPpack _ -> None
    | SPdot (p, name) -> 
        begin match f0 p with
        | None -> None
        | Some p -> Some (SPdot (p, name))
        end
    | SPapply (p1, p2) -> 
        begin match f0 p1 with
        | None -> None
        | Some p1 -> Some (SPapply (p1, f1 p2))
        end
    | SPattr (`AfterDot x, SPdot (p, name)) -> 
        begin match f0 p with
        | None -> 
            let t = SPident name in
            Some (SPattr (`Ident (t, x), t))
        | Some p ->
            Some (SPattr (`AfterDot x, SPdot (p, name)))
        end
    | SPattr _ -> Some p (* attributed node should be printed out *)
  in
  f1 p

