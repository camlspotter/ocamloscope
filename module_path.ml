open Spotlib.Spot

type t = string [@@deriving conv{ocaml}]

external of_string : string -> t = "%identity" 
external to_string : t -> string = "%identity" 

let of_path path = 
  let dir = Filename.dirname path in
  let base = Filename.basename path in
  let cap = String.capitalize & fst & Filename.split_extension base in
  dir ^/ cap
  
let file ext t =
  let open Option in
  let dir = Filename.dirname t in
  let base = Filename.basename t in
  let uncap = String.uncapitalize base in
  let cap = String.capitalize base in
  let if_exists p = if Sys.file_exists p then Some p else None in
  if_exists (dir ^/ uncap ^ ext)
  >>=! fun () -> if_exists (dir ^/ cap ^ ext)

let modname t = String.capitalize & Filename.basename t
