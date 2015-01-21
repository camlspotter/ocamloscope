type extracted = 
  [ `Class
  | `ClassType
  | `ClassField
  | `Constr
  | `Exception
  | `Field
  | `Method
  | `ModType
  | `Module
  | `Type
  | `Value
  | `Package
  ]

type search = extracted

type all = search

let to_string = function
  | `Class        -> "class"
  | `ClassType    -> "class type" 
  | `ClassField   -> "class val" 
  | `Constr       -> "constr" 
  | `Exception    -> "exception"
  | `Field        -> "field"
  | `Method       -> "method"
  | `ModType      -> "module type"
  | `Module       -> "module"
  | `Type         -> "type"
  | `Value        -> "val"
  | `Package      -> "package"

let of_string s = 
  (* CR jfuruse: parsing of kind is very ugly *)
  let s = {s|\s+/ /g|s} s in
  match s with
  | "class"         -> Some `Class
  | "class val"     -> Some `ClassField
  | "class type"    -> Some `ClassType
  | "constr"        -> Some `Constr
  | "exception"     -> Some `Exception
  | "field"         -> Some `Field
  | "method"        -> Some `Method
  | "module type"   -> Some `ModType
  | "module"        -> Some `Module
  | "type"          -> Some `Type
  | "val"           -> Some `Value
  | "package"       -> Some `Package
  | _ -> None

