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

type search = [ extracted | `ExactPackage ]

type all = search

val to_string : [< all] -> string
val of_string : string -> all option
