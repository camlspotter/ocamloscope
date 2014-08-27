open Hashtbl
type poo
val x : int -> (int, float) t (* int float are in [predef] *)
val y : poo (* it must be distinguished from [predef] *)
val z : fpclass (* must be in [stdlib].Pervasives *)
