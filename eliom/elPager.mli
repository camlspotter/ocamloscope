open Html5_types
open Eliom_content.Html5

val pager : 
  item:('item -> [< tr ] D.elt) 
  -> next:(int -> [< li_content_fun ] D.elt) 
  -> prev:(int -> [< li_content_fun ] D.elt) 
  -> here:(int -> [< li_content_fun ] D.elt) 
  -> goto:(int -> [< li_content_fun ] D.elt) 
  -> ('item * int (** size *)) list 
  -> int 
  -> [> table | ul ] D.elt list
