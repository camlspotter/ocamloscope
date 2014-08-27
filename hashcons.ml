open Spotlib.Spot

type 'a hcons = 'a
external (!>) : 'a hcons -> 'a = "%identity"

let clear_all_tables_ref = ref (fun () -> ())
let clear_all_tables () = !clear_all_tables_ref ()

let report_ref = ref (fun () -> ())
let report () = !report_ref ()

module Make(A : sig
    include Hashtbl.HashedType
    val name : string
end) = struct
  module Elem = A
  include Weak.Make(A)
  let tbl = create 1023

  let size = ref 0
  let hit = ref 0
    
  let non_rec_hcons v = 
    try 
      find tbl v |- fun _ -> incr hit 
    with Not_found ->
      add tbl v;
      incr size;
      v

  let clear () = 
    clear tbl;
    size := 0;
    hit := 0

  let report () =
    !!% "HCONS %s: size=%d hit=%d ratio=%f@." Elem.name !size !hit (float (!size + !hit) /.  float !size)

  let () = 
    let f = !clear_all_tables_ref in 
    clear_all_tables_ref := (fun () -> clear (); f ());
    let f = !report_ref in
    report_ref := (fun () -> report (); f ())
end
