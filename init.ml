let () =
  let open Gc in
  let c = get () in
  set { c with max_overhead = 100 }
