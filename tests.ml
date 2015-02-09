open Spotlib.Spot
open Ppx_test.Test
module PTest = Ppx_test.Test
  
module Do(A : sig
  val items : Item.t array
end) = struct

  open A

  let %TEST spath_print_ =
    Array.iter (fun i -> 
      Exn.tee Spath.test_read i.Item.path
        ~handler:(fun _ -> !!% "ITEM on error: %a@." Item.format i)
    ) items

  let %TEST stype_print_ = Stype_test.test items

  let () = run_tests false
end
