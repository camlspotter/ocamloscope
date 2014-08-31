open Spotlib.Spot
open Xtest

module Do(A : sig
  val items : Item.t array
end) = struct

  open A

  let spath =
    label "spath" & label "print" & fun_ & fun _ ->
      Array.iter (fun i -> 
        Exn.tee Spath.test_read i.Item.path
          ~handler:(fun _ -> !!% "ITEM on error: %a@." Item.format i)
      ) items
  
  let stype = 
    let print =
      label "print" & fun_ & fun _ ->
        Stype_test.test items
    in
    label "stype" & print

  let () = run false & list [ spath; stype ]
end
