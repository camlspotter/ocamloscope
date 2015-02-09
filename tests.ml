open Spotlib.Spot
open Ppx_test.Test

module Do(A : sig
  val items : Item.t array
end) = struct

  open A

  let spath =
    label "Spath" & label "Print" & fun_ & fun _ ->
      Array.iter (fun i -> 
        Exn.tee Spath.test_read i.Item.path
          ~handler:(fun _ -> !!% "ITEM on error: %a@." Item.format i)
      ) items

  let () = add spath

  let stype = 
    let print =
      label "Print" & fun_ & fun _ ->
        Stype_test.test items
    in
    label "Stype" & print

  let () = add stype

  let () = run_tests false |> Report.print_then_exit
end
