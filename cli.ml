(* The entry point *)
let () = 
  try 
    Query.cli ()
  with
  | Cmi_format.Error e -> 
      Format.eprintf "Cmi_format error: %a@."
        Cmi_format.report_error e
