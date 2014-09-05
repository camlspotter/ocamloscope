open Spotlib.Spot
open Eliom_content.Html5.F (* provides functions to create HTML nodes *)
open ElMisc

let service = ElServices.Bereth.service

let query arg =
  let rev_lines = 
    Unix.Command.(shell arg 
      |> fold ~init:[] ~f:(fun st -> function
          | (`Out | `Err), `Read s -> s :: st
          | (`Out | `Err), `EOF -> st)
      |> snd
    )
  in
  Lwt.return
  & html
    oco_head
  & body & List.concat &
    List.rev_map (fun s ->
      [ !$ s; br () ] 
    ) rev_lines
    

                 
let () = 
  Eliom_registration.Html5.register
    ~service & fun arg () ->
      query arg

