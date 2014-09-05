open Spotlib.Spot
open Eliom_content.Html5.F (* provides functions to create HTML nodes *)
open ElMisc

let logdir = lazy (Ocsigen_config.get_logdir ())

(*
let oc = lazy (
  let logdir = Ocsigen_config.get_logdir () in
  open_out_gen [ Open_wronly; Open_creat;  Open_append; ] 0o664 & logdir ^/ "oco.log"
)

let write o = 
  let s = Format.sprintf "%a\n" (Ocaml.format ~no_poly:true ~raw_string:false) o in
  output_string (Lazy.force oc) s
*)

let service = ElServices.Log.service

(*
access.log

Feb 18 07:25:08: access: connection for ocamloscope.herokuapp.com from 10.13.142.203 (Mozilla/5.0 (compatible; Googlebot/2.1; +http://www.google.com/bot.html)) with X-Forwarded-For: 66.249.65.4: ?q=concat 
Feb 18 07:36:09: access: connection for ocamloscope.herokuapp.com from 10.179.0.58 (Mozilla/5.0 (compatible; AhrefsBot/5.0; +http://ahrefs.com/robot/)) with X-Forwarded-For: 5.10.83.29: package?name=json-tc-custom 
Feb 18 07:45:55: access: connection for ocamloscope.herokuapp.com from 10.93.87.26 () with X-Forwarded-For: 54.204.247.177: pingpingping 

warnings.log

Feb 17 17:57:14: main: --Ocsigen_http_client will give to server 107.22.174.247:80 a first probing period for pipelining. 

errors.log

empty :-)
*)

let query = function
  | _ ->
      let rev_lines = 
        Unix.Command.(shell (!% "cat %s/warnings.log" (Lazy.force logdir))
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

