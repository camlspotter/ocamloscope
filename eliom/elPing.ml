(** Heroku kills idle processes, and this is bad for OCamlOScope, 
    since it takes time to warm up now for loading + initial hashconsing
*)

open Spotlib.Spot
open Eliom_content.Html5.F (* provides functions to create HTML nodes *)
open ElMisc

let service = ElServices.Ping.service

let query () =
  Lwt.return
  & html
    oco_head
  & body & [ !$ "ping" ] 
                 
let () = 
  Eliom_registration.Html5.register
    ~service & fun () () -> query ()


let access_self url =
  lwt res = OcHttp.get_string url in
  !!% "self access! %s@." begin match res with
    | `Error (`Exn exn) -> Exn.to_string exn
    | `Error `No_content -> "No content"
    | `Ok _ -> "ok"
    end;
  Lwt.return ()

let _never_end =
  let open ElConfig in
  match config.self_ping_host with
  | None -> Lwt.return ()
  | Some h ->
      let url = !% "%s/%s" h ElServices.Ping.name in
      let rec loop () = 
        lwt () = Lwt_unix.sleep 120.0 in
        lwt () = access_self url in
        loop ()
      in
      loop ()
