open Spotlib.Spot
open Lwt

let catch_lwt f = catch f & fun exn -> return & `Error (`Exn exn)
  
let get_string url =
  catch_lwt & fun () ->
    lwt frame = Ocsigen_http_client.get_url url in
    match frame.Ocsigen_http_frame.frame_content with
    | None -> return (`Error `No_content)
    | Some stream_t ->
        let stream = Ocsigen_stream.get stream_t in
        lwt contents = Ocsigen_stream.string_of_stream 10_000_000 stream in
        return (`Ok contents)

let get_xml url =
  lwt s = get_string url in
  return & Result.bind s & Exn.catch Simplexmlparser.xmlparser_string

let download ~url ~dest =
  catch_lwt & fun () ->
    lwt frame = Ocsigen_http_client.get_url url in
    match frame.Ocsigen_http_frame.frame_content with
    | None -> return (`Error `No_content)
    | Some stream_t ->
        let oc = open_out dest in
        let open Ocsigen_stream in
        let stream = get stream_t in
        let rec save stream = 
          lwt step = next stream in
          match step with
          | Finished _ -> close_out oc; return (`Ok ())
          | Cont (s, stream) -> 
              output_string oc s;
              save stream
        in
        save stream
