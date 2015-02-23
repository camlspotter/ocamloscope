open Spotlib.Spot
open Eliom_content.Html5.F (* provides functions to create HTML nodes *)
open ElMisc

let service = ElServices.Source.service

(* Html5_f defines [space () = entity "nbsp", but xhtml does not define it. 
   Chrome fails to display nbsp since it is not known.
*)

let line_id = !% "line_%d"

let code_to_pcdata x = pcdata (x ^ "\n")

let numbered ~highlight = function
  | [] -> tr [ td [ ] ]
  | xs ->
      let linenum n =
        let c = !$ (string_of_int n) in
        code ~a: [ a_class ["srclnum"]; a_id (line_id n) ]
          [ if n = highlight then code ~a: [ a_class [ "srchl" ] ] [c] else c ]
      in
      tr [ td & List.mapi (fun i _ -> linenum (i+1)) xs 
         ; td & flip List.mapi xs (fun i l -> 
           let n = i + 1 in
           let c = code_to_pcdata l in
           code ~a: [ a_class ["src"] ]
             [if n = highlight then code ~a: [ a_class ["srchl"] ] [c] else c])
         ]
  
let () = Eliom_registration.Html5.register ~service
  & fun (path, (digest, line)) () ->
    Lwt.return & html oco_head & body & 
      match Digest.from_hex digest with
      | exception _ -> [ !$% "Error: Invalid digest hex: %s" digest ]
      | dhex ->
          match Source.find (Filename.basename path) ~digest:dhex with
          | None ->
              [ !$% "Error: File %s with digest=%s does not exist locally" path digest ]
          | Some p ->
              match File.to_lines p with
              | `Error (`Exn e) ->
                  [ !$% "Error: File %s with digest=%s line=%d found at %s but displaying it raised an exception %s" path digest line p (Exn.to_string e)]
              | `Ok ls -> 
                  let tr = numbered ~highlight:line ls in
                  [ table [tr] ]
