open Spotlib.Spot
open Eliom_content.Html5.F (* provides functions to create HTML nodes *)
open ElMisc

let service = ElServices.Source.service

(* Html5_f defines [space () = entity "nbsp", but xhtml does not define it. 
   Chrome fails to display nbsp since it is not known.
*)

let line_id = !% "line_%d"

let space = entity "#160"

let code_to_pcdata = function
  (* CR jfuruse: very preliminary *)
  | "" -> [ space ]
  | s ->
    let len = String.length s in
    let rec loop n st = 
      if n < 0 then st
      else
        match String.unsafe_get s n with
        | ' ' (* for empty line *) -> loop (n-1) (space :: st)
        | '\t' -> 
            let makei f n = 
              let rec loop st = function
                | 0 -> st
                | n -> loop (f (n-1) :: st) (n-1)
              in
              assert (n>=0);
              loop [] n
            in
            loop (n-1) (makei (fun _ -> space) (8 - (n mod 8)) @ st)
        | c -> loop (n-1) (!$ (String.make 1 c) :: st)
    in
    loop (len-1) []

let numbered ~highlight = function
  | [] -> tr [ td [ !$ "empty file" ] ]
  | xs ->
      let linenum n = 
        div ~a: [ a_class ( [ "source-line" ]
                            @ if n = highlight then [ "source-highlight" ]
                              else [] )
                ; a_id (line_id n) ]
          [ !$ (string_of_int n) ] 
      in
      tr [ td & List.mapi (fun i _ -> linenum (i+1)) xs 
         ; td & List.mapi (fun i l -> 
           let n = i + 1 in
           div ~a: [ a_class (if n = highlight then [ "source-highlight" ]
                              else []) ]
             [ code (code_to_pcdata l) ]) xs 
         ]
  
let () = Eliom_registration.Html5.register ~service
  & fun (path, (digest, line)) () ->
    Lwt.return & html oco_head & body & 
      match Digest.from_hex digest with
      | exception _ -> [ !$% "Invalid digest hex: %s" digest ]
      | dhex ->
          match Source.find (Filename.basename path) ~digest:dhex with
          | None ->
              [ !$% "File %s with digest=%s does not exist locally" path digest ]
          | Some p ->
              match File.to_lines p with
              | `Error (`Exn e) ->
                  [ !$% "File %s with digest=%s line=%d found at %s but displaying it raised an exception %s" path digest line p (Exn.to_string e)]
              | `Ok ls -> 
                  let tr = numbered ~highlight:line ls in
                  [ table [tr] ]
