open Spotlib.Spot
open Orakuda.Std
open Regexp.Infix
open List

type t = {
  time : string;
  agent : string;
  ip : string;
  path : string
}

(*
let simplify_agent = <:s<\([^)]*\)//g>>
*)
let simplify_agent = id

let lines = Result.from_Ok & File.to_lines "access.log"

let accesses = flip filter_map lines (fun l ->
  match l =~ <:m<(.*): access: connection for [0-9]+\.[0-9]+\.[0-9]+\.[0-9]+ from [0-9]+\.[0-9]+\.[0-9]+\.[0-9]+ \((.*)\) with X-Forwarded-For: ([^:]+): (.*)>> with
  | None -> 
      !!% "??? %s@." l;
      None
  | Some p -> 
      Some { time = p#_1; agent = simplify_agent p#_2; ip = p#_3; path = p#_4 }
  )

let agents = Hashtbl.create 101

let () = flip iter accesses & fun t -> incr & Hashtbl.find_or_add (fun _ -> ref 0) agents t.agent

let () =
  Hashtbl.to_list agents
  |> sort (fun x y -> compare !(snd y) !(snd x)) 
  |> iter (fun (n, r) -> !!% "%d: %s@." !r n)

let () = flip iter accesses & fun t ->
  match t.path =~ <:m<\?q=(.*)>> with
  | None -> ()
  | Some m ->
      prerr_endline & Netencoding.Url.decode ~plus:true m#_1

