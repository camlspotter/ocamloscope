open Spotlib.Spot

(** test type *)
type t =
  | Test    of (unit -> unit)
  | Labeled of string * t
  | List    of t list

let fun_ f = Test f
let label s t = Labeled (s, t)
let list ts = List ts

module Config = struct
  type arg =
    | Do of Pcre.regexp
    | Dont of Pcre.regexp

  type t = {
    args : arg list;
    show_tests : bool;
    default_go : bool;
  }

  let rev_args = ref []
  let show_tests = ref false

  let from_args default_go = 
    { args = List.rev !rev_args;
      show_tests = !show_tests;
      default_go = default_go;
    }

  let is_go conf label =
    let s = String.concat "/" label in
    match 
      List.find_map_opt (function
        | Do rex when Pcre.pmatch ~rex s -> Some true
        | Dont rex when Pcre.pmatch ~rex s -> Some false
        | _ -> None) conf.args
    with
    | Some b -> b
    | None -> conf.default_go
end
  
let arg_specs = 
  let open Arg in
  let open Config in      
  [ "--test", String (fun s -> rev_args +::= Do (Pcre.regexp s)), "<string>: Perform tests match with the string";
    "--no-test", String (fun s -> rev_args +::= Dont (Pcre.regexp s)), "<string>: Skip tests match with the string";
    "--show-tests", Set show_tests, ": List all the tests";
  ]

module State = struct
  type t = {
    rev_label : string list;
    config : Config.t; (** We should test this or not *)
  }

  let add_label l st = { st with rev_label = l :: st.rev_label }

  let labels st = List.rev st.rev_label

  let is_go t = Config.is_go t.config & List.rev t.rev_label

  let from_args default_go =
    { rev_label = [];
      config = Config.from_args default_go;
    }
end

let rec iter st go prim = function
  | Test t -> prim st t
  | Labeled (l, t) ->
      let st = State.add_label l st in
      go st (fun () -> iter st go prim t)
  | List ts -> List.iter (iter st go prim) ts

let show st t = 
  let go _st f = f () in
  let prim st _ = 
    !!% "%s: %s@." 
      (String.concat "/" (State.labels st)) 
      (if State.is_go st then "go" else "skip")
  in
  iter st go prim t;
  exit 0

let run' st = 
  let go _st f = f () in
  let prim st t = 
    if State.is_go st then begin 
      let name = String.concat "/" (State.labels st) in
      !!% "Testing %s...@." name;
      begin try t () with
      | e -> !!% "Error: %s: %s@." (String.concat "." (State.labels st)) (Exn.to_string e); raise e end;
      !!% "Done %s@." name;
    end
  in
  iter st go prim

let run default_go t = 
  let st = State.from_args default_go in
  if st.State.config.Config.show_tests then
    show st t
  else
    run' st t
