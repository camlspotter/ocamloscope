open Spotlib.Spot
open Ppx_orakuda.Regexp.Infix
open List

type t =
  | Exclude of string (** -spotlib *)

let to_string = function
  | Exclude s -> "-" ^ s

let parse s =
  match s with
  | "" -> None
  | _ -> Option.do_;
      res <-- (s =~ {m|-([A-Za-z.0-9_-]+)|m});
      return & Exclude res#_1

let parse_query s =
  let tokens = String.split (function ' ' | '\t' -> true | _ -> false) s in
  let ts = map (fun s -> s, parse s) tokens in
  let ts1, rest = span (function (_,Some _) -> true | _ -> false) ts in
  let rev_ts2, rev_rest = span (function (_,Some _) -> true | _ -> false) & rev rest in
  (map (from_Some *< snd) (ts1 @ rev rev_ts2),
   String.concat " " & rev_map fst rev_rest)

let compile q =
  let match_ = OCamlFind.Packages.cached_match in
  let exclude s = let m = match_ s in fun t -> not (m t) in
  let ands ms = fun t -> for_all (fun m -> m t) ms in
  ands & map (function | Exclude s -> exclude s) q

