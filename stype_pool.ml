(* The pool of types *)

(* Currently we have 500k types in the db but only it contains
   only 350k unique types. 

   Top 2400 types of 350k uniques are shared more than 10 times.

   Top 17800 types are shared more than 3 times.
   
   Top 58000 types are shared at least once.

   We can skip lots of type matches by performing the match only once
   for each unique type.
*)

open Spotlib.Spot
open List
open Item

module M = struct
  include Hashtbl.Make(Stype_hcons.HashedType) 
  let to_list t =
    let r = ref [] in
    iter (fun k v -> r +::= (k,v)) t;
    !r
end

let build items = 
  let tbl = M.create 1023 in
  Array.iter (fun i ->
    match Item.type_of_item i with
    | None -> ()
    | Some ty ->
        try
          let (count, p) = M.find tbl ty in
          M.replace tbl ty (count+1, p)
        with
        | Not_found ->
            M.add tbl ty (1, ref (-1) (* will be filled by the pos *))) items;
  !!% "Type pool:@.";
  !!% "%d different types@." & M.length tbl;
  let sorted = List.sort (fun (_, (c,_)) (_, (c',_)) -> compare c' c) & M.to_list tbl in
  let top = 20000 in
  let a = Array.create top Stype.Nil in
  let rec f i = function
    | [] -> ()
    | _ when i >= top -> ()
    | (t, (_,r))::ts ->
        Array.unsafe_set a i t;
        r := i;
        f (i+1) ts
  in
  f 0 sorted;
  a, 
  fun ty -> 
    try 
      match M.find tbl ty with
      | (_, { contents = (-1) }) -> None
      | (_, { contents = n }) -> Some n
    with Not_found -> None

let wrap_kind wrap = function
  | Class -> Class
  | ClassType -> ClassType
  | ClassField (vf, ty) -> ClassField (vf, wrap ty)
  | Constr ty -> Constr (wrap ty)
  | Exception ty -> Exception (wrap ty)
  | Field ty -> Field (wrap ty)
  | Method (pv, vf, ty) -> Method (pv, vf, wrap ty)
  | ModType -> ModType
  | Module -> Module
  | Type (tys, tyopt, k) -> Type (map wrap tys, Option.map wrap tyopt, k)
  | Value ty -> Value (wrap ty)
  | Package (ps,xs) -> Package (ps,xs)

let wrap_item wrap i = { i with kind = wrap_kind wrap i.kind }

let poolize items =
  let a, f = build items in
  let wrap ty = 
    match f ty with
    | None -> Not_pooled ty
    | Some n -> Pooled n
  in
  Array.map (wrap_item wrap) items,
  a

let unpool_item a i = 
  let wrap = function
    | Not_pooled ty -> ty
    | Pooled i -> Array.unsafe_get a i
  in
  wrap_item wrap i
