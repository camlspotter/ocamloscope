(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(* $Id: hashtbl.ml 12475 2012-05-24 14:55:00Z doligez $ *)

(* Hash tables *)

external seeded_hash_param : int -> int -> int -> 'a -> int = "caml_hash" "noalloc"
external old_hash_param : int -> int -> 'a -> int = "caml_hash_univ_param" "noalloc"

let hash x = seeded_hash_param 10 100 0 x
let hash_param n1 n2 x = seeded_hash_param n1 n2 0 x
let seeded_hash seed x = seeded_hash_param 10 100 seed x

(* We do dynamic hashing, and resize the table and rehash the elements
   when buckets become too long. *)

type 'a t =
  { mutable size: int;                        (* number of entries *)
    mutable data: 'a list array;              (* the buckets *)
    mutable seed: int;                        (* for randomization *)
    initial_size: int;                        (* initial array size *)
  }

(* To pick random seeds if requested *)

let randomized_default =
  let params =
    try Sys.getenv "OCAMLRUNPARAM" with Not_found ->
    try Sys.getenv "CAMLRUNPARAM" with Not_found -> "" in
  String.contains params 'R'

let randomized = ref randomized_default

let randomize () = randomized := true

let prng = lazy (Random.State.make_self_init())

(* Creating a fresh, empty table *)

let rec power_2_above x n =
  if x >= n then x
  else if x * 2 > Sys.max_array_length then x
  else power_2_above (x * 2) n

let create ?(random = !randomized) initial_size =
  let s = power_2_above 16 initial_size in
  let seed = if random then Random.State.bits (Lazy.force prng) else 0 in
  { initial_size = s; size = 0; seed = seed; data = Array.make s [] }

let clear h =
  h.size <- 0;
  let len = Array.length h.data in
  for i = 0 to len - 1 do
    h.data.(i) <- []
  done

let reset h =
  let len = Array.length h.data in
  if Obj.size (Obj.repr h) < 4 (* compatibility with old hash tables *)
    || len = h.initial_size then
    clear h
  else begin
    h.size <- 0;
    h.data <- Array.make h.initial_size []
  end

let copy h = { h with data = Array.copy h.data }

let length h = h.size

let resize indexfun h =
  let odata = h.data in
  let osize = Array.length odata in
  let nsize = osize * 2 in
  if nsize < Sys.max_array_length then begin
    let ndata = Array.make nsize [] in
    h.data <- ndata;          (* so that indexfun sees the new bucket count *)
    let rec insert_bucket = function
        [] -> ()
      | key :: rest ->
          insert_bucket rest; (* preserve original order of elements *)
          let nidx = indexfun h key in
          ndata.(nidx) <- key :: ndata.(nidx) in
    for i = 0 to osize - 1 do
      insert_bucket odata.(i)
    done
  end

let key_index h key =
  (* compatibility with old hash tables *)
  if Obj.size (Obj.repr h) >= 3
  then (seeded_hash_param 10 100 h.seed key) land (Array.length h.data - 1)
  else (old_hash_param 10 100 key) mod (Array.length h.data)

let add h key =
  let i = key_index h key in
  let bucket = key :: h.data.(i) in
  h.data.(i) <- bucket;
  h.size <- h.size + 1;
  if h.size > Array.length h.data lsl 1 then resize key_index h

let remove h key =
  let rec remove_bucket = function
    | [] ->
        []
    | k :: next ->
        if compare k key = 0
        then begin h.size <- h.size - 1; next end
        else k :: remove_bucket next in
  let i = key_index h key in
  h.data.(i) <- remove_bucket h.data.(i)

let rec find_rec key = function
  | [] ->
      raise Not_found
  | k :: rest ->
      if compare key k = 0 then k else find_rec key rest

let find h key =
  match h.data.(key_index h key) with
  | [] -> raise Not_found
  | k1 :: rest1 ->
      if compare key k1 = 0 then k1 else
      match rest1 with
      | [] -> raise Not_found
      | k2 :: rest2 ->
          if compare key k2 = 0 then k2 else
          match rest2 with
          | [] -> raise Not_found
          | k3 :: rest3 ->
              if compare key k3 = 0 then k3 else find_rec key rest3

let find_all h key =
  let rec find_in_bucket = function
  | [] ->
      []
  | k :: rest ->
      if compare k key = 0
      then k :: find_in_bucket rest
      else find_in_bucket rest in
  find_in_bucket h.data.(key_index h key)

let replace h key =
  let rec replace_bucket = function
    | [] ->
        raise Not_found
    | k :: next ->
        if compare k key = 0
        then k :: next
        else k :: replace_bucket next in
  let i = key_index h key in
  let l = h.data.(i) in
  try
    h.data.(i) <- replace_bucket l
  with Not_found ->
    h.data.(i) <- key :: l;
    h.size <- h.size + 1;
    if h.size > Array.length h.data lsl 1 then resize key_index h

let mem h key =
  let rec mem_in_bucket = function
  | [] ->
      false
  | k :: rest ->
      compare k key = 0 || mem_in_bucket rest in
  mem_in_bucket h.data.(key_index h key)

let iter f h =
  let rec do_bucket = function
    | [] ->
        ()
    | k :: rest ->
        f k; do_bucket rest in
  let d = h.data in
  for i = 0 to Array.length d - 1 do
    do_bucket d.(i)
  done

let fold f h init =
  let rec do_bucket b accu =
    match b with
      [] ->
        accu
    | k :: rest ->
        do_bucket rest (f k accu) in
  let d = h.data in
  let accu = ref init in
  for i = 0 to Array.length d - 1 do
    accu := do_bucket d.(i) !accu
  done;
  !accu

let find_or_add tbl v =
  try find tbl v with
  | Not_found ->
      add tbl v; v

type statistics = {
  num_bindings: int;
  num_buckets: int;
  max_bucket_length: int;
  bucket_histogram: int array
}

let rec bucket_length accu = function
  | [] -> accu
  | _ :: rest -> bucket_length (accu + 1) rest

let stats h =
  let mbl =
    Array.fold_left (fun m b -> max m (bucket_length 0 b)) 0 h.data in
  let histo = Array.make (mbl + 1) 0 in
  Array.iter
    (fun b ->
      let l = bucket_length 0 b in
      histo.(l) <- histo.(l) + 1)
    h.data;
  { num_bindings = h.size;
    num_buckets = Array.length h.data;
    max_bucket_length = mbl;
    bucket_histogram = histo }

(* Functorial interface *)

module type HashedType =
  sig
    type t
    val equal: t -> t -> bool
    val hash: t -> int
  end

module type SeededHashedType =
  sig
    type t
    val equal: t -> t -> bool
    val hash: int -> t -> int
  end

module type S =
  sig
    type key
    type t
    val create: int -> t
    val clear : t -> unit
    val reset : t -> unit
    val copy: t -> t
    val add: t -> key -> unit
    val remove: t -> key -> unit
    val find: t -> key -> key
    val find_all: t -> key -> key list
    val replace : t -> key -> unit
    val mem : t -> key -> bool
    val iter: (key -> unit) -> t -> unit
    val fold: (key -> 'b -> 'b) -> t -> 'b -> 'b
    val length: t -> int
    val stats: t -> statistics
    val find_or_add : t -> key -> key
  end

module type SeededS =
  sig
    type key
    type t
    val create : ?random:bool -> int -> t
    val clear : t -> unit
    val reset : t -> unit
    val copy : t -> t
    val add : t -> key -> unit
    val remove : t -> key -> unit
    val find : t -> key -> key
    val find_all : t -> key -> key list
    val replace : t -> key -> unit
    val mem : t -> key -> bool
    val iter : (key -> unit) -> t -> unit
    val fold : (key -> 'b -> 'b) -> t -> 'b -> 'b
    val length : t -> int
    val stats: t -> statistics
    val find_or_add : t -> key -> key
  end

module MakeSeeded(H: SeededHashedType): (SeededS with type key = H.t) =
  struct
    type key = H.t
    type hashtbl = key t
    type t = hashtbl
    let create = create
    let clear = clear
    let reset = reset
    let copy = copy

    let key_index h key =
      (H.hash h.seed key) land (Array.length h.data - 1)

    let add h key =
      let i = key_index h key in
      let bucket = key :: h.data.(i) in
      h.data.(i) <- bucket;
      h.size <- h.size + 1;
      if h.size > Array.length h.data lsl 1 then resize key_index h

    let remove h key =
      let rec remove_bucket = function
        | [] ->
            []
        | k :: next ->
            if H.equal k key
            then begin h.size <- h.size - 1; next end
            else k :: remove_bucket next in
      let i = key_index h key in
      h.data.(i) <- remove_bucket h.data.(i)

    let rec find_rec key = function
      | [] ->
          raise Not_found
      | k :: rest ->
          if H.equal key k then k else find_rec key rest

    let find h key =
      match h.data.(key_index h key) with
      | [] -> raise Not_found
      | k1 :: rest1 ->
          if H.equal key k1 then k1 else
          match rest1 with
          | [] -> raise Not_found
          | k2 :: rest2 ->
              if H.equal key k2 then k2 else
              match rest2 with
              | [] -> raise Not_found
              | k3 :: rest3 ->
                  if H.equal key k3 then k3 else find_rec key rest3

    let find_all h key =
      let rec find_in_bucket = function
      | [] ->
          []
      | k :: rest ->
          if H.equal k key
          then k :: find_in_bucket rest
          else find_in_bucket rest in
      find_in_bucket h.data.(key_index h key)

    let replace h key =
      let rec replace_bucket = function
        | [] ->
            raise Not_found
        | k :: next ->
            if H.equal k key
            then key :: next
            else k :: replace_bucket next in
      let i = key_index h key in
      let l = h.data.(i) in
      try
        h.data.(i) <- replace_bucket l
      with Not_found ->
        h.data.(i) <- key :: l;
        h.size <- h.size + 1;
        if h.size > Array.length h.data lsl 1 then resize key_index h

    let mem h key =
      let rec mem_in_bucket = function
      | [] ->
          false
      | k :: rest ->
          H.equal k key || mem_in_bucket rest in
      mem_in_bucket h.data.(key_index h key)

    let iter = iter
    let fold = fold
    let length = length
    let stats = stats

    let find_or_add tbl v =
      try find tbl v with
      | Not_found ->
          add tbl v; v
  end

module Make(H: HashedType): (S with type key = H.t) =
  struct
    include MakeSeeded(struct
        type t = H.t
        let equal = H.equal
        let hash (_seed: int) x = H.hash x
      end)
    let create sz = create ~random:false sz
  end
