open Spotlib.Spot
open Query

(** Prevent searching the same thing many times in the Web page interface *)

type key = Query.t list

type error = [ `Checksum_failure | `Shrink_too_many ]

module Raw : sig
  (** Hashtbl from query => result, with refcount *)
  (* CR jfuruse: maybe reusable *)

  val get_current_size : unit -> int
  (** Sum of the cached results *)

  val get_current_refcount : unit -> int
  (** Sum of ref counts. Used for invariant check *)

  val add_new : key -> QueryResult.t -> (unit, [`Already_in_cache]) Result.t
  (** Add a new query result *)

  val incr : key -> (unit, [`Not_found ]) Result.t
  (** Increment the refcount of the key *)

  val decr : key -> unit
  (** Decrement the refcount of the key. If its refcount reaches to 0,
      the cached query result is discarded. *)

  val find : key -> (int * QueryResult.t) option
  (** Find the query result and its current refcount *)

  val clear : unit -> unit
  (** Forget everything *)
  
end = struct
  let t : (key, int (** ref count *) * QueryResult.t) Hashtbl.t = Hashtbl.create 107
  let current_size = ref 0
  let current_refcount = ref 0
  let get_current_size () = !current_size
  let get_current_refcount () = !current_refcount
  let add_new k v =
    if Hashtbl.mem t k then `Error `Already_in_cache
    else begin 
      Hashtbl.add t k (1, v);
      incr current_refcount;
      current_size += (QueryResult.size v);
      `Ok ()
    end
  (* CR jfuruse: 2 key queries. Hashtbl should have more general API to reduce the number of queries to 1 *)

  let incr k =
    match Hashtbl.find_opt t k with
    | None -> `Error `Not_found
    | Some (n, v) -> 
        Hashtbl.replace t k (n+1, v);
        incr current_refcount;
        `Ok ()

  let decr k =
    Hashtbl.alter t k (function
      | None -> None
      | Some (1, v) -> 
	  current_size -= QueryResult.size v;
          decr current_refcount;
          None
      | Some (n, v) -> 
          decr current_refcount;
          Some (n-1, v))

  let find k = Hashtbl.find_opt t k

  let clear () =
    Hashtbl.clear t;
    current_size := 0;
    current_refcount := 0
end

module User = struct
  type t = int (* CRv2 jfuruse: something to distinguish users. IP? Cookie? *)
end

module TimedQuery : sig
  type t = {
    stamp   : float (** epoch *) * int (** id *);
    queries : key;
    size    : int;
  }
  val compare : t -> t -> int
end = struct
  type t = {
    stamp   : float * int;
    queries : key;
    size    : int;
  }
  let compare x y = compare x.stamp y.stamp
end

module UserHistory = struct
  module History = Set.Make(TimedQuery)
  type t = {
    user        : User.t;
    history     : History.t;
    total_size  : int;
    total_count : int;
  }
  let eldest t = try Some (History.min_elt t.history) with Not_found -> None
  let compare x y = compare (x.total_size, x.user) (y.total_size, y.user)
  let find qs t = find_by_iter History.iter (fun tq -> tq.TimedQuery.queries = qs) t.history
  let empty user = { 
    user;
    history     = History.empty;
    total_size  = 0;
    total_count = 0
  }

  let is_empty uh = History.is_empty uh.history
    
  let add tq t =
    match find tq.TimedQuery.queries t with
    | None -> 
        { user        = t.user;
          history     = History.add tq t.history;
          total_size  = t.total_size + tq.TimedQuery.size;
          total_count = t.total_count + 1
        }, true
    | Some tq' ->
        assert (tq.TimedQuery.size = tq'.TimedQuery.size);
        { user        = t.user;
          history     = History.(add tq (remove tq' t.history));
          total_size  = t.total_size;
          total_count = t.total_count;
        }, false

  let remove q t =
    match find q t with
    | None -> t
    | Some tq ->
        { user        = t.user;
          history     = History.remove tq t.history;
          total_size  = t.total_size - tq.TimedQuery.size;
          total_count = t.total_count - 1
        }
end

module TotalHistory = struct
  module AllHistory = Set.Make(UserHistory)

  let all_history = ref AllHistory.empty

  let clear () = all_history := AllHistory.empty

  let find_user user =
    find_by_iter AllHistory.iter (fun uh -> uh.UserHistory.user = user) !all_history

  let max_cache_size = 100000 (** How much result the server keep in the cache *)
    
  let rec shrink retry ah =
    if retry = 0 then `Error `Shrink_too_many
    else
    if Raw.get_current_size () < max_cache_size then `Ok ah
    else
      match 
        try Some (AllHistory.max_elt ah) with Not_found -> None
      with
      | None -> (* we can do nothing *)
          `Ok ah
      | Some uh ->
          match UserHistory.eldest uh with
          | None -> (* we can do nothing *)
              `Ok ah
          | Some tq ->
              let ah = AllHistory.remove uh ah in
              Raw.decr tq.TimedQuery.queries;
              let uh = UserHistory.remove tq.TimedQuery.queries uh in
              let ah = if not & UserHistory.is_empty uh then AllHistory.add uh ah else ah in
              shrink (retry - 1) ah

  let shrink () = 
    match shrink 10 !all_history with
    | `Ok ah -> all_history := ah; `Ok ()
    | `Error s -> `Error s

  let get_current_refcount () =
    AllHistory.fold (fun uh st -> uh.UserHistory.total_count + st) !all_history 0

  let check_refcount_invariant () = 
    if get_current_refcount () = Raw.get_current_refcount () then `Ok ()
    else `Error `Checksum_failure

  let query items stamp user qs =
    match qs with
    | None -> query items qs, false (* no caching for empty query *)
    | Some qs ->
        let make_tq res = 
            { TimedQuery.stamp;
              queries = qs;
              size    = QueryResult.size res }
        in
        let update_user f =
          let uh = match find_user user with 
            | Some uh -> uh
            | None -> UserHistory.empty user
          in
          let uh' = f uh in
          all_history := AllHistory.(add uh' (remove uh !all_history))
        in
        match Raw.find qs with
        | None -> 
            let res = query items (Some qs) in
            let tq = make_tq res in
            update_user (fun uh ->
              let uh, _incred = UserHistory.add tq uh in
              Raw.add_new qs res |> Result.from_Ok;
              uh);
            res, false
            
        | Some (_, res) -> 
            let tq = make_tq res in
            update_user (fun uh ->
              let uh, incred = UserHistory.add tq uh in
              if incred then Raw.incr qs |> Result.from_Ok;
              uh);
            res, true
end

let get_stamp = 
  let cntr = ref 0 in
  fun () ->
    incr cntr;
    Unix.gettimeofday (), !cntr

let print_state () =
  Format.eprintf "Cache: current_size=%d; current_refcount=%d; users=%d@." 
    (Raw.get_current_size ())
    (Raw.get_current_refcount ())
    (TotalHistory.AllHistory.cardinal !TotalHistory.all_history)
  
let clear () =
  Raw.clear ();
  TotalHistory.clear ()

let query items user qs =
  let stamp = get_stamp () in
  let res, hit = TotalHistory.query items stamp user qs in
  match TotalHistory.shrink () with
  | `Error e -> clear (); (res, `Error e)
  | `Ok _ ->
      match TotalHistory.check_refcount_invariant () with
      | `Error e -> clear (); (res, `Error e)
      | `Ok _ ->
          print_state ();
          res, `Ok hit

let search items user query_string = query items user & Query.parse query_string




