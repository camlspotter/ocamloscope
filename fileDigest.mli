type tbl = (string, Digest.t Lazy.t * string) Hashtbl.t

val scan : ?exclude_dir: (string -> bool) ->  string list -> tbl

val find : tbl -> base:string -> digest:Digest.t -> string list
val find_by_base : tbl -> string -> string list
