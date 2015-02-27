val db : Load.PooledDB.t

val source_available : bool array
(** [source_available.(i) tells whether [db.items.(i)] has the corresponding local source file *)
