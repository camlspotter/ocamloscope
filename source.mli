val scan : string list -> unit
(** build the source file db *)

val find : string -> digest:string -> string option
(** find the file of the given base and md5sum in the db.
    It assures the file currently really exists with the same md5
 *)
