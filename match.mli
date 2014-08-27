module Make(A : sig 
  val cache : Levenshtein.StringWithCache.cache
end) : sig

  val error : bool ref (* for debug CR jfuruse: very dirty!*)

  val match_path_type :
    Spath.t * Stype.t
    -> Spath.t * Stype.t 
    -> int (** path limit *)
    -> int (** type limit *)
    -> (int * (Spath.t * Stype.t)) option
  (** Returns distance, not score *)
  
  val match_type : ?no_target_type_instantiate:bool -> Stype.t -> Stype.t -> int -> (int * Stype.t) option
  (** Returns distance, not score *)
  
  val match_path : Spath.t -> Spath.t -> int -> (int * Spath.t) option
  (** Returns distance, not score *)
end
