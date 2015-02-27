module Make(A : sig 
  val cache : Levenshtein.StringWithHashtbl.cache
end) : sig

  val error : bool ref (* for debug CR jfuruse: very dirty!*)

  val match_path_type :
    Spath.t * Stype.t
    -> Spath.t * Stype.t 
    -> int (** limit for path and type *)
    -> (int * (Spath.t * Stype.t)) option
  (** Returns distance, not score *)
  
  val match_type : (* ?no_target_type_instantiate:bool -> *) Stype.t -> Stype.t -> int -> (int * Stype.t) option
  (** Returns distance, not score *)
  
  val match_path : Spath.t -> Spath.t -> int -> (int * Spath.t) option
  (** Returns distance, not score *)

  val report_prof_type : unit -> unit
end

module MakePooled(A : sig
  val cache : Levenshtein.StringWithHashtbl.cache
  val pooled_types : Stype.t array
end) : sig

  val error : bool ref (* for debug CR jfuruse: very dirty!*)

  val match_path : Spath.t -> Spath.t -> int -> (int * Spath.t) option
  (** Returns distance, not score *)

  module WithType(T : sig
    val pattern : Stype.t
    val cache : [ `NotFoundWith of int | `Exact of int * Stype.t ] array
  end) : sig
    val match_type : Item.pooled_type -> int -> (int * Stype.t) option
    (** Returns distance, not score *)
    
    val match_path_type :
      Spath.t
      -> Spath.t * Item.pooled_type
      -> int (** limit for path and type *)
      -> (int * (Spath.t * Stype.t)) option
    (** Returns distance, not score *)
  end

  val report_prof_type : unit -> unit
end
 
