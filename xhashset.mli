type 'a t
val create : ?random:bool -> int -> 'a t
val clear : 'a t -> unit
val reset : 'a t -> unit
val copy : 'a t -> 'a t
val add : 'a t -> 'a -> unit
val find : 'a t -> 'a -> 'a
val find_all : 'a t -> 'a -> 'a list
val mem : 'a t -> 'a -> bool
val remove : 'a t -> 'a -> unit
val replace : 'a t -> 'a -> unit
val iter : ('a -> unit) -> 'a t -> unit
val fold : ('a -> 'c -> 'c) -> 'a t -> 'c -> 'c
val length : 'a t -> int
val randomize : unit -> unit
val find_or_add : 'a t -> 'a -> 'a

type statistics = {
  num_bindings: int;
    (** Number of bindings present in the table.
        Same value as returned by {!Hashtbl.length}. *)
  num_buckets: int;
    (** Number of buckets in the table. *)
  max_bucket_length: int;
    (** Maximal number of bindings per bucket. *)
  bucket_histogram: int array
    (** Histogram of bucket sizes.  This array [histo] has
        length [max_bucket_length + 1].  The value of
        [histo.(i)] is the number of buckets whose size is [i]. *)
}

val stats : 'a t -> statistics
(** [Hashtbl.stats tbl] returns statistics about the table [tbl]:
   number of buckets, size of the biggest bucket, distribution of
   buckets by size.
   @since 4.00.0 *)

(** {6 Functorial interface} *)

module type HashedType =
  sig
    type t
      (** The type of the hashtable keys. *)
    val equal : t -> t -> bool
      (** The equality predicate used to compare keys. *)
    val hash : t -> int
      (** A hashing function on keys. It must be such that if two keys are
          equal according to [equal], then they have identical hash values
          as computed by [hash].
          Examples: suitable ([equal], [hash]) pairs for arbitrary key
          types include
-         ([(=)], {!Hashtbl.hash}) for comparing objects by structure
              (provided objects do not contain floats)
-         ([(fun x y -> compare x y = 0)], {!Hashtbl.hash})
              for comparing objects by structure
              and handling {!Pervasives.nan} correctly
-         ([(==)], {!Hashtbl.hash}) for comparing objects by physical
              equality (e.g. for mutable or cyclic objects). *)
   end
(** The input signature of the functor {!Hashtbl.Make}. *)

module type S =
  sig
    type key
    type t
    val create : int -> t
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
(** The output signature of the functor {!Hashtbl.Make}. *)

module Make (H : HashedType) : S with type key = H.t
(** Functor building an implementation of the hashtable structure.
    The functor [Hashtbl.Make] returns a structure containing
    a type [key] of keys and a type ['a t] of hash tables
    associating data of type ['a] to keys of type [key].
    The operations perform similarly to those of the generic
    interface, but use the hashing and equality functions
    specified in the functor argument [H] instead of generic
    equality and hashing.  Since the hash function is not seeded,
    the [create] operation of the result structure always returns
    non-randomized hash tables. *)

module type SeededHashedType =
  sig
    type t
      (** The type of the hashtable keys. *)
    val equal: t -> t -> bool
      (** The equality predicate used to compare keys. *)
    val hash: int -> t -> int
      (** A seeded hashing function on keys.  The first argument is
          the seed.  It must be the case that if [equal x y] is true,
          then [hash seed x = hash seed y] for any value of [seed].
          A suitable choice for [hash] is the function {!Hashtbl.seeded_hash}
          below. *)
  end
(** The input signature of the functor {!Hashtbl.MakeSeeded}.
    @since 4.00.0 *)

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
(** The output signature of the functor {!Hashtbl.MakeSeeded}.
    @since 4.00.0 *)

module MakeSeeded (H : SeededHashedType) : SeededS with type key = H.t
(** Functor building an implementation of the hashtable structure.
    The functor [Hashtbl.MakeSeeded] returns a structure containing
    a type [key] of keys and a type ['a t] of hash tables
    associating data of type ['a] to keys of type [key].
    The operations perform similarly to those of the generic
    interface, but use the seeded hashing and equality functions
    specified in the functor argument [H] instead of generic
    equality and hashing.  The [create] operation of the
    result structure supports the [~random] optional parameter
    and returns randomized hash tables if [~random:true] is passed
    or if randomization is globally on (see {!Hashtbl.randomize}).
    @since 4.00.0 *)


(** {6 The polymorphic hash functions} *)


val hash : 'a -> int
(** [Hashtbl.hash x] associates a nonnegative integer to any value of
   any type. It is guaranteed that
   if [x = y] or [Pervasives.compare x y = 0], then [hash x = hash y].
   Moreover, [hash] always terminates, even on cyclic structures. *)

val seeded_hash : int -> 'a -> int
(** A variant of {!Hashtbl.hash} that is further parameterized by
   an integer seed.
   @since 4.00.0 *)

val hash_param : int -> int -> 'a -> int
(** [Hashtbl.hash_param meaningful total x] computes a hash value for [x],
   with the same properties as for [hash]. The two extra integer
   parameters [meaningful] and [total] give more precise control over
   hashing. Hashing performs a breadth-first, left-to-right traversal
   of the structure [x], stopping after [meaningful] meaningful nodes
   were encountered, or [total] nodes (meaningful or not) were
   encountered. Meaningful nodes are: integers; floating-point
   numbers; strings; characters; booleans; and constant
   constructors. Larger values of [meaningful] and [total] means that
   more nodes are taken into account to compute the final hash value,
   and therefore collisions are less likely to happen.  However,
   hashing takes longer. The parameters [meaningful] and [total]
   govern the tradeoff between accuracy and speed.  As default
   choices, {!Hashtbl.hash} and {!Hashtbl.seeded_hash} take
   [meaningful = 10] and [total = 100]. *)

val seeded_hash_param : int -> int -> int -> 'a -> int
(** A variant of {!Hashtbl.hash_param} that is further parameterized by
   an integer seed.  Usage:
   [Hashtbl.seeded_hash_param meaningful total seed x].
   @since 4.00.0 *)
