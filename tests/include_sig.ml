module type S = sig
  type t
  type t'
  val x : int
  val x' : int 
end

module type SS = sig
  include S
end
