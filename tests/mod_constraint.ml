module M = struct
  type t
  type t'
  let x = 1
  let x' = 1
end

module N = (M : sig type t val x : int end)

module O = (struct type t type t'' let x = 1 let x'' = 2 end : sig type t val x : int end)
