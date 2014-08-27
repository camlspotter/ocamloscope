module X = struct
  type t
  type t'
  let x = 1
  let x' = 1
end

include (X : sig type t val x : int end)

