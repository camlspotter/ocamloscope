module M = struct
  let x = []
end

module N = (M : sig val x : int list end)
