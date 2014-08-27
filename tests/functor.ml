module Make(S : sig val x : int end) = struct
  let y = S.x
end
