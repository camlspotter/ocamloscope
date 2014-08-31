module M = struct
  let x = 1
end

module type S = sig
  val x : int
end

let m = (module M : S)

module N = (val m : S)

