(* This should be ported / already ported to spotlib *)

module Exn = struct
  (* lwt does not like ~finally *)
  let protect f v ~final = Spotlib.Exn.protect f v ~finally:final
end

module Array = struct

  let shuffle ?(random=Random.int) a =
    let len = Array.length a - 1 in
    for i = 0 to len - 1 do
      let d = len - i in
      let j = random d + i in
      let x = Array.unsafe_get a i in
      let y = Array.unsafe_get a j in
      Array.unsafe_set a i y;
      Array.unsafe_set a j x;
    done

end

module Base = struct
  let ( !++ ) r = 
    let v = !r in
    incr r;
    v
end

module Gc = struct
  open Gc

  let used_words () =
    let c = get () in
    set { c with minor_heap_size = 1000; (* 1k words = 8k bytes *) };
    compact ();
    let s = stat () in
    let res = s.live_words in
    set c;
    res

  let with_big_compacts f v =
    let used_before = used_words () in

    Spotlib.Spot.Exn.protect_with f v
      ~finally:(fun _ ->
        let used_after = used_words () in
        used_before, used_after)

end
