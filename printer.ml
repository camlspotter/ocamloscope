module Attr = struct

  (** {6 Attributes for better printing and hyperlinks } *)

  include Attr

  let format internal ppf = function
    | `Bold -> Format.fprintf ppf "<<%t>>" internal

  let buffer internal buf = function
    | `Bold -> 
        Buffer.add_string buf "<<";
        internal buf;
        Buffer.add_string buf ">>"

end

include Treeprint.Aprinter.Make(Attr)

    
