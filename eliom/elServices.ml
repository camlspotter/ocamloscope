open Spotlib.Spot
open Eliom_content.Html5.F (* provides functions to create HTML nodes *)

module Search = struct
  let param = 
    let open Eliom_parameter in
    opt & (string "q" ** opt (int "p"))
  
  let service = Eliom_service.service 
    ~path:[""]
    ~get_params:param 
    ()

  let a = a ~service
end

module Packages = struct
  let service = Eliom_service.service 
    ~path:["packages"]
    ~get_params:Eliom_parameter.unit
    ()

  let a = a ~service
end

module Package = struct
  let param = 
    let open Eliom_parameter in
    string "name"
  
  let service = Eliom_service.service 
    ~path:["package"]
    ~get_params:param 
    ()

  let a = a ~service
end

module Source = struct
  let param = 
    let open Eliom_parameter in
    string "path" ** string "md5" ** int "line"

  let service = Eliom_service.service 
    ~path:["source"]
    ~get_params:param
    ()

  let a = a ~service
end

module Log = struct
  let param = 
    let open Eliom_parameter in
    opt & string "q"
  
  let service = Eliom_service.service 
    ~path:["logloglog"] (* CR jfuruse: This must be configurable, or the service itself must be protected *)
    ~get_params:param 
    ()

  let a = a ~service
end

module Bereth = struct
  let param = 
    let open Eliom_parameter in
    string "q"
  
  let service = Eliom_service.service 
    ~path:["elbereth"] (* CR jfuruse: This must be configurable *)
    ~get_params:param 
    ()

  let a = a ~service
end

(*
module Ping = struct
  let name = ElConfig.config.ElConfig.ping_path

  let service = Eliom_service.service 
    ~path:[name] (* CR jfuruse: This must be configurable, or the service itself must be protected *)
    ~get_params:Eliom_parameter.unit
    ()

  let a = a ~service
end
*)
