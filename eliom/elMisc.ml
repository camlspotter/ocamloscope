(* -*- coding:utf-8 -*- *)

open Spotlib.Spot
open Eliom_content.Html5.F (* provides functions to create HTML nodes *)

let (!$) = pcdata
let (!@) = Xml.uri_of_string
let (!$%) fmt = Printf.ksprintf (!$) fmt

let p_class    c = p    ~a: [ a_class [c] ]
let div_class  c = div  ~a: [ a_class [c] ]
let span_class c = span ~a: [ a_class [c] ]
let pre_class  c = pre  ~a: [ a_class [c] ]
let td_class   c = td   ~a: [ a_class [c] ]

let the_name = "OCaml◉Scope"
let uri_logo = !@ "/images/logo.svg"
let uri_favicon = !@ "/images/favicon.ico"
let uri_stylesheet = !@ "/css/style.css"

let oco_head = 
  let x = 
    [ link ~rel:[`Stylesheet] ~href:uri_stylesheet ()
    ; link ~rel:[`Icon] ~href:uri_favicon ()    
    ; meta ~a:[ a_http_equiv "Content-Type"
              ; a_content "application/xhtml+xml; charset=UTF-8" 
              ] ()
       (* <link rel="shortcut icon" href="/favicon.ico" /> *)
    ; script ( cdata_script
"function toggleDisplay(id) {
   var obj=document.all && document.all(id) || document.getElementById && document.getElementById(id);
   if(obj && obj.style) obj.style.display = \"none\" == obj.style.display ? \"\" : \"none\";
}" )
    ]
    
  in
  head (title !$ the_name) x

let hlink_index es =
  a ~service:ElServices.Search.service es None

let oco_logo = h1 [ hlink_index [ img ~src:uri_logo ~alt:the_name () ] ]

let oco_form value = 
  get_form ~service: ElServices.Search.service & fun (q,_) -> 
    [string_input ~input_type:`Text ~name:q ?value ()
    ]

let issues =
  let service = 
    let open Eliom_parameter in
    Eliom_service.external_service 
      ~prefix: "https://bitbucket.org"
      ~path: [ "camlspotter"; "ocamloscope-server"; "issues" ]
      ~get_params: (string "status" ** string "status")
      ()
  in
  a ~service [ !$ "Issues" ] ("new", "open")

let triangle = !$ "▼"

let toggle_display id xs =
  (* Not serviced <a> but normal <a> *)
  Eliom_content_core.Html5.F.a 
    ~a:[ a_class ["toggleDisplay"]
       ; a_href (Xml.uri_of_string (!% "javascript:toggleDisplay('%s')" id)) ] 
    xs 

