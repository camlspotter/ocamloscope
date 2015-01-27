open Spotlib.Spot
open Stype_print
open List

let sanitize = {s|\s+/ /g|s} *> {s|\s+;/;/g|s}

let read_show_read s =
  match read s with
  | `Error e ->
      !!% "read: %S: %s@." s e;
      assert false
  | `Ok ty ->
      let s' = show ty in

      let sfix = sanitize s in
      let s'fix = sanitize s' in

      if sfix <> s'fix then begin
        !!% "read => show:@.  Input =%S@.  Output=%S@.  FixI=%S@.  FixO=%S@.  Parsed=%a@."
            s
            s'
            sfix
            s'fix
            Stype.oformat ty;
          assert false
      end;

      match read s' with
      | `Error e ->
          !!% "read => show => read: %s@.  Input= %S@.  Output=%S@.  Parsed=%a@."
            e
            s
            s'
            Stype.oformat ty;
          assert false
      | `Ok ty' ->
          let ty = Stype.rec_hcons ty in
          let ty' = Stype.rec_hcons ty' in
          if ty <> ty' then begin
            !!% "read => show => read: not equal@.  Input=  %S@.  Output= %s@.  Input=  %a@.  Output= %a@."
              s 
              s' 
              Stype.oformat ty
              Stype.oformat ty';
            assert false
          end;
          if ty == ty' then begin
            let ty'' = 
              let s = Marshal.to_string ty [] in
              Stype.rec_hcons & Marshal.from_string s 0
            in
            if ty'' == ty' then begin
              ()
            end else begin
              !!% "read => show => read + Marshal:@.  Input=  %s@.  Output= %s@." s s';
              assert false
            end
          end else begin
            !!% "read => show => read: hcons@.  Input=  %s@.  Output= %s@.  Input=  %a@." 
              s 
              s'
              Stype.oformat ty;
            assert false
          end

let read_show_read_show ty = read_show_read & show ty

let test_read_show = read_show_read *< show

let test_printer_compatibility ty =
  let s2 = Format.to_string (Stype.format_gen (Spath.print ~predef:true)) ty in
  let s2' = sanitize s2 in
  try
    let s1 = Stype.to_string_via_type_expr ty in (* CR jfuruse: It does print #n and *predef* *)
    let s1' = sanitize s1 in
    if s1' <> s2' then begin
      !!% "test_printer_compatibility:@.  ORG:   %S@.  XTYPE: %S@.  %a@."
        s1'
        s2'
        Stype.oformat ty;
      assert false
    end;

    match
      try Type_expr.read s2 with 
      | Typetexp.Error (_loc, env, err) -> 
          begin try
            `Error (Format.to_string (Typetexp.report_error env) err)
          with
          | Not_found -> `Error "not found"
          end
      | Ctype.Unify trace ->
          `Error (Format.to_string (Typetexp.report_error Env.initial_unsafe_string) (Typetexp.Type_mismatch trace))
    with
    | `Error _s -> 
        (* !!% "skip: type_expr: %s: %S@." s s2' *)
        ()
    | `Ok type_expr ->
        let ty' = Stype.of_type_expr Spath.of_path type_expr in
        let s4 = sanitize & Stype.to_string ty' in
        let s5 = sanitize & Stype.to_string_via_type_expr ty' in
        if s4 <> s5 then begin
          !!% "test_printer_compatibility TYPE_EXPR:@.  ORG:   %S@.  XTYPE: %S@.  %a@."
            s4
            s5
            Stype.oformat ty;
          assert false
        end
  with
  | Stype_core.Unsupported -> 
      (* Stype.Org printer cannot print Variant inherit *)
      !!% "skip: Stype.format_via_type_expr cannot handle %s@." s2'

(* Test failures which are hard to fix (or too complex to investigate further) *)
let bad_guys = [
]

open Ppx_orakuda.Std
open Regexp.Infix

let test items n =
  let item = items.(n) in
  let p = Spath.show item.Item.path in
  if List.exists (fun r -> (p =~ r) <> None) bad_guys then
    !!% "Skipping %s@." p
  else
      try
        let tys = Item.types_of_kind item.Item.kind in
        iter (fun ty -> 
          if not & Stype_print.cannot_read ty then read_show_read_show ty;
          test_printer_compatibility ty) tys
      with
      | e ->
          !!% "ERROR at item #%d@." n;
          !!% "  @[%a@]@." Item.format item;
          raise e

let test items =

  let path = "tests/types.txt" in
  if File.Test._e path then File.iter_lines_exn path (fun l ->
    test_printer_compatibility & Result.from_Ok & Stype.read l;
    read_show_read l);

  !!% "tests/types.txt done@.";

  let ixs = Array.(init (length items) id) in
  let size_of_item i = 
    fold_left (+) 0 & map Stype.size_type (Item.types_of_kind i.Item.kind)
  in
  Array.sort (fun i1 i2 ->
    compare (size_of_item items.(i1)) (size_of_item items.(i2))) ixs;

  Array.iter (test items) ixs
