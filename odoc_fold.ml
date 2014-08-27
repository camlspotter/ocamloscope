open Odoc_type
open Odoc_value
open Odoc_class
open Odoc_exception
open Odoc_module

class virtual ['st] ofold_ref_kind =
  object (self)
    method virtual option :
      'a1. ('st -> 'a1 -> 'st) -> 'st -> 'a1 option -> 'st
    method virtual list : 'a1. ('st -> 'a1 -> 'st) -> 'st -> 'a1 list -> 'st
    method variant_constructor : 'st -> variant_constructor -> 'st =
      fun __st __value ->
        let __st = self#list (fun __st _ -> __st) __st __value.vc_args in
        let __st = self#option (fun __st _ -> __st) __st __value.vc_ret in
        let __st = self#option (fun __st _ -> __st) __st __value.vc_text
        in __st
    method record_field : 'st -> record_field -> 'st =
      fun __st __value ->
        let __st = self#option (fun __st _ -> __st) __st __value.rf_text
        in __st
    method t_type : 'st -> t_type -> 'st =
      fun __st __value ->
        let __st = self#option (fun __st _ -> __st) __st __value.ty_info in
        let __st =
          self#list (fun __st (__tup1, __tup2, __tup3) -> __st) __st
            __value.ty_parameters in
        let __st =
          self#option (fun __st _ -> __st) __st __value.ty_manifest in
        let __st = self#option (fun __st _ -> __st) __st __value.ty_code
        in __st
    method t_value : 'st -> t_value -> 'st =
      fun __st __value ->
        let __st = self#option (fun __st _ -> __st) __st __value.val_info in
        let __st =
          self#list (fun __st _ -> __st) __st __value.val_parameters in
        let __st = self#option (fun __st _ -> __st) __st __value.val_code
        in __st
    method t_attribute : 'st -> t_attribute -> 'st =
      fun __st __value ->
        let __st = self#t_value __st __value.att_value in __st
    method t_method : 'st -> t_method -> 'st =
      fun __st __value ->
        let __st = self#t_value __st __value.met_value in __st
    method class_element : 'st -> class_element -> 'st =
      fun __st __value ->
        match __value with
        | Class_attribute __x1 ->
            let __st = self#t_attribute __st __x1 in __st
        | Class_method __x1 -> let __st = self#t_method __st __x1 in __st
        | Class_comment __x1 -> __st
    method cct : 'st -> cct -> 'st =
      fun __st __value ->
        match __value with
        | Cl __x1 -> let __st = self#t_class __st __x1 in __st
        | Cltype (__x1, __x2) ->
            let __st = self#t_class_type __st __x1 in
            let __st = self#list (fun __st _ -> __st) __st __x2 in __st
    method inherited_class : 'st -> inherited_class -> 'st =
      fun __st __value ->
        let __st = self#option self#cct __st __value.ic_class in
        let __st = self#option (fun __st _ -> __st) __st __value.ic_text
        in __st
    method class_apply : 'st -> class_apply -> 'st =
      fun __st __value ->
        let __st = self#option self#t_class __st __value.capp_class in
        let __st = self#list (fun __st _ -> __st) __st __value.capp_params in
        let __st =
          self#list (fun __st _ -> __st) __st __value.capp_params_code
        in __st
    method class_constr : 'st -> class_constr -> 'st =
      fun __st __value ->
        let __st = self#option self#cct __st __value.cco_class in
        let __st =
          self#list (fun __st _ -> __st) __st __value.cco_type_parameters
        in __st
    method class_kind : 'st -> class_kind -> 'st =
      fun __st __value ->
        match __value with
        | Class_structure (__x1, __x2) ->
            let __st = self#list self#inherited_class __st __x1 in
            let __st = self#list self#class_element __st __x2 in __st
        | Class_apply __x1 -> let __st = self#class_apply __st __x1 in __st
        | Class_constr __x1 -> let __st = self#class_constr __st __x1 in __st
        | Class_constraint (__x1, __x2) ->
            let __st = self#class_kind __st __x1 in
            let __st = self#class_type_kind __st __x2 in __st
    method t_class : 'st -> t_class -> 'st =
      fun __st __value ->
        let __st = self#option (fun __st _ -> __st) __st __value.cl_info in
        let __st =
          self#list (fun __st _ -> __st) __st __value.cl_type_parameters in
        let __st = self#class_kind __st __value.cl_kind in
        let __st = self#list (fun __st _ -> __st) __st __value.cl_parameters
        in __st
    method class_type_alias : 'st -> class_type_alias -> 'st =
      fun __st __value ->
        let __st = self#option self#cct __st __value.cta_class in
        let __st =
          self#list (fun __st _ -> __st) __st __value.cta_type_parameters
        in __st
    method class_type_kind : 'st -> class_type_kind -> 'st =
      fun __st __value ->
        match __value with
        | Class_signature (__x1, __x2) ->
            let __st = self#list self#inherited_class __st __x1 in
            let __st = self#list self#class_element __st __x2 in __st
        | Class_type __x1 ->
            let __st = self#class_type_alias __st __x1 in __st
    method t_class_type : 'st -> t_class_type -> 'st =
      fun __st __value ->
        let __st = self#option (fun __st _ -> __st) __st __value.clt_info in
        let __st =
          self#list (fun __st _ -> __st) __st __value.clt_type_parameters in
        let __st = self#class_type_kind __st __value.clt_kind in __st
    method exception_alias : 'st -> exception_alias -> 'st =
      fun __st __value ->
        let __st = self#option self#t_exception __st __value.ea_ex in __st
    method t_exception : 'st -> t_exception -> 'st =
      fun __st __value ->
        let __st = self#option (fun __st _ -> __st) __st __value.ex_info in
        let __st = self#list (fun __st _ -> __st) __st __value.ex_args in
        let __st = self#option self#exception_alias __st __value.ex_alias in
        let __st = self#option (fun __st _ -> __st) __st __value.ex_code
        in __st
    method module_element : 'st -> module_element -> 'st =
      fun __st __value ->
        match __value with
        | Element_module __x1 -> let __st = self#t_module __st __x1 in __st
        | Element_module_type __x1 ->
            let __st = self#t_module_type __st __x1 in __st
        | Element_included_module __x1 ->
            let __st = self#included_module __st __x1 in __st
        | Element_class __x1 -> let __st = self#t_class __st __x1 in __st
        | Element_class_type __x1 ->
            let __st = self#t_class_type __st __x1 in __st
        | Element_value __x1 -> let __st = self#t_value __st __x1 in __st
        | Element_exception __x1 ->
            let __st = self#t_exception __st __x1 in __st
        | Element_type __x1 -> let __st = self#t_type __st __x1 in __st
        | Element_module_comment __x1 -> __st
    method mmt : 'st -> mmt -> 'st =
      fun __st __value ->
        match __value with
        | Mod __x1 -> let __st = self#t_module __st __x1 in __st
        | Modtype __x1 -> let __st = self#t_module_type __st __x1 in __st
    method included_module : 'st -> included_module -> 'st =
      fun __st __value ->
        let __st = self#option self#mmt __st __value.im_module in
        let __st = self#option (fun __st _ -> __st) __st __value.im_info
        in __st
    method module_alias : 'st -> module_alias -> 'st =
      fun __st __value ->
        let __st = self#option self#mmt __st __value.ma_module in __st
    method module_parameter : 'st -> module_parameter -> 'st =
      fun __st __value ->
        let __st = self#module_type_kind __st __value.mp_kind in __st
    method module_kind : 'st -> module_kind -> 'st =
      fun __st __value ->
        match __value with
        | Module_struct __x1 ->
            let __st = self#list self#module_element __st __x1 in __st
        | Module_alias __x1 -> let __st = self#module_alias __st __x1 in __st
        | Module_functor (__x1, __x2) ->
            let __st = self#module_parameter __st __x1 in
            let __st = self#module_kind __st __x2 in __st
        | Module_apply (__x1, __x2) ->
            let __st = self#module_kind __st __x1 in
            let __st = self#module_kind __st __x2 in __st
        | Module_with (__x1, __x2) ->
            let __st = self#module_type_kind __st __x1 in __st
        | Module_constraint (__x1, __x2) ->
            let __st = self#module_kind __st __x1 in
            let __st = self#module_type_kind __st __x2 in __st
        | Module_typeof __x1 -> __st
        | Module_unpack (__x1, __x2) ->
            let __st = self#module_type_alias __st __x2 in __st
    method t_module : 'st -> t_module -> 'st =
      fun __st __value ->
        let __st = self#option (fun __st _ -> __st) __st __value.m_info in
        let __st = self#module_kind __st __value.m_kind in
        let __st = self#list (fun __st _ -> __st) __st __value.m_top_deps in
        let __st = self#option (fun __st _ -> __st) __st __value.m_code in
        let __st = self#option (fun __st _ -> __st) __st __value.m_code_intf
        in __st
    method module_type_alias : 'st -> module_type_alias -> 'st =
      fun __st __value ->
        let __st = self#option self#t_module_type __st __value.mta_module
        in __st
    method module_type_kind : 'st -> module_type_kind -> 'st =
      fun __st __value ->
        match __value with
        | Module_type_struct __x1 ->
            let __st = self#list self#module_element __st __x1 in __st
        | Module_type_functor (__x1, __x2) ->
            let __st = self#module_parameter __st __x1 in
            let __st = self#module_type_kind __st __x2 in __st
        | Module_type_alias __x1 ->
            let __st = self#module_type_alias __st __x1 in __st
        | Module_type_with (__x1, __x2) ->
            let __st = self#module_type_kind __st __x1 in __st
        | Module_type_typeof __x1 -> __st
    method t_module_type : 'st -> t_module_type -> 'st =
      fun __st __value ->
        let __st = self#option (fun __st _ -> __st) __st __value.mt_info in
        let __st = self#option (fun __st _ -> __st) __st __value.mt_type in
        let __st = self#option self#module_type_kind __st __value.mt_kind
        in __st
  end
and virtual ['st] ofold_text_element = ['st] ofold_ref_kind
and virtual ['st] ofold_text = ['st] ofold_ref_kind
and virtual ['st] ofold_see_ref = ['st] ofold_ref_kind
and virtual ['st] ofold_see = ['st] ofold_ref_kind
and virtual ['st] ofold_param = ['st] ofold_ref_kind
and virtual ['st] ofold_raised_exception = ['st] ofold_ref_kind
and virtual ['st] ofold_info = ['st] ofold_ref_kind
and virtual ['st] ofold_location = ['st] ofold_ref_kind
and virtual ['st] ofold_simple_name = ['st] ofold_ref_kind
and virtual ['st] ofold_param_info = ['st] ofold_ref_kind
and virtual ['st] ofold_parameter = ['st] ofold_ref_kind
and virtual ['st] ofold_private_flag = ['st] ofold_ref_kind
and virtual ['st] ofold_variant_constructor = ['st] ofold_ref_kind
and virtual ['st] ofold_record_field = ['st] ofold_ref_kind
and virtual ['st] ofold_type_kind = ['st] ofold_ref_kind
and virtual ['st] ofold_t_type = ['st] ofold_ref_kind
and virtual ['st] ofold_t_value = ['st] ofold_ref_kind
and virtual ['st] ofold_t_attribute = ['st] ofold_ref_kind
and virtual ['st] ofold_t_method = ['st] ofold_ref_kind
and virtual ['st] ofold_class_element = ['st] ofold_ref_kind
and virtual ['st] ofold_cct = ['st] ofold_ref_kind
and virtual ['st] ofold_inherited_class = ['st] ofold_ref_kind
and virtual ['st] ofold_class_apply = ['st] ofold_ref_kind
and virtual ['st] ofold_class_constr = ['st] ofold_ref_kind
and virtual ['st] ofold_class_kind = ['st] ofold_ref_kind
and virtual ['st] ofold_t_class = ['st] ofold_ref_kind
and virtual ['st] ofold_class_type_alias = ['st] ofold_ref_kind
and virtual ['st] ofold_class_type_kind = ['st] ofold_ref_kind
and virtual ['st] ofold_t_class_type = ['st] ofold_ref_kind
and virtual ['st] ofold_exception_alias = ['st] ofold_ref_kind
and virtual ['st] ofold_t_exception = ['st] ofold_ref_kind
and virtual ['st] ofold_module_element = ['st] ofold_ref_kind
and virtual ['st] ofold_mmt = ['st] ofold_ref_kind
and virtual ['st] ofold_included_module = ['st] ofold_ref_kind
and virtual ['st] ofold_module_alias = ['st] ofold_ref_kind
and virtual ['st] ofold_module_parameter = ['st] ofold_ref_kind
and virtual ['st] ofold_module_kind = ['st] ofold_ref_kind
and virtual ['st] ofold_t_module = ['st] ofold_ref_kind
and virtual ['st] ofold_module_type_alias = ['st] ofold_ref_kind
and virtual ['st] ofold_module_type_kind = ['st] ofold_ref_kind
and virtual ['st] ofold_t_module_type = ['st] ofold_ref_kind
  

