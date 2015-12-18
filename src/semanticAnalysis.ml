open Ast
open Sast

module StringMap = Map.Make(String)

exception Main_not_found



let s_check_func_decl f_context v_context fdecl =
    let s_formals_t = List.map (fun var_decl -> s_check_var_decl f_context v_context
    var_decl) fdecl.formals in
    let s_ret_t = s_check_var_type f_context v_context fdecl.return_type in
    {s_fname = fdecl.fname;
     s_formals = s_formals_t;
     s_return_type = s_ret_t;
     s_body = s_check_stmt (StringMap.add "0current" [(List.map (fun a ->
         a.s_vtype) s_formals_t,s_ret_t)] f_context) (List.fold_left (fun a l ->
             if StringMap.mem l.s_vname a && snd (StringMap.find l.s_vname a) == 1 then 
                 raise Dup_var_id
             else
                 StringMap.add l.s_vname l.s_vtype a) v_context s_formals_t) 1 fdecl.body
    }

let rec s_check_func_decls f_context v_context func_decl_list = match func_decl_list with
     [] -> []
   | hd::tl ->  
      (s_check_func_decl f_context v_context hd) :: (s_check_func_decls f_context v_context tl)

let s_var_decl_to_var_map map s_vdecl = 
    if StringMap.mem s_vdecl.s_vname map then
        raise Dup_var_id
    else
        StringMap.add s_vdecl.s_vname s_vdecl.s_vtype map

let is_s_var_type_equal t1 t2 = 
    if t1 == t2 then
        true
    else
        false

let rec is_s_var_type_list_equal l1 l2 = match l1, l2 with
    [], [] -> true
   | [], hd::tl -> false
   | hd::tl, [] -> false
   | h1::t1, h2::t2 -> 
           if is_s_var_type_equal h1 h2 then
               is_s_var_type_list_equal t1 t2
           else
               false

let func_decl_check_func_map s_var_type_list_list s_var_type_list =
    let check = List.map (fun l1 -> is_s_var_type_list_equal (fst l1) s_var_type_list)
    s_var_type_list_list in
    if List.length (List.filter (fun a -> a) check) != 0 then
        true
    else
        false

let func_decl_to_func_map map fdecl v_context =
    let f_s_var_type_list = List.map (fun a -> a.s_vtype) (List.map (fun a -> s_check_var_decl StringMap.empty v_context a) fdecl.formals) in 
    if StringMap.mem fdecl.fname map 
    then 
        if func_decl_check_func_map (StringMap.find fdecl.fname map) f_s_var_type_list 
        then
            raise Func_duplicate
        else
            StringMap.add fdecl.fname ((f_s_var_type_list, s_check_var_type StringMap.empty v_context fdecl.return_type)::StringMap.find fdecl.fname map) map
    else
        StringMap.add fdecl.fname [(f_s_var_type_list, s_check_var_type StringMap.empty v_context fdecl.return_type)] map

let s_check_program prog =
	let temp_s_gdecls = List.map (fun var_decl -> s_check_var_decl StringMap.empty StringMap.empty var_decl) prog.gdecls
	and std_func = (
		(* TODO: add custom functions *)
		StringMap.empty
	) in
{
	s_gdecls = temp_s_gdecls;
	s_fdecls =
		let v_context = List.fold_left s_var_decl_to_var_map StringMap.empty temp_s_gdecls in
		let f_context = List.fold_left (fun x y -> func_decl_to_func_map x y v_context) std_func prog.fdecls in
    if StringMap.mem "main" f_context then s_check_func_decls f_context v_context prog.fdecls
    else raise Main_not_found
}