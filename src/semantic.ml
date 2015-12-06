(* Static Semantic Check
   Input AST, output SAST *)
open Ast
open Sast
open Semantic_expr
open Printf

(* find the type of a @name in @var_table
   return: (true, dtype) on found, (false, _) on not_found *)
let find_var var_table name =
  try
    let t, _, _ = List.find (fun (_,b,_) -> b = name) var_table in
    true, t
  with Not_found -> false, Void


(* general type equality - int = float *)
let eq_t t1 t2 = match t1, t2 with
    Int, Float | Float, Int -> true
  | x, y -> if x=y then true else false

(* helper: generate Sast.func_sig of Sast.sfunc_decl *)
let sig_sfunc fdecl = {
  sig_fname = fdecl.s_fname;
  sig_formals = fdecl.s_formals
}

(* helper: check if a s_func_decl is declaration only *)
let is_func_dec fdecl = (fdecl.s_body=EmptyStmt)

(* helper: print function signature *)
let print_func_sig fsig =
  let rec formals_s = function
      [] -> ""
    | [a] -> string_of_datatype a
    | a::b::tl -> (sprintf "%s, %s" (string_of_datatype a) (formals_s (b::tl)))
  in
  eprintf "%s(%s)\n" fsig.sig_fname (formals_s fsig.sig_formals)

(* replace @o in List @lst with @n *)
let rec list_rep o n lst = match lst with
    [] -> []
  | hd::tl -> let a = (if hd=o then n else hd) in a::(list_rep o n tl)

(* find a function signature in function table
   arguments: @eq - the equal operator : can be (=) or eq_t
              @fnsg - function signature to be found
              @func_table - function table
   return: (true, sfun_def) on found, (false, _) on not_found *)
let find_func eq func_table fnsg =
  let dummy = {s_fname="_"; s_formals=[]; s_body = S_EmptyStmt; s_return_type=Void} in
  let func_eq f1 fd =
    let f2 = sig_sfunc fd in
    f1.sig_fname = f2.sig_fname &&
    try List.for_all2 eq f1.sig_formals f2.sig_formals
    with Invalid_argument _ -> false
  in
  try true, (List.find (func_eq fnsg) func_table)
  with Not_found -> false, dummy


(* variable default value,
   return a s_expr *)
let s_var_init_s_expr var = match var with
    Int -> Int, S_IntLit 0
  | Float -> Float, S_FloatLit 0.0
  | Bool -> Bool, S_BoolLit false
  | String -> String, S_StringLit ""
  | Void -> raise (Invalid_use "cannot define a void-type variable")

(* check expr,
   return a s_expr *)
let rec check_lvalue ftbl vtbl lv = match lv with
    Id x -> let f, t = find_var vtbl x in
    if f then t, (S_Id x)
    else raise (Invalid_use ("variable " ^ x ^ " not defined"))
and check_call ftbl vtbl fn exp_list =
  let s_exp_list = List.map (check_expr ftbl vtbl) exp_list in
  let typ_list = List.map (fst) s_exp_list in
  let found, fnsg = find_func eq_t ftbl {sig_fname=fn; sig_formals=typ_list} in
  if found && not (is_func_dec fnsg) then fnsg.s_return_type, S_FuncCall(fn, s_exp_list)
  else raise (Invalid_use ("function " ^ fn ^ " not defined"))
and check_expr ftbl vtbl exp = match exp with
    IntLit x -> Int, SIntval x
  | DoubleLit x -> Double, S_DoubleLit x
  | StringLit x -> String, S_StringLit x
  | BoolLit x -> Bool, S_BoolLit x
  | Lvalue lv -> check_lvalue ftbl vtbl lv
  | Binop(e1, bop, e2) -> check_binop bop
                            (check_expr ftbl vtbl e1)
                            (check_expr ftbl vtbl e2)
  | Unaop(uop, x) -> check_uniop uop (check_expr ftbl vtbl x)
  | Assign(lv, x) -> check_assign (check_lvalue ftbl vtbl lv)
                       (check_expr ftbl vtbl x)
  | FuncCall(fn, xl) -> check_call ftbl vtbl fn xl


(* check variable definition list,
   while building variable table
   return a svar_def list *)
let rec check_var_decls ftbl vtbl var_decls = match var_decls with
    [] -> vtbl
  | hd::tl -> let new_var_decl = (
    let new_v, init_e = (
      match hd with
        VarNoInit v -> v, (svar_init_sexpr v.vtype)
      | VarInit (v, e) -> v, (check_expr ftbl vtbl e))
    in
    let new_type, new_name = new_v.vtype, new_v.vname in
    let _ =
      let f, _ = find_var vtbl new_name in
      if not f then () else raise (Invalid_use (new_v.vname ^ " defined twice"))
    in
    let new_type =
      if eq_t new_type (fst init_e) then new_type
      else raise (Invalid_use "variable and expression type mismatch")
    in
    [(new_type, new_name, init_e)] )
    in
    (check_var_decls ftbl (vtbl@new_vardec) tl)

(* get svar locals from var list, check all var with incremental vtbl, but not returning a merged vtbl *)
let rec check_local_var_def ftbl vtbl local_var_list = match local_var_list with
    [] -> []
  | hd::tl -> let new_vardec = (
    let new_v, init_e = (
      match hd with
        VarNoInit v -> v, (svar_init_sexpr v.vtype)
      | VarInit (v, e) -> v, (check_expr ftbl vtbl e))
    in
    let new_type, new_name = new_v.vtype, new_v.vname in
    let _ =
      let f, _ = find_var vtbl new_name in
      if not f then () else raise (Invalid_use (new_v.vname ^ " defined twice"))
    in
    let new_type =
      if eq_t new_type (fst init_e) then new_type
      else raise (Invalid_use "variable and expression type mismatch")
    in
    [(new_type, new_name, init_e)] )
    in
    (new_vardec @ (check_local_var_def ftbl (new_vardec@vtbl) tl))


(* check statement list.
   return: sstmt list *)
let rec check_condstmts ftbl vtbl ret_type loop_flag cs = match cs with(* translate a list of elif *)
    [] -> []
  | hd::tl -> let _, sstmts = (check_stmts ftbl vtbl ret_type false false loop_flag hd.stmts) in
    { scond = (check_expr ftbl vtbl hd.cond) ;
      sstmts
    } :: (check_condstmts ftbl vtbl ret_type loop_flag tl )
and check_stmts ftbl vtbl ret_type main_flag ret_flag loop_flag stmts= match stmts with
    [] -> ret_flag,[]
  | hd::tl ->
    let flag0, flist0 =
      ( match hd with
          Empty -> ret_flag, SEmpty
        | Expr e -> ret_flag, SExpr (check_expr ftbl vtbl e)
        | Return e ->
          let ret = check_expr ftbl vtbl e in
          if fst ret == ret_type
          then (if (main_flag) then true,SReturn ret else false,SReturn ret)
          else raise (Invalid_use "mismatch with function's return type")
        | If (c, cl, ss) -> let _, check_ss = check_stmts ftbl vtbl ret_type false ret_flag loop_flag ss  in
          ret_flag, SIf ((List.hd (check_condstmts ftbl vtbl ret_type loop_flag [c] )),
                         (check_condstmts ftbl vtbl ret_type loop_flag cl),
                         check_ss
                        )
        | CntFor (s, e, ss) -> (
            let f, st = find_var vtbl s in
            let et, e = check_expr ftbl vtbl e in
            let _, sss = check_stmts ftbl vtbl ret_type false ret_flag true ss  in
            let _ = if f then () else raise (Invalid_use (s ^ "undefined")) in
            let et_t = match et with
                IntMat -> Int
              | DoubleMat -> Double
              | StringMat -> String
              | _ -> raise (Invalid_use "must be loop in a mat")
            in
            if eq_t et_t st then ret_flag, SCntFor (s, (et, e), sss)
            else raise (Invalid_use "loop variable type mismatch") )
        | CndFor cs -> ret_flag,SCndFor (List.hd (check_condstmts ftbl vtbl ret_type true [cs]))
        | Disp e -> ret_flag, SDisp (check_expr ftbl vtbl e)
        | Continue -> if(loop_flag) then ret_flag, SContinue else raise (Invalid_use "Continue should only be used inside a loop")
        | Break -> if(loop_flag) then ret_flag, SBreak else raise (Invalid_use "Break should only be used inside a loop")
      ) in
    let flag1, flist1 = check_stmts ftbl vtbl ret_type main_flag ret_flag loop_flag tl
    in flag0||flag1 , flist0::flist1


(* check_func_decl
     check function declaration
   arguments: Sast.s_func_decl list, Ast.func_decl
   return: Sast.s_func_decl list *)
let check_func_decl new_ftbl ftbl new_func_decl =
  let sig_func fn = {
    sig_fname = fn.fname;
    sig_formals = fn.formals
  } in
  let new_fnsg = sig_func new_func_decl in (* signature *)
  let new_ret = new_func_decl.return_type in (* return type *)
  (*let _ = print_func_sig new_fnsg in*)
  let new_fname = new_func_decl.fname in (* name *)
  let new_formals = new_func_decl.formals in (* arguments *)
  (* check local variables & build variable table *)
  let formal_decl = new_formals in
  let full_ftbl = ftbl @ new_ftbl in
  let vtbl = (arg_decl@new_local) in
  (* check statements *)
  let flag, new_fstmts = check_stmts full_ftbl vtbl new_ret true false false new_func_decl.body in
  let new_sfun_decl = { s_fname = new_fname;
                       s_formals = new_formals;
                       s_body = new_fstmts;
                       s_return_type = new_ret } in
  let _ = if (new_sret != Void && not (is_func_dec new_sfun_decl) && not flag)
    then raise (Invalid_use ("Function '" ^ new_sname ^ "' return statement missing"))
    else ()
  in
  let found, _ = find_func (=) (ftbl) new_fnsg in
  let foundnew, fbodynew = find_func (=) (new_ftbl) new_fnsg in
  (*let _ = eprintf "%s: %s" new_sname (if found then "found" else "not found") in*)
  match found, foundnew with
    false, false -> new_ftbl @ [new_sfun_decl]
  | false, true ->  if (is_func_dec fbodynew) && not (is_func_dec new_sfun_decl)
                    then begin
                      if fbodynew.sreturn = new_sret then
                        (list_rep fbodynew new_sfun_decl new_ftbl)
                      else
                        raise (Invalid_use ("Function '" ^ new_sname ^ "' return type different with declaration"))
                    end
                    else raise (Invalid_use ("Function '" ^ new_sname ^ "' already defined"))
  | true, _ -> raise (Invalid_use ("Function '" ^ new_sname ^ "' already defined"))


(* check function definition list
   input: func_decl list
   return: s_func_decl list *)
let rec check_func_decls new_ftbl ftbl funsgs = match funsgs with
    [] -> new_ftbl
  | hd::tl -> let new_ftbl = check_func_decl new_ftbl ftbl hd in
               check_func_decls new_ftbl ftbl tl


(* check the whole program
   returns: sprogram
   note: lib_funs is imported by default
 *)
let check need_dec_extern extern_funs prg =
  let func_table =
    let func_table_0 = extern_funs in (* init function table (should be built-in functions)
                                      and init new function table (user-defined & empty) *)
    check_func_decls [] func_table_0 prg.pfuns
  in
  let full_ftbl =  lib_funs @ extern_funs @ func_table in
  let var_table =
    let var_table_0 = [] in    (* init variable table as empty *)
    check_var_decls full_ftbl var_table_0 prg.pvars
  in
  let _, stm_lines =            (* statements *)
    check_stmts full_ftbl var_table Int true true false prg.pstms
  in
  match need_dec_extern with
    IMP -> { s_fdecls = func_table; s_gdecls = [] }
  | _ -> { s_fdecls = func_table; s_gdecls = var_table }
