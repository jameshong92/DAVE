(* Translate from SAST to TAST*)
open Ast
open Sast
open Tast

(*Translate Expression*)
let rec translateExpr expr = match expr with
  S_Id (x)  -> IId (x)
| S_Intlit (x) -> IIntlit (x)
| S_Floatlit (x) -> IFloatlit (x)
| S_Stringlit (x) -> IStringlit (x)
| S_Boollit (x) -> IBoollit (x)
| S_Array (el) -> (let iel = translateArglist el in
				   IArray(el))
| S_Access (e1, x) -> (let ie1 = translateExpr e1 in 
					   IAccess(ie1,x))
| S_Index (x, e1) -> (let ie1 = translateExpr e1 in 
					 IIndex(x,ie1))
| S_Range (e1,e2) -> (let ie1 = translateExpr e1 in
					  let ie2 = translateExpr e2 in
					  IRange(ie1,ie2))
| S_Binop (e1, binop, e2) -> (let ie1 = translateExpr e1 in
							  let ie2 = translateExpr e2 in
							  IBinop(ie1,binop,ie2))
| S_Unop (unop, e1) -> (let ie1 = translateExpr e1 in
					    IUnop(ie1))
| S_Postop (e1, postop) -> (let ie1 = translateExpr e1 in
						    IPostop(ie1,postop))
| S_Assignop (e1, asnop, e2) -> (let ie1 = translateExpr e1 in
							 	 let ie2 = translateExpr e2 in
							 	 IAssignOp(ie1,binop,ie2))
| S_Assign (e1, e2) -> (let ie1 = translateExpr e1 in
						let ie2 = translateExpr e2 in
						IAssign(ie1,binop,ie2))
| S_Cast (d1, e1) -> 
| S_CastFld (e1, x) ->
| S_CastTbl (d1, d2) ->
| S_FuncCall (e1, el) -> 
| S_Tbl (el) ->
| S_Rec (el) ->
| S_RecRef (e1, e2) ->
| S_Fld (el, x) -> 
| S_Noexpr -> No_expr
and translateArglist vars -> match vars with
[] -> []
| e1:tl -> (let ie1 = translateExpr ie1 in
			let itl = translateArglist tl in
			ie1::itl)

(*Translate Declaration*)
let rec translateDecl decl = match decl with
S_VarDecl (d, e1, e2) -> (let d1 = translateVDatatype d in
					      let ie1 = translateExpr e1 in
 						  let ie2 = translateExpr e2 in
					     {new_datatype = d1; new_name = ie1; new_expr = ie2}
| S_AssignDecl (d, e1, e2) ->
| S_ArrayDecl (d, e1, e2) -> (let d1 = translateADatatype d in
					          let ie1 = translateExpr e1 in
					          let ie2 = translateExpr e2 in
					          {new_datatype = d1; new_name = ie1; new_expr = ie1}

and translateVDatatype d = match d with
Int -> Iint
| Float -> Ifloat
| String -> Istring
| Bool -> Ibool
| _ -> 
and translateADatatype d = match d with
Int -> Iint_array
| Float -> Ifloat_array
| String -> Istring_array
| _ -> 

(*Translate Statement*)
let rec translateStatement stmts = match stmts with
[] -> []
| hd::tl -> let hd_stmt = (match hd with
						  S_Expr(e1) -> (let ie1 = translateExpr e1 in
						  				 Iexpr(ie1))
						  | S_Return(e1) -> (let ie1 = translateExpr e1 in
						  				     IReturn(ie1))
						  | S_If(e1,s1,s2) -> (let ie1 = translateExpr e1 in
						  					   let is1 = translateStatement s1 in
						  					   let is2 = translateStatement s2 in
						  					   IIf(ie1,is1,is2))
						  | S_For(e1,e2,e3,s1) -> (let ie1 = translateExpr e1 in
						  						   let ie2 = translateExpr e2 in
						  						   let ie3 = translateExpr e3 in
						  						   let is1 = translateStatement s1 in
						  						   IFor(ie1,ie2,ie3,s1))
						  | S_While(e1,s1) -> (let ie1 = translateExpr e1 in
						 					   let is1 = translateStatement s1 in
						 					   IWhile(ie1,is1))
						  | S_VarDeclStmt(dc1) -> (let idc1 = translateDecl dc1 in
						  						   IVarDec(idc1))
						  | S_Continue -> IContinue
						  | S_Break -> IBreak)
			in (let tl_stmt = translateStatement tl) 
			in hdstmt::tl_stmt

(*Translate Function Declaration*)
let rec translateFuncDecl fundDecls = match fundDecls with
    [] -> []
  | hd::tl -> (
      let i_fname = hd.s_fname in
      let i_decl = translateDecl hd.s_formals in
      let i_body = translateStatement hd.s_body in
      { i_return = (translateVDatatype hd.s_return_type); 
      	i_fname = i_fname;
        i_args = i_decl; 
        i_body = i_body }
    )::(translateFuncDecl tl)

(*Translate Program*)
let translateProgram src = 
	let func_src = 
		translateFuncDecl src.s_fdecls in
	let var_src =
		translateDecl src.s_gdecls in
	let struct_src =
		in
	{i_var_decls = var_src; i_struct_decls = struct_src; i_func_defs = func_src}

























