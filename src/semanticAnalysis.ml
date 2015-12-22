open Ast
open Sast
open SemanticExceptions

module StringMap = Map.Make(String)

let rec type_of_var id v_context =
	if StringMap.mem id v_context then
		fst (StringMap.find id v_context)
	else
		raise (No_variable_err id)

let rec type_of_expr f_context v_context exp = match exp with
	Var(id) -> type_of_var id v_context
	| Array(id, exp) -> type_of_array id exp f_context v_context
	| Access(exp, id) -> type_of_access exp id f_context v_context
	| IntLit(lit) -> { s_ptype = Int; s_dimension = [] }
	| FloatLit(lit) -> { s_ptype = Float; s_dimension = [] }
	| StringLit(lit) -> { s_ptype = String; s_dimension = [] }
	| BoolLit(lit) -> { s_ptype = Bool; s_dimension = [] }
	| ArrayLit(exprs) -> type_of_array_lit exprs f_context v_context
	| Range(exp1, exp2) ->
		let type1 = type_of_expr f_context v_context exp1 and
				type2 = type_of_expr f_context v_context exp2 in
					(match type1.s_ptype, type2.s_ptype with
						Int, Int -> { s_ptype = Int; s_dimension = [{ exp = exp1; typ = type1 }] }
						| Int, Void -> { s_ptype = Int; s_dimension = [{ exp = exp1; typ = type1 }] }
						| Void, Int -> { s_ptype = Int; s_dimension = [{ exp = exp2; typ = type2 }] }
						| _, _ -> raise (Type_err ("type error for " ^ string_of_expr exp1 ^ ", " ^ string_of_expr exp2))
					)
	| Binop(exp1, binop, exp2) -> (
		match binop with
			Add ->
				let type1 = type_of_expr f_context v_context exp1 and
						type2 = type_of_expr f_context v_context exp2 in
							(match type1.s_ptype, type2.s_ptype with
								Int, Int -> type1
								| Int, Float -> type2
								| Float, Int -> type1
								| Float, Float -> type1
								| String, String -> type1
								| Tbl, Tbl -> type1
								| _, _ -> raise (Type_err ("type error for " ^ string_of_expr exp1 ^ ", " ^ string_of_expr exp2))
							)
			| Eq | Neq | Lt | Gt | Leq | Geq ->
				let type1 = type_of_expr f_context v_context exp1 and
						type2 = type_of_expr f_context v_context exp2 in
							(match type1.s_ptype, type2.s_ptype with
								Int, Int -> { s_ptype = Bool; s_dimension = [] }
								| Int, Float -> { s_ptype = Bool; s_dimension = [] }
								| Float, Int -> { s_ptype = Bool; s_dimension = [] }
								| Float, Float -> { s_ptype = Bool; s_dimension = [] }
								| String, String -> { s_ptype = Bool; s_dimension = [] }
								| Bool, Bool -> { s_ptype = Bool; s_dimension = [] }
								| _, _ -> raise (Type_err ("type error for " ^ string_of_expr exp1 ^ " " ^ string_of_datatype type1.s_ptype ^ ", " ^ string_of_expr exp2 ^ " " ^ string_of_datatype type2.s_ptype))
							)
			| Sub | Mul | Div ->
				let type1 = type_of_expr f_context v_context exp1 and
					type2 = type_of_expr f_context v_context exp2 in
						(match type1.s_ptype, type2.s_ptype with
							Int, Int -> type1
							| Int, Float -> type2
							| Float, Int -> type1
							| Float, Float -> type1
							| Tbl, Tbl -> type1
							| _, _ -> raise (Type_err ("type error for " ^ string_of_expr exp1 ^ ", " ^ string_of_expr exp2))
						)

			| Exp | Mod ->
				let type1 = type_of_expr f_context v_context exp1 and
						type2 = type_of_expr f_context v_context exp2 in
							(match type1.s_ptype, type2.s_ptype with
								Int, Int -> type1
								| Int, Float -> type2
								| Float, Int -> type1
								| Float, Float -> type1
								| _, _ -> raise (Type_err ("type error for " ^ string_of_expr exp1 ^ ", " ^ string_of_expr exp2))
							)
			| And | Or ->
				let type1 = type_of_expr f_context v_context exp1 and
						type2 = type_of_expr f_context v_context exp2 in
							(match type1.s_ptype, type2.s_ptype with
								| Bool, Bool -> { s_ptype = Bool; s_dimension = [] }
								| _, _ -> raise (Type_err ("type error for " ^ string_of_expr exp1 ^ ", " ^ string_of_expr exp2))
							)
	)
	| Unop(unop, exp) -> (
		match unop with
			Not ->
				let type1 = type_of_expr f_context v_context exp in
						(match type1.s_ptype with
							Int -> type1
							| Bool -> type1
							| _ -> raise (Type_err ("type error for " ^ string_of_expr exp))
						)
			| Neg ->
				let type1 = type_of_expr f_context v_context exp in
						(match type1.s_ptype with
							Int -> type1
							| Bool -> type1
							| Float -> type1
							| _ -> raise (Type_err ("type error for " ^ string_of_expr exp))
						)
	)
	| Postop(exp, postop) -> (
		match postop with
			Inc | Dec ->
				let type1 = type_of_expr f_context v_context exp in
						(match type1.s_ptype with
							Int -> type1
							| _ -> raise (Type_err ("type error for " ^ string_of_expr exp))
						)
	)
	| AssignOp(exp1, asnop, exp2) -> (
		match asnop with
			Asn ->
				let type1 = type_of_expr f_context v_context exp1 and
						type2 = type_of_expr f_context v_context exp2 in
							if List.length type1.s_dimension != List.length type2.s_dimension then
								raise (Type_err ("type error for " ^ string_of_expr exp1 ^ "type: " ^ string_of_datatype type1.s_ptype ^ ", " ^ string_of_expr exp2))
							else if List.length type2.s_dimension != 0 && type1.s_ptype == type2.s_ptype then
								type2
							else
								(match type1.s_ptype, type2.s_ptype with
									Int, Int -> type2
									| Float, Float -> type2
									| String, String -> type2
									| Bool, Bool -> type2
									| Fld, Fld -> type2
									| Rec, Rec -> type2
									| Tbl, Tbl -> type2
									| _, _ -> raise (Type_err ("type error for " ^ string_of_expr exp1 ^ "type: " ^ string_of_datatype type1.s_ptype ^ ", " ^ string_of_expr exp2))
								)
			| Addeq ->
				let type1 = type_of_expr f_context v_context exp1 and
						type2 = type_of_expr f_context v_context exp2 in
							(match type1.s_ptype, type2.s_ptype with
								Int, Int -> type1
								| Int, Float -> type2
								| Float, Int -> type1
								| Float, Float -> type1
								| String, String -> type1
								| _, _ -> raise (Type_err ("type error for " ^ string_of_expr exp1 ^ ", " ^ string_of_expr exp2))
							)
			| Subeq | Muleq | Diveq | Modeq ->
				let type1 = type_of_expr f_context v_context exp1 and
						type2 = type_of_expr f_context v_context exp2 in
							(match type1.s_ptype, type2.s_ptype with
								Int, Int -> type1
								| Int, Float -> type2
								| Float, Int -> type1
								| Float, Float -> type1
								| _, _ -> raise (Type_err ("type error for " ^ string_of_expr exp1 ^ ", " ^ string_of_expr exp2))
							)
	)
	| Lval(exp) -> type_of_expr f_context v_context exp (* TODO: CHECK LVAL *)
	| Cast(var, exp) -> { s_ptype = var.ptype; s_dimension = [] } (* TODO: check if it works *)
	| FuncCall(fid, actuals_opt) -> type_of_func_ret fid actuals_opt f_context v_context
	| Tbl(exprs) -> type_of_tbl exprs f_context v_context
	| Rec(exprs) -> type_of_rec exprs f_context v_context
	| RecRef(id, exp) -> type_of_rec_ref id exp f_context v_context
	| Fld(expr, id) -> type_of_fld expr id f_context v_context
	| Noexpr -> { s_ptype = Void; s_dimension = [] }
	| None -> { s_ptype = Void; s_dimension = [] } 

and type_of_tbl exprs f_context v_context =
  let expr_list = (List.map (fun expr -> type_of_expr f_context v_context expr) exprs) in
    if List.length (List.filter (fun a -> (match a.s_ptype with Fld -> true | _ -> false)) expr_list) == (List.length exprs) ||
    	 List.length (List.filter (fun a -> (match a.s_ptype with Rec -> true | _ -> false)) expr_list) == (List.length exprs) then
    		{ s_ptype = Tbl; s_dimension = []}
			else
				raise (Tbl_err "Table error")

and type_of_fld expr id f_context v_context =
	let first_lit = type_of_expr f_context v_context expr in
		match first_lit.s_ptype with
			Int | String | Bool | Float -> 
				if (List.length first_lit.s_dimension) > 0 then
					{ s_ptype = Fld; s_dimension = [] }
				else raise Fld_err
			| _ -> raise Fld_err

and type_of_rec exprs f_context v_context =
  let rec_ref_list = (List.map (fun expr -> s_check_expr f_context v_context expr) exprs) in
    if List.length (List.filter (fun a -> (match a.exp with RecRef(_,_) -> true | _ -> false)) rec_ref_list) == (List.length exprs) then
					{ s_ptype = Rec; s_dimension = []}
			else
				raise Rec_err

and type_of_rec_ref id expr f_context v_context =
	let first_lit = type_of_expr f_context v_context expr in
		match first_lit.s_ptype with
			Int | String | Bool | Float -> 
				if (List.length first_lit.s_dimension) == 0 then
					{ s_ptype = first_lit.s_ptype; s_dimension = first_lit.s_dimension }
				else raise Rec_ref_err
			| _ -> raise Rec_ref_err

and type_of_array_lit exprs f_context v_context =
	let first_lit = type_of_expr f_context v_context (List.hd exprs) in
		let check_function x =
			let p = (type_of_expr f_context v_context x) in
				(p.s_ptype = first_lit.s_ptype) && (List.length p.s_dimension) = (List.length first_lit.s_dimension) in
		let list_filter = List.filter check_function exprs in
			if List.length list_filter == List.length exprs then
				let exp = {exp = IntLit(List.length exprs); typ = {s_ptype = first_lit.s_ptype; s_dimension = []}} in
					{ s_ptype = first_lit.s_ptype; s_dimension = [exp]}
			else
				raise Array_lit_err

and type_of_array id exp f_context v_context =
	if StringMap.mem id v_context then
		let type1 = fst (StringMap.find id v_context) in
			(* check if id is of type array *)
			if List.length type1.s_dimension >= 1 then
				(* check if given index is of type int or range *)
				let type2 = type_of_expr f_context v_context exp in
					if List.length type2.s_dimension >= 1 && type2.s_ptype == Int then
						(* TODO: check if this is correct? check for dimension size *)
						{s_ptype = type1.s_ptype; s_dimension = type1.s_dimension}
					else if type2.s_ptype == Int then
						{s_ptype = type1.s_ptype; s_dimension = []}
					else
						raise Arr_err
			else
				raise Arr_err
	else
		raise Arr_err

and type_of_access exp id f_context v_context = (
	let exp_type = type_of_expr f_context v_context exp in
	match exp_type.s_ptype with
		Fld -> 
			if id = "length" then 
				{s_ptype = Int; s_dimension = []} 
			else if id = "name" then
				{s_ptype = String; s_dimension = []} 
			else if id = "type" then 
				{s_ptype = Int; s_dimension = []} 
			else
				raise Access_err
		| Tbl -> if id = "row_length" || id = "col_length" then {s_ptype = Int; s_dimension = []} else raise Access_err
		| Rec -> if id = "length" then {s_ptype = Int; s_dimension = []} else raise Access_err
		| Int | Bool | Float | String -> if (List.length exp_type.s_dimension) == 1 then {s_ptype = Int; s_dimension = []} else raise Access_err
		| _ -> raise Access_err
)

and type_of_func_ret fid param f_context v_context =
	if StringMap.mem fid f_context then
		let s_param = List.map (fun x -> type_of_expr f_context v_context x) param in
			let found_map = StringMap.find fid f_context in
				try
					let found = List.find (match_param s_param) found_map in
						snd found
				with Not_found -> raise (Invalid_func_err (fid ^ "(" ^ String.concat ", " (List.map (fun x -> string_of_datatype x.s_ptype) s_param) ^ ")"))
	else
		raise (No_func_err fid)

and match_param p1 map =
	let p2 = fst map in
		let rec check_param t_l1 t_l2 = match t_l1, t_l2 with
			[], [] -> true
			| hd::tl, [] -> false
			| [], hd::tl -> false
			| h1::t1, h2::t2 ->
				if h1.s_ptype == h2.s_ptype && List.length h1.s_dimension == List.length h2.s_dimension then
					check_param t1 t2
				else
					false
		in check_param p1 p2

and s_check_expr f_context v_context in_exp = match in_exp with
	ArrayLit(indices) ->
		{exp = ArrayLit(List.map (fun a -> (s_check_expr f_context v_context a).exp) indices); typ = type_of_expr f_context v_context in_exp }
	| Range(exp1, exp2) ->
		{exp = Range((s_check_expr f_context v_context exp1).exp, (s_check_expr f_context v_context exp2).exp); typ = type_of_expr f_context v_context in_exp }
	| Binop(exp1, binop, exp2) ->
		{exp = Binop((s_check_expr f_context v_context exp1).exp, binop, (s_check_expr f_context v_context exp2).exp); typ = type_of_expr f_context v_context in_exp }
	| Unop(unop, exp) ->
		{exp = Unop(unop, (s_check_expr f_context v_context exp).exp); typ = type_of_expr f_context v_context in_exp }
	| Postop(lvalue, postop) ->
		{exp = Postop((s_check_expr f_context v_context lvalue).exp, postop); typ = type_of_expr f_context v_context in_exp }
	| AssignOp(lvalue, asnop, exp2) ->
		{exp = AssignOp((s_check_expr f_context v_context lvalue).exp, asnop, (s_check_expr f_context v_context exp2).exp); typ = type_of_expr f_context v_context in_exp }
	| Lval(lvalue) ->
		{exp = Lval((s_check_expr f_context v_context lvalue).exp); typ = type_of_expr f_context v_context in_exp }
	| Cast(var, exp) -> (* TODO: check cast *)
		{exp = Cast(var, (s_check_expr f_context v_context exp).exp); typ = type_of_expr f_context v_context in_exp }
	| Array(id, indices) -> s_check_array f_context v_context in_exp id indices
	| Access(exp, id) -> s_check_access f_context v_context in_exp exp id
	| FuncCall(id, exprs) -> {exp = FuncCall(id, List.map (fun a -> (s_check_expr f_context v_context a).exp) exprs); typ = (type_of_expr f_context v_context in_exp)}
	| Tbl(exprs) -> s_check_tbl f_context v_context in_exp exprs
	| Rec(exprs) -> s_check_rec f_context v_context in_exp exprs
	| Fld(expr, id) -> s_check_fld f_context v_context in_exp expr id
	| _ -> { exp = in_exp; typ = type_of_expr f_context v_context in_exp }

and s_check_access f_context v_context in_exp expr id =
	let exp_type = type_of_expr f_context v_context expr in (
		match exp_type.s_ptype with
			Int | String | Bool | Float -> { exp = Access((s_check_expr f_context v_context expr).exp, "size()"); typ = type_of_expr f_context v_context in_exp }
			| _ -> { exp = Access((s_check_expr f_context v_context expr).exp, id); typ = type_of_expr f_context v_context in_exp }
		)
and s_check_tbl f_context v_context in_exp exprs =
  let tbl_exprs = (List.map (fun expr -> s_check_expr f_context v_context expr) exprs) in
	  if List.length (List.filter (fun a -> (
	  		match a.exp with 
	  		Lval(lval) -> let lval_type = type_of_expr f_context v_context lval in (
	  			if (List.length lval_type.s_dimension) > 0 then true
	  			else false
	  		)
	  		| ArrayLit(_) -> false 
	  		| _ -> raise (Tbl_err (string_of_expr a.exp)))) tbl_exprs
		) == (List.length exprs) then
      {exp = in_exp; typ = { s_ptype = Tbl; s_dimension = [] }}
    else
    	raise (Tbl_err "Parameter in the table cannot be a type of array")

and s_check_rec f_context v_context in_exp exprs =
  let rec_ref_list = (List.map (fun expr -> s_check_expr f_context v_context expr) exprs) in
    if List.length (List.filter (fun a -> (match a.exp with RecRef(_,_) -> true | _ -> false)) rec_ref_list) == (List.length exprs) then
    	{ exp = Rec(List.map (fun a -> (s_check_expr f_context v_context a).exp) exprs); typ = (type_of_expr f_context v_context in_exp)}
    else
    	raise Rec_err

and s_check_rec_ref f_context v_context in_exp id expr =
	{ exp = RecRef(id, (s_check_expr f_context v_context expr).exp); typ = type_of_expr f_context v_context in_exp }

and s_check_fld f_context v_context in_exp expr id =
	let exp_type = type_of_expr f_context v_context expr in
		if List.length exp_type.s_dimension > 0 then
			{ exp = Fld((s_check_expr f_context v_context expr).exp, id); typ = type_of_expr f_context v_context in_exp }

		else
			raise Fld_err

and s_check_array f_context v_context in_exp id indices =
	let index_type = type_of_expr f_context v_context indices and
			index_expr = s_check_expr f_context v_context indices in
		if List.length index_type.s_dimension == 0 then
			{exp = Array(id, index_expr.exp); typ = type_of_expr f_context v_context in_exp}
		else
			match index_expr.exp with
				Range(exp1, exp2) ->
					let type1 = type_of_expr f_context v_context exp1 and
							type2 = type_of_expr f_context v_context exp2 in
						if type1.s_ptype == Int && type2.s_ptype == Int then
							{exp = Array(id, index_expr.exp); typ = type_of_expr f_context v_context in_exp}
						else if type1.s_ptype == Void && type2.s_ptype == Int then
							let start_range = {exp = Range(IntLit(0), (s_check_expr f_context v_context exp2).exp); typ = type_of_expr f_context v_context in_exp } in
								{exp = Array(id, start_range.exp); typ = type_of_expr f_context v_context in_exp}
						else if type1.s_ptype == Int && type2.s_ptype == Void then
							let end_range = {exp = Range((s_check_expr f_context v_context exp1).exp, IntLit(0)); typ = type_of_expr f_context v_context in_exp } in
								{exp = Array(id, end_range.exp); typ = type_of_expr f_context v_context in_exp}
						else raise Arr_err
				| _ -> raise Arr_err

let s_check_var_type f_context v_context vtype =
    let dimention_type_list = (List.map (fun expr -> type_of_expr f_context v_context expr) vtype.dimension) in
      if List.length (List.filter (fun a -> (a.s_ptype == Int)) dimention_type_list) == List.length dimention_type_list
      then
          {s_ptype = vtype.ptype;
          s_dimension = List.map (fun expr -> s_check_expr f_context v_context expr) vtype.dimension }
      else (* Error dimension not int *)
          raise Var_type_err

let s_stmt_context_v f_context v_context level stmt = match stmt with
	VarDeclStmt(vdecl) ->
		let lhs = (s_check_var_type f_context v_context vdecl.vtype) and
				rhs = (type_of_expr f_context v_context vdecl.vinit) in
			(* check if variable is not initialized or is initialized to same type *)
			if vdecl.vinit == Noexpr || vdecl.vinit == None || (List.length lhs.s_dimension == List.length rhs.s_dimension && lhs.s_ptype == rhs.s_ptype) then
				if StringMap.mem vdecl.vname v_context then
					let p_level = snd (StringMap.find vdecl.vname v_context) in
					(* check if variable has been defined in the same level *)
					if p_level == level then
						raise Duplicate_variable_err
					else
						StringMap.add vdecl.vname ((s_check_var_type f_context v_context vdecl.vtype), level) v_context
				else
					StringMap.add vdecl.vname ((s_check_var_type f_context v_context vdecl.vtype), level) v_context
			else
				raise (Init_type_err ("lhs type: " ^ string_of_datatype lhs.s_ptype ^ ", dimension: " ^ string_of_int (List.length lhs.s_dimension) ^ ", rhs type: " ^ string_of_datatype rhs.s_ptype ^ ", dimension: " ^ string_of_int (List.length rhs.s_dimension)))
	| _ -> v_context

let s_check_var_decl f_context v_context vdecl =
	let lhs = (s_check_var_type f_context v_context vdecl.vtype) and
			rhs = (type_of_expr f_context v_context vdecl.vinit) in
		if vdecl.vinit == Noexpr || vdecl.vinit == None || (List.length lhs.s_dimension == List.length rhs.s_dimension && lhs.s_ptype == rhs.s_ptype) then
			{
				s_vname = vdecl.vname;
				s_vtype = (s_check_var_type f_context v_context vdecl.vtype);
				s_vinit = (s_check_expr f_context v_context vdecl.vinit)
			}
		else
			raise (Init_type_err "Type does not match")

let rec s_check_stmt_list context_list stmt_list = match context_list, stmt_list with
     [], [] -> []
   | context_hd::context_tl, stmt_hd::stmt_tl ->
           (s_check_stmt (fst context_hd) (snd context_hd) stmt_hd)
        :: (s_check_stmt_list context_tl stmt_tl)
   | _, _ -> raise Stmt_list_err

and s_check_stmt f_context v_context level stmt = match stmt with
	Expr(expr) -> S_Expr(s_check_expr f_context v_context expr)
	| Return(expr) ->
		let t_expr = s_check_expr f_context v_context expr in
			if StringMap.mem "0current" f_context then
				let curr = StringMap.find "0current" f_context in
					if t_expr.typ.s_ptype == (snd (List.hd curr)).s_ptype &&
							List.length t_expr.typ.s_dimension == List.length (snd (List.hd curr)).s_dimension then
						S_Return(s_check_expr f_context v_context expr)
					else
						raise Return_type_err
			else
				raise Current_not_found
	| Block(stmt_list) ->
		let first(f,_,_) = f and second(_,s,_) = s and third (_,_,t) = t in
			S_Block(List.rev
				(first
					(List.fold_left
						(fun x y -> (((s_check_stmt (second x) (third x) (level+1) y) :: (first x)),
												 (second x),
												 (s_stmt_context_v (second x) (third x) (level+1) y)
												)
						) ([], f_context, v_context) stmt_list
		)
		))
	| If(expr, stmt1, stmt2) ->
	  let exp_type = type_of_expr f_context v_context expr in
			if (exp_type.s_ptype == Bool && exp_type.s_dimension == []) then
				S_If(s_check_expr f_context v_context expr,
				s_check_stmt f_context v_context level stmt1,
				s_check_stmt f_context v_context level stmt2)
			else
				raise Err_s_check_stmt_if;
	| For(expr1, expr2, expr3, stmt) ->
	       let expr2_t = type_of_expr f_context v_context expr2 in
	    if expr2_t.s_ptype ==  Bool && List.length expr2_t.s_dimension == 0
	    then
	        S_For(s_check_expr f_context v_context expr1,
	        s_check_expr f_context v_context expr2,
	        s_check_expr f_context v_context expr3,
	        s_check_stmt f_context v_context level stmt)
	    else
	        raise Err_s_check_stmt_for;
	| While(expr, stmt) ->
	       let expr_t = type_of_expr f_context v_context expr in
	    if expr_t.s_ptype == Bool && expr_t.s_dimension == []
	    then
	        S_While(s_check_expr f_context v_context expr,
	        s_check_stmt f_context v_context level stmt)
	    else
	        raise Err_s_check_stmt_while;
	| VarDeclStmt(vdecl) ->
	       S_VarDeclStmt(s_check_var_decl f_context v_context vdecl)
	| Continue -> S_Continue
	| Break -> S_Break
	| EmptyStmt -> S_EmptyStmt

let s_check_func_decl f_context v_context fdecl =
	let s_formals = List.map (fun var_decl -> s_check_var_decl f_context v_context var_decl) fdecl.formals in
	let s_return_type = s_check_var_type f_context v_context fdecl.return_type in
		{
			s_fname = fdecl.fname;
			s_formals = s_formals;
			s_return_type = s_return_type;
			s_body = s_check_stmt (StringMap.add "0current" [(List.map (fun decl -> decl.s_vtype) s_formals, s_return_type)] f_context)
														(List.fold_left (fun map decl ->
															(* check if variable is already defined within the function *)
															if StringMap.mem decl.s_vname map && snd (StringMap.find decl.s_vname map) == 1 then
																raise Duplicate_variable_err
															else
																StringMap.add decl.s_vname (decl.s_vtype, 1) map) v_context s_formals)
														1 fdecl.body
		}

(* check list of function declarations *)
let rec s_check_func_decls f_context v_context func_decl_list = match func_decl_list with
	[] -> []
	| hd::tl ->
		(s_check_func_decl f_context v_context hd) :: (s_check_func_decls f_context v_context tl)

let s_var_decl_to_var_map map s_vdecl =
	if StringMap.mem s_vdecl.s_vname map then
		(* check if variable name is already defined *)
		raise Duplicate_variable_err
	else
		(* add variable to map, 0 for gdecl *)
		StringMap.add s_vdecl.s_vname (s_vdecl.s_vtype, 0) map

(* check if vartiable type is the same for the two arguments *)
let is_s_var_type_equal t1 t2 =
	if t1.s_ptype == t2.s_ptype && List.length t1.s_dimension == List.length t2.s_dimension then
		true
	else
		false

(* recursively check if the variables in the list are equal *)
let rec is_s_var_type_list_equal l1 l2 = match l1, l2 with
	[], [] -> true
	| [], hd::tl -> false
	| hd::tl, [] -> false
	| h1::t1, h2::t2 ->
		if is_s_var_type_equal h1 h2 then
			is_s_var_type_list_equal t1 t2
		else
			false

(* check if the function return type is equal *)
let func_decl_check_func_map s_vtype_list_map s_vtype_list =
	let check = List.map (fun l1 -> is_s_var_type_list_equal (fst l1) s_vtype_list) s_vtype_list_map in
		if List.length (List.filter (fun a -> a) check) != 0 then
			true
		else
			false

let func_decl_to_func_map map fdecl v_context =
	let func_s_vtype_list = List.map (fun decl -> decl.s_vtype) (List.map (fun arg -> s_check_var_decl StringMap.empty v_context arg) fdecl.formals) in
		if StringMap.mem fdecl.fname map then
			if func_decl_check_func_map (StringMap.find fdecl.fname map) func_s_vtype_list then
				(* raise duplicate function error if function of same name and return type is already defined *)
				raise Duplicate_function_err
			else
				(* add function to map if function type is different *)
				StringMap.add fdecl.fname ((func_s_vtype_list, s_check_var_type StringMap.empty v_context fdecl.return_type)::StringMap.find fdecl.fname map) map
		else
			(* if function name is not in the map, add new map with list of functions for that fname *)
			StringMap.add fdecl.fname [(func_s_vtype_list, s_check_var_type StringMap.empty v_context fdecl.return_type)] map

let check prog check_option =
	let s_gdecls = List.map (fun var_decl -> s_check_var_decl StringMap.empty StringMap.empty var_decl) prog.gdecls
	and extern_funs = (
			(* TODO: add external functions to include *)
			let map = StringMap.empty in
			let map = StringMap.add "print" [([{s_ptype = String; s_dimension = []}], {s_ptype = Void; s_dimension = []});
														 					 ([{s_ptype = Int; s_dimension = []}], {s_ptype = Void; s_dimension = []});
														 					 ([{s_ptype = Float; s_dimension = []}], {s_ptype = Void; s_dimension = []});
		                         					 ([{s_ptype = Bool; s_dimension = []}], {s_ptype = Void; s_dimension = []});
		                         					 ([{s_ptype = Rec; s_dimension = []}], {s_ptype = Void; s_dimension = []});
		                         					 ([{s_ptype = Fld; s_dimension = []}], {s_ptype = Void; s_dimension = []});
		                         					 ([{s_ptype = Tbl; s_dimension = []}], {s_ptype = Void; s_dimension = []})] map in
			let map = StringMap.add "get_int" [([{s_ptype = Fld; s_dimension = []}; {s_ptype = Int; s_dimension = []}], {s_ptype = Int; s_dimension = []})] map in
			let map = StringMap.add "get_string" [([{s_ptype = Fld; s_dimension = []}; {s_ptype = Int; s_dimension = []}], {s_ptype = String; s_dimension = []})] map in
			let map = StringMap.add "get_float" [([{s_ptype = Fld; s_dimension = []}; {s_ptype = Int; s_dimension = []}], {s_ptype = Float; s_dimension = []})] map in
			let map = StringMap.add "get_bool" [([{s_ptype = Fld; s_dimension = []}; {s_ptype = Int; s_dimension = []}], {s_ptype = Bool; s_dimension = []})] map in
		  let map = StringMap.add "load" [([{s_ptype = String; s_dimension = []}], {s_ptype = Tbl; s_dimension = []})] map in
		  let map = StringMap.add "save" [([{s_ptype = Tbl; s_dimension = []}; {s_ptype = String; s_dimension = []}], {s_ptype = Void; s_dimension = []})] map in
		  let map = StringMap.add "access" [([{s_ptype = Tbl; s_dimension = []}; {s_ptype = String; s_dimension = [{ exp = IntLit(0); typ = {s_ptype = Int; s_dimension = []} }]}; {s_ptype = Int; s_dimension = []}], {s_ptype = Tbl; s_dimension = []});
		  																	([{s_ptype = Tbl; s_dimension = []}; {s_ptype = Int; s_dimension = []}; {s_ptype = Int; s_dimension = []}], {s_ptype = Tbl; s_dimension = []});
		  																	([{s_ptype = Tbl; s_dimension = []}; {s_ptype = String; s_dimension = []}], {s_ptype = Fld; s_dimension = []});
		  																	([{s_ptype = Tbl; s_dimension = []}; {s_ptype = Int; s_dimension = []}], {s_ptype = Rec; s_dimension = []})] map in
			let map = StringMap.add "append" [([{s_ptype = Tbl; s_dimension = []}; {s_ptype = Rec; s_dimension = []}], {s_ptype = Tbl; s_dimension = []});
		  																	([{s_ptype = Tbl; s_dimension = []}; {s_ptype = Fld; s_dimension = []}], {s_ptype = Tbl; s_dimension = []});
		  																	([{s_ptype = Tbl; s_dimension = []}; {s_ptype = Tbl; s_dimension = []}; {s_ptype = Bool; s_dimension = []}], {s_ptype = Tbl; s_dimension = []})] map in
		  let map = StringMap.add "modify" [([{s_ptype = Tbl; s_dimension = []}; {s_ptype = Int; s_dimension = []}; {s_ptype = Int; s_dimension = []}; {s_ptype = Float; s_dimension = []}], {s_ptype = Tbl; s_dimension = []});
		  																	([{s_ptype = Tbl; s_dimension = []}; {s_ptype = Int; s_dimension = []}; {s_ptype = Int; s_dimension = []}; {s_ptype = Bool; s_dimension = []}], {s_ptype = Tbl; s_dimension = []});
		  																	([{s_ptype = Tbl; s_dimension = []}; {s_ptype = Int; s_dimension = []}; {s_ptype = Int; s_dimension = []}; {s_ptype = String; s_dimension = []}], {s_ptype = Tbl; s_dimension = []});
		  																	([{s_ptype = Tbl; s_dimension = []}; {s_ptype = Int; s_dimension = []}; {s_ptype = Int; s_dimension = []}; {s_ptype = Int; s_dimension = []}], {s_ptype = Tbl; s_dimension = []})] map in
		  (* add dave library functions *)
		  if check_option = "import" then 
		  	map 
		  else
		  	let map = StringMap.add "min" [([{s_ptype = Fld; s_dimension = []}], {s_ptype = Int; s_dimension = []});
		  																 ([{s_ptype = Fld; s_dimension = []}], {s_ptype = Float; s_dimension = []})] map in
				let map = StringMap.add "max" [([{s_ptype = Fld; s_dimension = []}], {s_ptype = Int; s_dimension = []});
		  																 ([{s_ptype = Fld; s_dimension = []}], {s_ptype = Float; s_dimension = []})] map in
			  StringMap.add "mean" [([{s_ptype = Fld; s_dimension = []}], {s_ptype = Int; s_dimension = []});
		  																	([{s_ptype = Fld; s_dimension = []}], {s_ptype = Float; s_dimension = []})] map
		)
		in {
			s_gdecls = s_gdecls;
			s_fdecls =
				let v_context = List.fold_left s_var_decl_to_var_map StringMap.empty s_gdecls in
				let f_context = List.fold_left (fun ext_func_map func_decl -> func_decl_to_func_map ext_func_map func_decl v_context) extern_funs prog.fdecls in
					if (StringMap.mem "main" f_context) || check_option = "import" then
						s_check_func_decls f_context v_context prog.fdecls
					else
					(* main has to be defined in the function *)
						raise Main_not_found_err
		}
