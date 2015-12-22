open Printf

type action = Ast | Compile | Verbose (* | Semantic *)

let loc_err lex_buf =
  let p = lex_buf.Lexing.lex_curr_p in
  let tok = Lexing.lexeme lex_buf in
  let fname = p.Lexing.pos_fname in
  let line = p.Lexing.pos_lnum in
  let cnum = p.Lexing.pos_cnum - p.Lexing.pos_bol + 1
             - String.length tok in
  sprintf "token %s, in %s line %d,%d" tok fname line cnum

let _ =
	let action = if Array.length Sys.argv > 1 then
		List.assoc Sys.argv.(1) [ ("-a", Ast); 
															("-c", Compile) ]
	else Compile in
	let lexbuf = Lexing.from_channel stdin and
			std_lib = Lexing.from_channel (open_in "dave_core.dave") in
	let programs = (
		try
			List.map (fun x -> Parser.program Scanner.token x) [std_lib; lexbuf]
		with
    	Parsing.Parse_error -> raise (Ast.Syntax_error (loc_err lexbuf)) 
    ) in
	match action with
	Ast -> print_string (Ast.string_of_program (List.nth programs 1)) 
	| Compile ->
			let import = (SemanticAnalysis.check (List.hd programs) "import") in
			let checked_program = import :: [SemanticAnalysis.check (List.nth programs 1) ""] in
			Compile.compile "dave.cc" checked_program
	| _ -> print_string "Usage: dave <compile_option> < <input_file>\n"