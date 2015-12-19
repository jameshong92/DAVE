type action = Ast | Sast | Compile | Verbose (* | Semantic *)

let _ =
	let action = if Array.length Sys.argv > 1 then
		List.assoc Sys.argv.(1) [ ("-a", Ast); 
															("-s", Sast);
															("-c", Compile) ]
	else Compile in
	let lexbuf = Lexing.from_channel stdin in
	let program = Parser.program Scanner.token lexbuf in
	match action with
	Ast -> print_string (Ast.string_of_program program) 
	| Sast -> print_string (Sast.string_of_program (SemanticAnalysis.check program))
	| Compile ->
			Compile.compile "dave.cc" (SemanticAnalysis.check program)
	| _ -> print_string "Usage: dave <compile_option> < <input_file>\n"