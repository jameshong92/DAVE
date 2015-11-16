type action = Ast | Tcgen

let _ =
	let action = if Array.length Sys.argv > 1 then
		List.assoc Sys.argv.(1) [ ("-a", Ast); 
															("-c", Tcgen) ]
	else Tcgen in
		let lexbuf = Lexing.from_channel stdin in
			let program = Parser.program Scanner.token lexbuf in
				match action with
					Ast -> print_string (Ast.string_of_program program)
					| _ -> print_string "Unrecognized option"
