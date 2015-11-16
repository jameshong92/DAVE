{ 
	open Parser 
}

let digit = ['0' - '9']
let letter = ['a'-'z' 'A'-'Z']

rule token = parse
	[' ' '\t' '\r' '\n'] { token lexbuf }
| "/*"								 { comment lexbuf }
| "//"								 { singlelinecomment lexbuf }
| '('									 { LPAREN }
| ')'									 { RPAREN }
| '['									 { LBRACK }
| ']'									 { RBRACK }
| '{'									 { LBRACE }
| '}'									 { RBRACE }
| ';'									 { SEMICOL }
| ':'									 { COLON }
| ','									 { COMMA }
| '.'									 { DOT }
| '+'									 { PLUS }
| '-'									 { MINUS }
| '*'									 { TIMES }
| '/'									 { DIVIDE }
| '%'									 { MOD }
| "+="								 { ADDEQ }
| "-="								 { SUBEQ }
| "*="								 { MULEQ }
| "/="								 { DIVEQ }
| "%="								 { MODEQ }
| "++"								 { INC }
| "--"								 { DEC }
| '='									 { ASN }
| "&&"                 { AND }
| "||"                 { OR }
| '!'                  { NOT }
| "=="                 { EQ }
| "!="                 { NEQ }
| '<'                  { LT }
| "<="                 { LEQ }
| ">"                  { GT }
| ">="                 { GEQ }
| "tbl" | "rec" | "fld" as keyword 
											 { VAR_TYPE(keyword) }
| "if"                 { IF }
| "else"               { ELSE }
| "for"                { FOR }
| "while"              { WHILE }
| "break"              { BREAK }
| "continue"           { CONTINUE }
| "return"             { RETURN }
| "void" | "int" | "float" | "bool" | "str" as primtype 
											 { PRIMITIVE_TYPE(primtype) }
| "none"							 { NONE }
| "true" 							 { BOOL_LIT(true) }
| "false"              { BOOL_LIT(false) }
| (digit)+ as lit 		 { INT_LIT(int_of_string lit) }
| ((digit)*'.'(digit)+ | (digit)+'.') as lit
											 { FLOAT_LIT(float_of_string lit) }
| '"' ([^ '"' '\\' '\n' '\r' '\t']* ('\\' ['\\' '"' 'n' 'r' 't'])* as lit) '"'
											 { STR_LIT(lit) }
| letter (letter | digit | '_')* as lit 
											 { ID(lit) }
| eof 								 { EOF }
| _ as c               { raise (Failure("Found illegal character: " ^ Char.escaped c)) }

and comment = parse
	"*/"	{ token lexbuf }
| _			{ comment lexbuf }


and singlelinecomment = parse
	"\n"	{ token lexbuf }
| eof		{ EOF }
| _			{ singlelinecomment lexbuf }
