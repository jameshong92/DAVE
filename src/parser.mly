%{ open Ast %}

%token LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK
%token SEMICOL COMMA DOT COLON
%token ADDEQ SUBEQ MULEQ DIVEQ MODEQ
%token PLUS MINUS TIMES DIVIDE MOD INC DEC EXP
%token ASN AND OR NOT
%token EQ NEQ LT LEQ GT GEQ
%token CONTINUE BREAK IF ELSE FOR WHILE RETURN
%token FLD TBL REC
%token <int> INT_LIT
%token <float> FLOAT_LIT
%token <bool> BOOL_LIT
%token <string> STR_LIT ID VAR_TYPE PRIMITIVE_TYPE
%token NONE
%token EOF

%nonassoc NOELSE
%nonassoc ELSE
%left COMMA
%right ASN INC DEC ADDEQ SUBEQ MULEQ DIVEQ MODEQ
%right DOT
%left LBRACK
%left OR
%left AND
%left EQ NEQ
%left LT LEQ GT GEQ
%left PLUS MINUS
%left TIMES DIVIDE MOD EXP
%right NOT UNMINUS

%start program
%type <Ast.program> program

%%

/* Expressions */
/* id */
id:
	ID 													{ $1 }

/* str int float bool rec fld tbl str[] int[] float[] bool[] */
datatype:
	PRIMITIVE_TYPE							{
																{
																	ptype = type_of_string $1;
																	dimension = []
																}
															}
	| VAR_TYPE									{
																{
																	ptype = type_of_string $1;
																	dimension = []
																}
															}
	| PRIMITIVE_TYPE LBRACK expr_opt RBRACK
															{ 
																{
																	ptype = type_of_string $1;
																	dimension = [(if $3 == Noexpr then IntLit(0) else $3)];
																}
															}

/* id[index_list], expr.id */
lvalue:
	id 													{ Var($1) }
	| id LBRACK index_list RBRACK
															{ Array($1, $3) }
	| expr DOT id 							{ Access($1, $3) }

index_list:
	expr 									 			{ $1 }
	| COLON expr 					 			{ Range(Noexpr, $2) }
	| expr COLON					 			{ Range($1, Noexpr) }
	| expr COLON expr 		 			{ Range($1, $3) }

expr:
	| expr PLUS expr 						{ Binop($1, Add, $3) }
	| expr MINUS expr 					{ Binop($1, Sub, $3) }
	| expr TIMES expr 					{ Binop($1, Mul, $3) }
	| expr DIVIDE expr 					{ Binop($1, Div, $3) }
	| expr MOD expr 						{ Binop($1, Mod, $3) }
	| expr EXP expr 						{ Binop($1, Exp, $3) }
	| expr EQ expr 							{ Binop($1, Eq, $3) }
	| expr NEQ expr 						{ Binop($1, Neq, $3) }
	| expr LT expr 							{ Binop($1, Lt, $3) }
	| expr GT expr 							{ Binop($1, Gt, $3) }
	| expr LEQ expr 						{ Binop($1, Leq, $3) }
	| expr GEQ expr 						{ Binop($1, Geq, $3) }
	| expr AND expr 						{ Binop($1, And, $3) }
	| expr OR expr 							{ Binop($1, Or, $3) }

	| NOT expr 									{ Unop(Not, $2) }
	| MINUS expr %prec UNMINUS	{ Unop(Neg, $2) }

	| lvalue INC 								{ Postop($1, Inc) }
	| lvalue DEC 								{ Postop($1, Dec) }

	| lvalue ADDEQ expr 				{ AssignOp($1, Addeq, $3) }
	| lvalue SUBEQ expr 				{ AssignOp($1, Subeq, $3) }
	| lvalue MULEQ expr 				{ AssignOp($1, Muleq, $3) }
	| lvalue DIVEQ expr 				{ AssignOp($1, Diveq, $3) }
	| lvalue MODEQ expr 				{ AssignOp($1, Modeq, $3) }
	| lvalue ASN expr 					{ AssignOp($1, Asn, $3) }
	| lvalue 										{ Lval($1) }

	| LPAREN expr RPAREN 				{ $2 }

	| datatype LPAREN expr RPAREN
															{ Cast($1, $3) }
/*	| FLD LPAREN expr COMMA STR_LIT RPAREN
															{ CastFld($3, $5) }
	| TBL LPAREN VAR_TYPE COMMA VAR_TYPE RPAREN
															{ CastTbl(type_of_string $3, type_of_string $5) } */
	| literal 									{ $1 }
	| tbl_lit 									{ Tbl($1) }
	| fld_lit 									{ $1 }
	| rec_lit 									{ $1 }

	| id LPAREN actuals_opt RPAREN
															{ FuncCall($1, $3) }

literal:
	INT_LIT 										{ IntLit($1) }
	| FLOAT_LIT 								{ FloatLit($1) }
	| STR_LIT 									{ StringLit($1) }
	| BOOL_LIT 									{ BoolLit($1) }
	| LBRACK literal_list RBRACK { ArrayLit(List.rev $2) }

literal_list:
	literal 										{ [$1] }
	| literal_list COMMA literal { $3 :: $1 }

tbl_lit:
	TBL LPAREN rec_lit_list RPAREN
															{ List.rev $3 }
	| TBL LPAREN fld_lit_list RPAREN
															{ List.rev $3 }

rec_lit_list:
	rec_lit_list COMMA rec_lit 	{ $3 :: $1 }
	| rec_lit 									{ [$1] }

rec_lit:
	REC LBRACE rec_init RBRACE 			{ Rec(List.rev $3) }

rec_init:
	id COLON literal 						{ [RecRef($1, $3)] }
	| rec_init COMMA id COLON literal
															{ RecRef($3, $5) :: $1 }

fld_lit_list:
	fld_lit_list COMMA fld_lit 	{ $3 :: $1 }
	| fld_lit 									{ [$1] }

fld_lit:
	FLD LPAREN actuals_list COMMA STR_LIT RPAREN
															{ Fld(List.rev $3, $5) }

actuals_opt:
	/* nothing */ 							{ [] }
	| actuals_list 							{ List.rev $1 }

actuals_list:
	expr 												{ [$1] }
	| actuals_list COMMA expr 	{ $3 :: $1 }

/* Declarations */
program:
  /* nothing */ 							{ { gdecls = []; fdecls = [] } }
  | program vdecl 						{ { gdecls = $2 :: $1.gdecls; fdecls = $1.fdecls } }
  | program fdecl 						{ { gdecls = $1.gdecls; fdecls = $2 :: $1.fdecls } }

fdecl:
	datatype ID LPAREN formals_opt RPAREN LBRACE stmt_list RBRACE
															{ {
																	fname = $2;
																	formals = $4;
																	body = Block(List.rev $7);
																	return_type = $1
															} }

formals_opt:
	/* nothing */ 							{ [] }
	| formal_list 							{ List.rev $1 }

/* TODO: modify formal_list if doing in c++ */
formal_list:
	datatype id 								{[
																{
																	vname = $2;
																	vtype = $1;
																	vinit = Noexpr;
																}]
															}
	| formal_list COMMA datatype id
															{ ({
																	vname = $4;
																	vtype = $3;
																	vinit = Noexpr;
																}) :: $1 
															}

vdecl:
	datatype id SEMICOL 				{ 
																{
																	vname = $2;
																	vtype = $1;
																	vinit = Noexpr;
																}
															}
	| datatype id ASN expr SEMICOL
															{ 
																{
																	vname = $2;
																	vtype = $1;
																	vinit = $4;
																}
															}
/*	datatype id SEMICOL 				{ VarDecl({vtype=$1; vname=$2}) }
	| datatype id ASN expr SEMICOL
															{ AssignDecl({vtype=$1; vname=$2}, $4) }
	| PRIMITIVE_TYPE LBRACK expr RBRACK id SEMICOL
															{ ArrayDecl(type_of_string $1, $3, $5) } */

expr_opt:
	/* nothing */ 							{ Noexpr }
	| expr 											{ $1 }

stmt_list:
	/* nothing */ 							{ [] }
	| stmt_list stmt 						{ $2 :: $1 }

stmt:
	expr SEMICOL 								{ Expr($1) }
	| RETURN expr SEMICOL 			{ Return($2) }
	| LBRACE stmt_list RBRACE 	{ Block(List.rev $2) }
	| IF LPAREN expr RPAREN stmt ELSE stmt
															{ If($3, $5, $7) }
	| IF LPAREN expr RPAREN stmt %prec NOELSE
															{ If($3, $5, EmptyStmt) }
	| FOR LPAREN expr_opt SEMICOL expr_opt SEMICOL expr_opt RPAREN stmt
															{ For($3, $5, $7, $9) }
	| WHILE LPAREN expr RPAREN stmt
															{ While($3, $5) }
	| CONTINUE SEMICOL					{ Continue }
	| BREAK SEMICOL							{ Break }
	| vdecl 										{ VarDeclStmt($1) }
