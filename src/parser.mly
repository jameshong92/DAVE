%{ open Ast %}

%token LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK 
%token SEMICOL COMMA DOT COLON
%token ADDEQ SUBEQ MULEQ DIVEQ MODEQ
%token PLUS MINUS TIMES DIVIDE MOD INC DEC EXP
%token ASN AND OR NOT
%token EQ NEQ LT LEQ GT GEQ 
%token REC FLD TBL
%token CONTINUE BREAK IF ELSE FOR WHILE RETURN VOID
%token INT STR FLOAT BOOL
%token <int> INT_LIT 
%token <float> FLOAT_LIT
%token <bool> BOOL_LIT
%token <string> STR_LIT ID
%token NONE
%token EOF

%nonassoc NOELSE
%nonassoc ELSE
%left COMMA
%right ASN INC DEC ADDEQ SUBEQ MULEQ DIVEQ MODEQ
%left LBRACK
%left OR
%left AND
%left EQ NEQ
%left LT LEQ GT GEQ
%left PLUS MINUS
%left TIMES DIVIDE MOD EXP
%right NOT

%start program
%type <Ast.program> program

%%

/* Expressions */
/* id */
id:
	ID { Id($1) }

/* id[list_expr], id[expr:expr] */
lvalue:
	id { Var($1) }
	| id LBRACK list_expr RBRACK { Array($1, $3) }
	| id LBRACK expr COLON expr RBRACK { ArrayRange($1, $3, $5) }

list_expr:
	expr 									 { [$1] }
	| expr COMMA list_expr { $1 :: $3 }

assign_expr:
	id ASN expr SEMICOL 	{ Assign($1, $3) }

expr:
	| expr PLUS expr 			{ Binop($1, Add, $3) }
	| expr MINUS expr 		{ Binop($1, Sub, $3) }
	| expr TIMES expr 		{ Binop($1, Mul, $3) }
	| expr DIVIDE expr 		{ Binop($1, Div, $3) }
	| expr MOD expr 			{ Binop($1, Mod, $3) }
	| expr EXP expr 			{ Binop($1, Exp, $3) }
	| expr EQ expr 				{ Binop($1, Equal, $3) }
	| expr NEQ expr 			{ Binop($1, Neq, $3) }
	| expr LT expr 				{ Binop($1, Lt, $3) }
	| expr GT expr 				{ Binop($1, Gt, $3) }
	| expr LEQ expr 			{ Binop($1, Leq, $3) }
	| expr GEQ expr 			{ Binop($1, Geq, $3) }
	| expr AND expr 			{ Binop($1, And, $3) }
	| expr OR expr 				{ Binop($1, Or, $3) }

	| INT_LIT 						{ IntLit($1) }
	| FLOAT_LIT 					{ FloatLit($1) }
	| STR_LIT 					{ StringLit($1) }
	| BOOL_LIT 						{ BoolLit($1) }

program:
  /* nothing */ { { stmts = []; funcs = [] } }