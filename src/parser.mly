%{ open Ast %}

%token LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK 
%token SEMICOL COMMA DOT COLON
%token ADDEQ SUBEQ MULEQ DIVEQ MODEQ
%token PLUS MINUS TIMES DIVIDE MOD INC DEC
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
%left TIMES DIVIDE MOD
%right NOT

%start program
%type <Ast.program> program

%%
