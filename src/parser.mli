type token =
  | NEW
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | LBRACK
  | RBRACK
  | SEMICOL
  | COMMA
  | DOT
  | COLON
  | ADDEQ
  | SUBEQ
  | MULEQ
  | DIVEQ
  | MODEQ
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | MOD
  | INC
  | DEC
  | EXP
  | ASN
  | AND
  | OR
  | NOT
  | EQ
  | NEQ
  | LT
  | LEQ
  | GT
  | GEQ
  | CONTINUE
  | BREAK
  | IF
  | ELSE
  | FOR
  | WHILE
  | RETURN
  | FLD
  | TBL
  | REC
  | VOID
  | STR
  | BOOL
  | INT
  | FLOAT
  | INT_LIT of (int)
  | FLOAT_LIT of (float)
  | BOOL_LIT of (bool)
  | STR_LIT of (string)
  | ID of (string)
  | NONE
  | EOF

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
