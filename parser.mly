%{
open Term
%}
%token <string> IDENT
%token <string> EXTERNAL_IDENT
%token LAM
%token DOT
%token LPAREN
%token RPAREN
%token EOF
%token LET
%token IN
%token ASSIGN
%token SEMICOLON

%start main

%type <Term.term> main

%nonassoc LAM
%nonassoc IN
%nonassoc IDENT EXTERNAL_IDENT
%nonassoc SEMICOLON
%nonassoc LPAREN
%nonassoc ap_prec

%%

main:
  term EOF { $1 }

term2:
| t=term { t }

term:
| i=IDENT { Var(i) }
| i=EXTERNAL_IDENT { Extern(i) }
| LET x=IDENT ASSIGN v=term IN e=term %prec IN { Ap(Abs(x, e), v) }
| LPAREN t=term RPAREN { t }
| LAM x=IDENT DOT e=term %prec LAM { Abs(x, e) }
| e1=term SEMICOLON e2=term2 %prec SEMICOLON { Ap(Abs("_", e2), e1) }
| e1=term e2=term %prec ap_prec { Ap(e1, e2) }

%%
