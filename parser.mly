
%token <string> IDENT
%token <string> EXTERNAL_IDENT
%token LAM
%token DOT
%token LPAREN
%token RPAREN
%token EOF

%start main

%type <Term.term> main

%type <string> IDENT

%%

main:
  term EOF { $1 }

term:
  IDENT { Var($1) }
| EXTERNAL_IDENT { Extern($1) }
| LPAREN term RPAREN { $2 }
| LAM IDENT DOT term { Abs($2, $4) }
| term term { Ap($1, $2) }

%%
