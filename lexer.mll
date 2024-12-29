{
open Parser
}

rule token = parse
| [' ' '\t' '\n']+ { token lexbuf }
| ['a'-'z' 'A'-'Z']+ as s { IDENT(s) }
| '@' ['a'-'z' 'A'-'Z' '_' ]+ as s { EXTERNAL_IDENT(s) }
| "λ" { LAM }
| '.' { DOT }
| eof { EOF }
| '(' { LPAREN }
| ')' { RPAREN }
| _ as c { failwith (Printf.sprintf "unexpected character: %C" c) }
