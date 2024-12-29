{
open Parser
}

rule token = parse
| "let" { LET }
| "in" { IN }
| ":=" { ASSIGN }
| "λ" { LAM }
| '.' { DOT }
| "()" { IDENT("tt") }
| '(' { LPAREN }
| ')' { RPAREN }
| ';' { SEMICOLON }
| [' ' '\t' '\n']+ { token lexbuf }
| ['a'-'z' 'A'-'Z' '_' '0'-'9']+ as s { IDENT(s) }
| '@' ['a'-'z' 'A'-'Z' '_' '0'-'9' ]+ as s { EXTERNAL_IDENT(String.sub s 1 (String.length s - 1)) }
| eof { EOF }
| _ as c { failwith (Printf.sprintf "unexpected character: %C" c) }
