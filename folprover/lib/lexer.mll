{
open Parser
}

rule token = 
parse
| [' ''\t''\n']+ {token lexbuf}
| '(' {LPAREN}
| ')' {RPAREN}
| ',' {COMMA}
| "<->" {IFF}
| "->" {IMPLIES}
| "&&" {AND}
| "||" {OR}
| "~" {NOT}
| "forall" {FORALL}
| "exists" {EXISTS}
| ['a'-'z''A'-'Z''_']+ as id { ID id }
| eof {EOF}
