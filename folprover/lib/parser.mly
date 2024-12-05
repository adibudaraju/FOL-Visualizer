%{
open Fol
%}

%token <string> ID
%token LPAREN RPAREN COMMA FORALL EXISTS IFF IMPLIES AND OR NOT EOF

%nonassoc FORALL EXISTS
%left IFF
%right IMPLIES
%left OR
%left AND
%nonassoc NOT

%start <Fol.formula> goal

%%

goal: 
| formula EOF {$1}

formula:
| formula IFF formula {Cv (And, Cv (Or, Not $1, $3), Cv (Or, $1, Not $3))}
| formula IMPLIES formula {Cv (Or, Not $1, $3)}
| formula OR formula {Cv (Or, $1, $3)}
| formula AND formula {Cv (And, $1, $3)}
| NOT formula {Not $2}
| FORALL ID COMMA formula %prec FORALL {Qf (Forall, ($2,0), $4)}
| EXISTS ID COMMA formula %prec EXISTS {Qf (Exists, ($2,0), $4)}
| ID LPAREN separated_list(COMMA, term) RPAREN {Rel ($1, $3)}
| LPAREN formula RPAREN {$2}
;

term:
| ID {Var ($1, 0)}
| ID LPAREN separated_list(COMMA, term) RPAREN {Fun ($1, $3)}
;
