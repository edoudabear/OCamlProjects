/* file: calc.mly */
/* Infix notatoin calculator -- calc */
%{
open Printf
%}
/* Ocamlyacc Declarations */
%token NEWLINE
%token L_PAREN R_PAREN
%token <float> NUM
%token PLUS MINUS MULTIPLY DIVIDE CARET QUOTIENT TEX_BRACE_OPEN TEX_BRACE_CLOSE X
%left PLUS MINUS
%left MULTIPLY DIVIDE
%left NEG /* negation -- unary minus */
%right CARET /* exponentiation */
%start input
%type <Deriveur.expression> input
%%
input:
| exp NEWLINE { $1 }
;
exp: 
/* 
Déjà réglé : Il faut traîter la question du signe négatif dans un arbre.. fait !
Question des fonctions à régler
Priorité des parenthèses, accolades etc
*/
| NUM { Deriveur.Leaf($1) }
| NUM X { Deriveur.Node(Deriveur.Mult,Deriveur.Leaf $1,Deriveur.X) }
| exp PLUS exp { Deriveur.Node(Deriveur.Sum,$1,$3) }
| exp MINUS exp { Deriveur.Node(Deriveur.Diff,$1,$3) }
| exp MULTIPLY exp { Deriveur.Node(Deriveur.Mult,$1,$3) }
| exp DIVIDE exp { Deriveur.Node(Deriveur.Quotient,$1,$3) }
| MINUS exp %prec NEG { Deriveur.Node(Deriveur.Diff,Deriveur.Leaf 0.,$2) }
| exp CARET exp { Deriveur.Fonction("exp",Deriveur.Node(Deriveur.Mult,$3,Deriveur.Fonction("ln",$1))) }
| L_PAREN exp R_PAREN { $2 }
| X { Deriveur.X }
| QUOTIENT  TEX_BRACE_OPEN exp TEX_BRACE_CLOSE TEX_BRACE_OPEN  exp TEX_BRACE_CLOSE { Deriveur.Node(Deriveur.Quotient,$3,$6) }
;
%%
