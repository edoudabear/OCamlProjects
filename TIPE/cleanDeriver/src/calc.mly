/* fichier: calc.mly */
%{
open Printf
%}
/* Déclarations Ocamlyacc */
/* e^x+2sin(x)+5.25tan(x^2+3/2x+1) */
%token NEWLINE
%token L_PAREN R_PAREN
%token <bool*int*int> NUM
%token PLUS MINUS MULTIPLY DIVIDE CARET QUOTIENT TEX_BRACE_OPEN TEX_BRACE_CLOSE X COMPLEX_I EXPONENTIAL E_POW SIN COS TAN
%left PLUS MINUS
%left NEG /* négation -- moins unaire */
%left DIVIDE
%left MULTIPLY
%left EXPONENTIAL E_POW SIN COS TAN
%right CARET /* exponentiation */
%start input
%type <Deriveur.expression_etendue> input
%%
input:
    | exp NEWLINE { $1 }
;
exp: 
/* 
To Do List :
Opérateur fois par défaut
*/
    /* Traîtement rationnels (imaginaire pur ou réel)*/
    | NUM { Deriveur.Leaf_e $1 }
    | COMPLEX_I NUM {Deriveur.make_complex_leaf_e $2}
    | NUM COMPLEX_I {Deriveur.make_complex_leaf_e $1}
    | COMPLEX_I { Deriveur.Leaf_e (true,1,1)}
    /* Indéterminée formelle x */
    | NUM X { Deriveur.Node_e(Deriveur.Mult_e,Deriveur.Leaf_e $1,Deriveur.Puissance_e (false,1,1)) }
    /* Fonctions trigonométriques */
    | COS L_PAREN exp R_PAREN { Deriveur.Fonction_e (Cos_e,$3) }
    | SIN L_PAREN exp R_PAREN { Deriveur.Fonction_e (Sin_e,$3) }
    | TAN L_PAREN exp R_PAREN { Deriveur.Fonction_e (Tan_e,$3) }
    | COS TEX_BRACE_OPEN exp TEX_BRACE_CLOSE { Deriveur.Fonction_e (Cos_e,$3) }
    | SIN TEX_BRACE_OPEN exp TEX_BRACE_CLOSE { Deriveur.Fonction_e (Sin_e,$3) }
    | TAN TEX_BRACE_OPEN exp TEX_BRACE_CLOSE { Deriveur.Fonction_e (Tan_e,$3) }

    /* Opérateurs canoniques (+,-,/,- unaire) */
    | exp PLUS exp { Deriveur.Node_e(Deriveur.Sum_e,$1,$3) }
    | exp MINUS exp { Deriveur.Node_e(Deriveur.Sum_e,$1,Node_e(Deriveur.Mult_e,Deriveur.Leaf_e (false,-1,1),$3)) }
    | exp MULTIPLY exp { Deriveur.Node_e(Deriveur.Mult_e,$1,$3) }
    | exp exp %prec MULTIPLY { Deriveur.Node_e(Deriveur.Mult_e,$1,$2) }
    | exp DIVIDE NUM { Deriveur.Node_e(Deriveur.Quotient_e,$1,Leaf_e $3) }
    | exp DIVIDE X { Deriveur.Node_e(Deriveur.Quotient_e,$1,Puissance_e (false,1,1)) }
    | exp DIVIDE L_PAREN exp R_PAREN { Deriveur.Node_e(Deriveur.Quotient_e,$1,$4) }
    | MINUS exp %prec NEG { Deriveur.Node_e(Deriveur.Mult_e,Deriveur.Leaf_e (false,-1,1),$2) }
    /* Gestion des puissances */
    | NUM X CARET NUM { 
        Deriveur.Node_e(
            Deriveur.Mult_e,
            Deriveur.Leaf_e $1,
            Deriveur.Puissance_e $4
        )
    }
    | NUM X CARET TEX_BRACE_OPEN exp TEX_BRACE_CLOSE {
        Deriveur.Node_e(
            Deriveur.Mult_e,
            Deriveur.Leaf_e $1,
            (Deriveur.simplificateurX $5)
        )
    }
    | X CARET TEX_BRACE_OPEN exp TEX_BRACE_CLOSE {
        (Deriveur.simplificateurX $4)
    }
    | X CARET exp { (Deriveur.simplificateurX $3)}
    | exp CARET NUM  {
        Deriveur.simplificateur $1 (Deriveur.Leaf_e $3)
    }
    | exp CARET TEX_BRACE_OPEN exp TEX_BRACE_CLOSE  {
        Deriveur.simplificateur $1 $4
    }
    /*| NUM CARET NUM {
        (Deriveur.pow_rat_e (Deriveur.Leaf_e $1) (Deriveur.Leaf_e $3))
    }
    | COMPLEX_I CARET NUM { Deriveur.pow_rat_e (Deriveur.Leaf_e (true,1,1)) (Deriveur.Leaf_e $3)}*/
    /* Exponentielle */
    | EXPONENTIAL L_PAREN exp R_PAREN { Deriveur.Fonction_e (Exp_e,$3) }
    | E_POW TEX_BRACE_OPEN exp TEX_BRACE_CLOSE { Deriveur.Fonction_e (Exp_e,$3) }
    | E_POW exp { Deriveur.Fonction_e (Exp_e,$2)}

    | L_PAREN exp R_PAREN { $2 }
    | L_PAREN exp R_PAREN exp {Deriveur.Node_e(Deriveur.Mult_e,$2,$4)}
    | X { Deriveur.Puissance_e (false,1,1) }
    | QUOTIENT  TEX_BRACE_OPEN exp TEX_BRACE_CLOSE TEX_BRACE_OPEN  exp TEX_BRACE_CLOSE { Deriveur.Node_e(Deriveur.Quotient_e,$3,$6) }

;
%%
