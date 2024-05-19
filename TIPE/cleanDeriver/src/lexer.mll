(* file: lexer.mll *)
(* Lexical analyzer returns one of the tokens:
the token NUM of a floating point number,
operators (PLUS, MINUS, MULTIPLY, DIVIDE, CARET, UMINUS),
or NEWLINE. It skips all blanks and tabs, unknown characters
and raises End_of_file on EOF. *)
{
open Calc (* Assumes the parser file is "calc.mly". *)
}
let digit = ['0'-'9']
rule token = parse
| [' ' '\t'] { token lexbuf }
| '\n' { NEWLINE }
| digit+
| "." digit+
| digit+ "." digit* as num
(* Le -49 devant le "int_of_char est pour convertier le CODE CHAR vers l'entier qu'il désigne" *)
{ NUM (let dec=ref false in let (bo,a,b)=String.fold_left (fun (bo,a,b) c->if c=',' || c='.' then begin dec:=true; (bo,a,b) end else if !dec then (bo,10*a+int_of_char c-48,10*b) else (bo,10*a+int_of_char c-48,b)) (false,0,1) num
                            in (bo,a/(Deriveur.gcd a b),b/(Deriveur.gcd a b))) }
| "," digit+
| digit+ "," digit* as num
{ NUM (let dec=ref false in let (bo,a,b)=String.fold_left (fun (bo,a,b) c->if c=',' || c='.' then begin dec:=true; (bo,a,b) end else if !dec then (bo,10*a+int_of_char c-48,10*b) else (bo,10*a+int_of_char c-48,b)) (false,0,1) num
                            in (bo,a/(Deriveur.gcd a b),b/(Deriveur.gcd a b))) }
| '+' { PLUS }
| '-' { MINUS }
| '{' { TEX_BRACE_OPEN }
| '}' { TEX_BRACE_CLOSE }
| '(' { L_PAREN }
| ')' { R_PAREN }
| 'x' { X }
| '*' | "\\times" { MULTIPLY }
| "\\frac" { QUOTIENT }
| '/' { DIVIDE }
| '^' { CARET }
| 'i' { COMPLEX_I }
| "\\exp" | "exp" { EXPONENTIAL}
| "\\sin" | "sin" { SIN }
| "\\cos" | "cos" { COS }
| "\\tan" | "tan" { TAN }
| "e^" { E_POW}
| _ { token lexbuf }
| eof { raise End_of_file }
