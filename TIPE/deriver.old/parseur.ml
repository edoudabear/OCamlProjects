open Deriveur;;
exception SyntaxError of string;;
type operation= Pow | Mult | Div | Add | Sub;; (* !! Les types opératoires sont différents dans deriveur.ml et parseur.ml. Il faudra les fusionner !!*)
type tokens=Fonction of string*int | OuvrParenthese | FermParenthese | OuvrAccolade | FermAccolade | Valeur of float | X | Op of operation ;;

let match_symbols lst typeOuvrant typeFermant = (* Détermine l'emplacement d'une sous-expression *)
  let s=Stack.create () in
  let rec findMatch_aux = function
  | [] -> raise (SyntaxError "braces do not match..")
  | h :: t when h=typeFermant -> if Stack.is_empty s then 0 else (let _=Stack.pop s in 1 + findMatch_aux t)
  | h::t -> (if h=typeOuvrant then Stack.push 42 s); 1+ findMatch_aux t
  in findMatch_aux lst
  ;;

let matchParenthese lst=match_symbols lst OuvrParenthese FermParenthese;;
let matchAccolade lst=match_symbols lst OuvrAccolade FermAccolade;;

let op_of = function
| Op op->op
| _ -> raise (SyntaxError "Internal error : invalid op")

let rec gen_substract k lst=match k,lst with (* extrait les k+1 premiers éléments d'une liste *)
| 0,h::t -> [h],t
| k,h::t when k>0 -> let a,b=gen_substract (k-1) t in h::a,b
| _ -> raise (SyntaxError "expression extraction failed (list too short)");;  

let rec read_expression lst=match read_term lst with
| tree,[]->tree
| tree,Op Add::t -> Node (Sum,tree,read_expression t)
| tree,Op Sub::t -> Node (Diff,tree,read_expression t) (* à surveiller de près, dans le code original, il y avait un "changeSign" devant read_expression pour Sub*)
| _ -> raise (SyntaxError "expr error")



and read_term lst=if lst=[] then raise (SyntaxError "term error") else
let p,l=(read_fact lst) in let p=ref p in
let rec read_aux =function
| op::t when op=Op Mult || op=Op Div-> let fact,r=read_fact t in p:=Node(op_of op,!p,fact);read_aux r
(*| op::b::t when op=Op Mul || op=Op Div-> p:=Node(!p,op_of op,Leaf (val_of b));read_aux t *)
| l -> !p,l
in read_aux l


and read_fact_exp lst = match read_fact lst with
|tree,Op Pow::t-> tree,Op Pow::t (* Il faut revoir la structure de Pow *)
|tree,l->tree,l



and read_fact = function
| (Op Sub)::(Valeur a)::t -> Leaf [-.a,0],t
| Valeur x::t -> Leaf [x,0],t
| X::t -> Leaf [1.,1],t
| OuvrParenthese::t -> let a,b=gen_substract ((matchParenthese t)-1) t in begin match b with
                                               | FermParenthese::[] -> read_expression a,[]
                                               | FermParenthese::(Op op)::t ->let f,l'=read_fact t in Node (read_expr a,op,f),l' (* Pour les divisions et compagnie, il faut ruser. *)
                                               | _ -> raise (SyntaxError "Fact error (probably a bracket error)")
end
| _ -> raise (SyntaxError "fact error")





and read_function lst=if lst=[] then failwith "RF error" else (* Code inspiré du TP lexer *)
match lst with
| (Fonction (str,1))::OuvrAccolade::t when isFunRegistered str -> begin
      let a,b=gen_substract ((matchAccolade t)-1) t in begin match b with
      | FermAccolade::[] -> Fonction (str,read_expression a),[]
      | FermAccolade::(Op op)::t ->let r,l'=read_fact t in Node (Fonction (str,read_expression a),op,r),l' (* Pour les divisions et compagnie, il faut ruser. *)
      | _ -> raise (SyntaxError "Fact error (probably a bracket error)")
      end
| (Fonction (str,1))::exp::t when isFunRegistered str ->Fonction (str,read_expression [exp]),t
| Fonction ("frac",2)::OuvrAccolade::t ->begin
      let a,b=gen_substract ((findMatch t)-1) t in begin match b with
      | FermAccolade::[] -> raise (SyntaxError "Missing argument for function '"^str^"'")
      | FermAccolade::exp::t ->let r,_=read_fact [exp] in Node (Quotient,read_expression a,r),l' (* Pour les divisions et compagnie, il faut ruser. *)
      | FermAccolade::OuvrAccolade::t2 -> begin
            let a2,b2=gen_substract ((findMatch t2)-1) t in begin match b2 with
            | FermAccolade::[] -> Node (Quotient,read_expression a,read_expression b2),[]
            | FermAccolade::(Op op)::t ->let r,l'=read_fact t in Node (Node (Quotient,read_expression a,read_expression b2),op,r),l' (* Pour les divisions et compagnie, il faut ruser. *)
            | _ -> raise (SyntaxError "Fact error (probably a bracket error)")
            end
      end
      | _ -> raise (SyntaxError "Fact error (probably a bracket error)")
      end
| _ -> failwith "RF error"
;;

let isFunRegistered str=match str with
| "cos" | "sin" | "tan" | "exp" | "ln" -> true
| _ -> false;;

(* Deriver grammar
*fonction :  "sin{"expresion"}" | "cos{"expresion"}" | "exp{"expresion"}" | "tan{"expresion"}" | "frac{"expression"}{"expression"}"
*facteur : ["0".."9"] liste | "x" | "(" expression ")" | "\"fct
*facteur_exposant : facteur [["^" expression à un seul caractère] ["^{expression}"] ]*
*terme : facteur_exposant ["\times" "*" "/" expression]*
*expression : terme ["+" "-" terme]* 
*)