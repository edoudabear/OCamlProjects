type oper= Add | Sub | Mul | Div;;
type expr = Leaf of float
          | Node of expr*oper*expr;;

let a1=Node (Node (Leaf 2.,Mul,Leaf 3.),Add,Leaf 4.)
and a2=Node (Leaf 2.,Mul,Node (Leaf 3.,Add,Leaf 4.))
and a3=Node (Leaf 2.,Sub,Node (Leaf 3.,Add,Leaf 4.));;

let rec eval=function
| Leaf a-> a
| Node (a,op,b)->begin match op with
                 | Add -> eval a +. eval b
                 | Mul -> eval a *. eval b
                 | Sub -> eval a -. eval b
                 | Div -> eval a /. eval b
                 end;;

let rec print_expr = function
| Leaf a-> print_float a
| Node (a,op,b)->begin match op with
                 | Add -> Printf.printf "(" ;print_expr a;Printf.printf ")+(" ; print_expr b;Printf.printf ")" ;
                 | Mul -> Printf.printf "(" ;print_expr a;Printf.printf ")*(" ; print_expr b;Printf.printf ")" ;
                 | Sub -> Printf.printf "(" ;print_expr a;Printf.printf ")-(" ; print_expr b;Printf.printf ")" ;
                 | Div -> Printf.printf "(" ;print_expr a;Printf.printf ")/(" ; print_expr b;Printf.printf ")" ;
                 end;;

let rec simplify = function
| Leaf a -> Leaf a
| Node (a,op,b)->begin match op with
                | Add | Sub | Div -> Node (a,op,b)
                | Mul -> begin match a,b with
                        | Node (a,Add,b),Node(a',Add,b') -> Node (Node(Node (a,Mul,a'),Add,Node (a,Mul,b')),Add,Node(Node (b,Mul,a'),Add,Node (b,Mul,b')))
                        | _ -> Node (a,op,b)
                end
              end;;

(* RPN *)
type elem=Val of float | Op of oper | Var of string | AddedSub | Open | Close | Let of string | Delete of string  ;;

let a1'=[Val 2.;Val 3.;Op Mul;Val 4.;Op Add];;

let eval_polinv lst=
  let s=Stack.create () in
  let rec eval_aux = function
  | [] -> begin try Stack.pop s with
          | Stack.Empty -> 0. end
  | (Val a)::t -> Stack.push a s;eval_aux t
  | (Op op)::t -> begin let x=Stack.pop s in
                  let y=Stack.pop s in
                  match op with
                    | Add -> Stack.push (x+.y) s;eval_aux t
                    | Sub -> Stack.push (y-.x) s;eval_aux t
                    | Mul -> Stack.push (x*.y) s;eval_aux t
                    | Div -> Stack.push (x/.y) s;eval_aux t
            end
  | _ -> failwith "data Structure is not well compiled (contains Open and Close)"
    in eval_aux lst;;

let rec affiche_arbre_polinv=function
| Leaf a -> print_float a
| Node (a,op,b)->begin match op with
| Add -> affiche_arbre_polinv a; affiche_arbre_polinv b;Printf.printf "+"
| Mul -> affiche_arbre_polinv a; affiche_arbre_polinv b;Printf.printf "*"
| Sub -> affiche_arbre_polinv a; affiche_arbre_polinv b;Printf.printf "-"
| Div -> affiche_arbre_polinv a; affiche_arbre_polinv b;Printf.printf "/"
end;;

let rec polinv_of_arbre=function
| Leaf a -> [Val a]
| Node (a,op,b)-> polinv_of_arbre a @ polinv_of_arbre b @ [Op op];;

let rec arbre_of_polinv lst=
  let s=Stack.create () in
  let rec aux = function
  | [] -> begin try Stack.pop s with
          | Stack.Empty -> failwith "Votre structure est sus (ITWT)" end
  | (Val a)::t -> Stack.push (Leaf a) s;aux t
  | (Op op)::t -> begin let x=Stack.pop s in
                  let y=Stack.pop s in
                  match op with
                    | Add -> Stack.push (Node (y,Add,x)) s;aux t
                    | Sub -> Stack.push (Node (y,Sub,x)) s;aux t
                    | Mul -> Stack.push (Node (y,Mul,x)) s;aux t
                    | Div -> Stack.push (Node (y,Div,x)) s;aux t
            end
  | _ -> failwith "data Structure is not well compiled (contains Open and Close)"
in aux lst;;


(*
 Global Vars for Project
*)
exception SyntaxError of string;;
exception ScopeError of string;;

let letVars=ref [];;
let forbidden=['.';'/';'(';')';'{';'}';'e';'E';'+';'=';'-';'*';'/'];;
let forbiddenNames=["print";"eval";"_h"];;
let digitsNotation=['0';'1';'2';'3';'4';'5';'6';'7';'8';'9';'.';'e';'E'];;
let forbiddenAsFirstVarChar='h'::digitsNotation;;
let chars=['+';'-';'/';'*';'(';')'];;

let isAVarName str i=
  let rec aux str i= function
  | [] -> ""
  | (s,_)::t when ((String.length str)>=(String.length s) + i) && (String.sub str i (String.length s)=s) -> begin match aux str i t with | s2 -> if (String.length s2)>(String.length s) then s2 else s end
  | _::t -> aux str i t
  in aux str i (!letVars)
;;

let float_of_string_i str = if String.fold_left (fun h v -> h || not (List.mem v digitsNotation)) false str then raise (SyntaxError ("Unbound var "^str))
else float_of_string str;;

let tokenize str=
    let i=ref 0 and min_i=ref 0 and lst=ref [] and n=String.length str in
    while (!i)<n do
      let varName=isAVarName str (!i) in
      if varName<>"" then begin
        lst:=Var varName::(!lst);
        i:=(!i)+(String.length varName);
        min_i:=!i;
      end else if (List.mem (str.[!i]) chars) then begin
        if (!min_i)<>(!i) then lst:=(Val (String.sub str (!min_i) ((!i)-(!min_i)) |> float_of_string_i))::(!lst) else ();
        (match String.sub str (!i) (1) with
        | "+" -> lst:=(Op Add)::(!lst);
        | "-" -> lst:=(Op Sub)::(!lst);
        | "/" -> lst:=(Op Div)::(!lst);
        | "*" -> lst:=(Op Mul)::(!lst);
        | "(" -> lst:=(Open)::(!lst);
        | ")" -> lst:=(Close)::(!lst);
        | _ -> failwith "Incomplete implementation..");
        min_i:=(!i)+1; incr i;
      end else if (!i)<n-4 && String.sub str (!i) 4="let " then begin
        min_i:=!i+4;
        while (!i)<n && str.[!i]!='=' do
          incr i;
        done;
        if str.[!i]<>'=' then raise (SyntaxError "Could not match assignment operator '='") 
        else if !lst<>[] then raise (SyntaxError "Cannot make a let statement in the middle of an expression")
        else lst:=(Let (String.sub str (!min_i) (!i-(!min_i))))::!lst; incr i; min_i:=!i;
      end else if (!i)<n-7 && String.sub str (!i) 7="delete " then begin
        min_i:=!i+7;
        while (!i)<n do
          incr i;
        done;
        if !lst<>[] then raise (SyntaxError "Cannot make a delete statement in the middle of an expression") 
        else lst:=(Delete (String.sub str (!min_i) (!i-(!min_i))))::!lst; incr i; min_i:=!i;
      end else begin
        if !i=n-1 then begin
          try let x=String.sub str (!min_i) ((!i)-(!min_i)+1) |> float_of_string_i in
          lst:=(Val x)::(!lst)
          with
          | Failure _ -> raise (SyntaxError "Could not parse expression") ;
        end;
        incr i
      end
    done;
    List.rev (!lst)
;;

(**
# facteur :: flottant
# terme :: facteur [('*','/') facteur]*
# expression :: terme [('+','-') terme]*
*)


(*let read_term lst= 
  let rec aux acc=function
  | [] -> Node (Leaf 0.,Add,Leaf 0.),[]
  | a::b::t when b=Op Mul || b=Op Div -> let x,r=read_fact t in begin match a,b with
                                                            | Val a',Op o -> Node(Leaf a',o, x),r
                                                            | _ -> raise SyntaxError end
  | l -> (read_fact l);;
*)

let op_of=function | Op a -> a | _ -> failwith "not expected in op_of";;
let val_of= function | Val a -> a | _ -> failwith "not expected in val_of";;

let findMatch lst = (* Détermine l'emplacement d'une sous-expression *)
  let s=Stack.create () in
  let rec findMatch_aux = function
  | [] -> raise (SyntaxError "Parentheses do not match..")
  | Close :: t -> if Stack.is_empty s then 0 else (let _=Stack.pop s in 1 + findMatch_aux t)
  | h::t -> (if h=Open then Stack.push 42 s); 1+ findMatch_aux t
  in findMatch_aux lst
  ;;

let changeSign lst =
  let s=Stack.create () in
  let rec aux= function
  | [] -> []
  | Open::t -> Stack.push 42 s; Open::aux t
  | Close::t -> let _=Stack.pop s in Close::aux t
  | Op Add::t -> if Stack.is_empty s then Op Sub::aux t else Op Add::aux t
  | Op Sub::t -> if Stack.is_empty s then Op Add::aux t else Op Sub::aux t
  | h::t -> h::aux t
  in aux lst;;

let rec gen_substract k lst=match k,lst with (* extrait les k+1 premiers éléments d'une liste *)
| 0,h::t -> [h],t
| k,h::t when k>0 -> let a,b=gen_substract (k-1) t in h::a,b
| _ -> raise (SyntaxError "')' not found");;  

let getVar str=
  let rec aux=function
  | [] -> raise (SyntaxError ("Unbound value "^str))
  | (s,tr)::t when s=str-> tr
  | _::t -> aux t
  in aux (!letVars)
;; 

let rec read_fact = function
| (Op Sub)::(Val a)::t -> Node (Leaf 0.,Sub,Leaf a),t
| Val x::t -> Leaf x,t
| Var str::t -> getVar str,t
| Open::t -> let a,b=gen_substract ((findMatch t)-1) t in begin match b with
                                               | Close::[] -> read_expr a,[]
                                               | Close::(Op op)::t ->let f,l'=read_fact t in Node (read_expr a,op,f),l' (* Pour les divisions et compagnie, il faut ruser. *)
                                               | _ -> raise (SyntaxError "Fact error (probably a bracket error)")
end
| _ -> raise (SyntaxError "Fact error")
and read_term lst=
  if lst=[] then failwith "ITWT" else
  let p,l=(read_fact lst) in let p=ref p in
  let rec read_aux =function
  | op::t when op=Op Mul || op=Op Div-> let fact,r=read_fact t in p:=Node(!p,op_of op,fact);read_aux r
  (*| op::b::t when op=Op Mul || op=Op Div-> p:=Node(!p,op_of op,Leaf (val_of b));read_aux t *)
  | l -> !p,l
in read_aux l
and read_expr lst=match read_term lst with
| tree,[]->tree
| tree,Op Add::t -> Node (tree,Add,read_expr t)
| tree,Op Sub::t -> Node (tree,Sub,t|> changeSign |>read_expr)
| _ -> raise (SyntaxError "expr error");;

let isForbidden s=List.mem (s.[0]) (forbiddenAsFirstVarChar) || String.fold_left (fun p c-> p || (List.mem c forbidden)) false s || List.mem (String.trim s) forbiddenNames;;


let let_Statement =function
| Let s::t when String.trim s<>s -> raise (SyntaxError "var cannot contain spaces !")
| Let s::t when isForbidden s -> raise (SyntaxError  ((List.fold_left (fun s v->s^" "^(String.make 1 v)) "This label belongs to the forbidden names : " (forbidden))^"\nIt should also not start with the following characters :"^(List.fold_left (fun s v->s^" "^(String.make 1 v)) "" (forbiddenAsFirstVarChar))))
| Let s::t->let sV=read_expr t in letVars:=(s,sV)::!letVars; sV
| _ -> raise (SyntaxError "Invalid let statement..")
;;

let delete_statement=function
| Delete label::t -> if isForbidden label then raise (SyntaxError  ((List.fold_left (fun s v->s^" "^(String.make 1 v)) "This label belongs to the forbidden names : " (forbidden))^"\nIt should also not start with the following characters :"^(List.fold_left (fun s v->s^" "^(String.make 1 v)) "" (forbiddenAsFirstVarChar))))
                  else if String.trim label<>label then raise (SyntaxError "Var cannot contain spaces !")
                  else let rec aux =function
                  | (h,d)::t when label=h -> t,d
                  | h::t -> let l,d=aux t in h::l,d
                  | [] -> raise (SyntaxError "This variable does not exist (anymore)")
                  in let newlist,d=(aux (!letVars)) in letVars:=newlist;d
| _ -> raise (SyntaxError "Invalid remove statement");;

let interpreter = function
| Let a::t -> let_Statement (Let a::t)
| Delete a::t -> delete_statement ((Delete a)::t)
| a -> read_expr a
;;

let compile str = str |> tokenize |> interpreter;;

let print_intro () = Printf.printf "Diskloud™ Parser • Feb 2022 • Edouard Aubert\n" ;;

let print_help () = 
print_intro () ;
Printf.printf
"Doc :
let [var_name]=[exp] -> store value [exp] in [var_name]
delete [var_name] -> deletes [var_name]'s value
[expr] -> evaluates an expression and returns its value
print [expr] -> prints out an expression without computing it
_h [strictly positive int] -> executes nth previous command that has been run before
exit -> to exit the program\n" ;;

let histoVals=ref [];;

let rec run_prev_command i =
  if i<1 then Printf.printf "0 or less is not an acceptable index !";
  let rec aux k=function
  | [] -> Printf.printf "Command index out of bounds..\n";
  | h::t when k=0 && (String.length h > 4) && String.sub h 0 2="_h" -> Printf.printf "Cannot run recursive history commands\n";
  | h::t when k=0 -> run h
  | h::t -> aux (k-1) t
  in aux (i-1) (!histoVals)
and run str=match str with
| str when String.trim str="" -> ()
| str when (String.length str >= 4) && String.sub str 0 2="_h" -> begin try histoVals:=List.tl (!histoVals); run_prev_command (int_of_string (String.sub str 3 (String.length str - 3))) with | _ ->  Printf.printf "Invalid history index\n"; end
| "exit" -> Printf.printf "Bye !\n";exit 0;
| "help" -> print_help ();
| str when str.[0]=' ' -> Printf.printf "Do not put spaces at the beginning of your statement please..\n";
| str when (String.length str > 5) && String.sub str 0 5="print" -> (try String.sub str 6 ((String.length str) - 6) |> compile |> print_expr;Printf.printf "\n" with | SyntaxError s -> Printf.printf "Fail by eval : %s \n" s);
| str when (String.length str > 4) && String.sub str 0 4="eval" -> (try Printf.printf "%f\n" (String.sub str 5 ((String.length str) - 5) |> compile |> eval) with | SyntaxError s -> Printf.printf "Fail by printing expression : %s \n" s);
| str -> (try Printf.printf "%f\n" (str |> compile |> eval) with | SyntaxError s -> Printf.printf "Fail : %s \n" s);
;;

let rec main () = run (Printf.printf "[Kin]>";histoVals:=read_line ()::(!histoVals);List.hd !histoVals);main () ;;

let () = print_intro ();main ();;
