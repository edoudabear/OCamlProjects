type operation = Mult | Sum | Diff | Quotient;;
type monome = (float*int);;
type pol = monome list;;
type expression = | Node of (operation*expression*expression) | Fonction of string*expression | Leaf of pol;;
type matrix=float array array;;

let parse str=Leaf [(42.,0)];;


let expr=Node (Sum,Fonction ("cos",Leaf [2.*.3.14,1]),Leaf [(2.5,0);(1.,1)]);;

let expr2=Node (Sum,Fonction ("exp",Fonction("ln",Fonction ("f",Leaf [2.*.3.14,1]))),Leaf [(2.5,0);(1.,1)]);;

let eval_P p x= List.fold_left (fun j (a_k,p)->j+.a_k*.(x**(p |> float_of_int))) (0.) p;;

let rec derivee_P=function
| [] -> []
| (a,b)::t when b=0 -> derivee_P t
| (a,b)::t -> let bf=float_of_int b in (a*.bf,b-1)::derivee_P t;;

let rec sum_P p1 p2=match p1,p2 with
| p,[] -> p
| (x1,p1)::t1,(x2,p2)::t2 when p1=p2 -> (x1+.x2,p1)::sum_P t1 t2
| (x1,p1)::t1,(x2,p2)::t2 when p1<p2 -> (x1,p1)::sum_P t1 ((x2,p2)::t2)
| p1,p2 -> sum_P p2 p1;;

let rec mult_P_M po ((k,p) : monome)=match po with
| [] -> []
| (a,b)::t -> (k*.a,b+p)::mult_P_M t (k,p);;

let rec mult_P p1 p2=match p1 with
| [] -> ([] : pol)
| mon::t -> sum_P (mult_P_M p2 mon) (mult_P t p2);;

let float_to_good_str f=
  let str=string_of_float f in if str.[String.length str-1]='.' then String.sub str 0 (String.length str-1) else str;;

let rec print_P = function
| [] -> ();
| [(a,0)] ->  Printf.printf "%s" (float_to_good_str a);
| [(1.,1)] -> Printf.printf "x";
| [(a,1)] ->  Printf.printf "%sx" (float_to_good_str a);
| [(a,b)] ->  Printf.printf "%sx^%d" (float_to_good_str a) b;
| (a,0)::t -> Printf.printf "%s+" (float_to_good_str a);print_P t;
| (1.,1)::t -> Printf.printf "x"; print_P t;
| (a,1)::t ->  Printf.printf "%sx+" (float_to_good_str a); print_P t;
| (a,b)::t -> Printf.printf "%sx^%d+" (float_to_good_str a) b;print_P t;;

let rec print_P_latex = function
| [] -> ();
| [(a,0)] ->  Printf.printf "%s" (float_to_good_str a);
| [(1.,1)] -> Printf.printf "x";
| [(a,1)] ->  Printf.printf "%s" (float_to_good_str a);
| [(a,b)] ->  Printf.printf "%sx^{%d}" (float_to_good_str a) b;
| (a,0)::t -> Printf.printf "%s+" (float_to_good_str a);print_P t;
| (1.,1)::t -> Printf.printf "x"; print_P t;
| (a,1)::t ->  Printf.printf "%sx+" (float_to_good_str a); print_P t;
| (a,b)::t -> Printf.printf "%sx^{%d}+" (float_to_good_str a) b;print_P t;; 

let rec isEmpty = function
| [] -> true
| (0.,_)::t-> isEmpty t
| _ -> false;;

let rec print_expression = function
| Node (op,exp1,exp2) -> begin match op with
                        | Sum -> print_expression exp1 ; print_string "+" ; print_expression exp2; ()
                        | Diff -> print_expression exp1 ; print_string "-" ; print_expression exp2; ()
                        | Mult -> Printf.printf "(" ;print_expression exp1 ; Printf.printf ")*(" ; print_expression exp2; Printf.printf ")" ; ()
                        | Quotient -> Printf.printf "(" ;print_expression exp1 ; Printf.printf ")/(" ; print_expression exp2; Printf.printf ")" ; ()
                        end
(*| Pow (k,exp) -> print_string "(";print_expression exp;Printf.printf ")^%s " (float_to_good_str k); *)
| Fonction (f,exp1) -> print_string f;print_string "(";print_expression exp1 ;print_string ")"; ()
| Leaf po -> if isEmpty po then Printf.printf "0" else print_P_latex po; ();;

let rec print_exp_latex=function
| Node (op,exp1,exp2) -> begin match op with
          | Sum -> print_exp_latex exp1 ; print_string "+" ; print_exp_latex exp2; ()
          | Diff -> print_exp_latex exp1 ; print_string "-" ; print_exp_latex exp2; ()
          | Mult -> Printf.printf "(" ;print_exp_latex exp1 ; Printf.printf ")\\time(" ; print_exp_latex exp2; Printf.printf ")" ; ()
          | Quotient -> Printf.printf "\\frac{" ;print_exp_latex exp1 ; Printf.printf "}{" ; print_exp_latex exp2; Printf.printf "}" ; ()
          end
| Fonction (f,exp1) -> print_string f;print_string "(";print_exp_latex exp1 ;print_string ")"; ()
| Leaf po -> if isEmpty po then Printf.printf "0" else print_P_latex po; ();;

let derivee_f f exp1=match f with
| "sin"-> Fonction ("cos",exp1)
| "cos" -> Node (Mult,Leaf [(-1.,0)],Fonction ("sin",exp1))
| "exp" -> Fonction ("exp",exp1)
| _ -> Fonction (f^"'",exp1);;

let rec minus_P p=List.map (fun (a,b) -> (-.a,b)) p;;

let rec clean = function
| Node (Sum,Leaf b,Leaf a)  -> begin let iEa=isEmpty a and iEb=isEmpty b in match iEa,iEb with
                              | true,true -> Leaf []
                              | true,false -> Leaf a
                              | false,true -> Leaf b
                              | false,false-> Node (Sum,Leaf b,Leaf a)
                              end
| Node (Sum,Leaf a,b) -> if isEmpty a then clean b else Node (Sum,Leaf a,b)
| Node (Sum,a,Leaf b) -> clean (Node (Sum,Leaf b,a))

| Node (Diff,Leaf b,Leaf a) -> begin let iEa=isEmpty a and iEb=isEmpty b in match iEa,iEb with
                              | true,true -> Leaf []
                              | true,false -> Leaf a
                              | false,true -> Leaf (minus_P b)
                              | false,false-> Node (Diff,Leaf b,Leaf a)
                              end
| Node (Diff,Leaf a,b) -> if isEmpty a then Node(Mult,Leaf [(-1.,0)],clean b) else Node (Diff,Leaf a,b)
| Node (Diff,a,Leaf b) -> if isEmpty b then clean a else Node (Diff,clean a,Leaf b)

| Node (Mult,Leaf a,Leaf b) -> begin let iEa=isEmpty a and iEb=isEmpty b in match iEa || iEb with
                              | true -> Leaf []
                              | false -> Node (Mult,Leaf a,Leaf b)
                              end
| Node (Mult,Leaf a,b) -> if isEmpty a then Leaf [] else Node (Mult,Leaf a,clean b)
| Node (Mult,a,Leaf b) -> clean (Node (Mult,Leaf b,clean a))

| Node (Quotient,Leaf a,Leaf b) -> begin let iEa=isEmpty a and iEb=isEmpty b in 
                                        if iEb then failwith "Pas derivable" else
                                        if iEa && iEb then Leaf [] else
                                          Node (Quotient,Leaf a,Leaf b)
                                        end
| Node (Quotient,Leaf a,b) -> if isEmpty a then Leaf [] else Node (Quotient,Leaf a,clean b)
| Node (Quotient,a,Leaf b) -> if isEmpty b then failwith "Pas derivable" else Node (Quotient,clean a,Leaf b)
| Node (op,a,b) -> Node (op,clean a,clean b)
| Fonction (f,Leaf a) -> Fonction (f,Leaf a)
| Fonction (f, a) -> Fonction (f,clean a)
| Leaf a -> Leaf a;;

let deriveur expr =
  let rec deriveur=function
  | Node (op,exp1,exp2) -> begin match op with 
                          | Sum | Diff -> Node (op,deriveur exp1,deriveur exp2)
                          | Mult -> Node (Sum,Node (Mult,exp1,deriveur exp2),Node (Mult,deriveur exp1,exp2))
                          | Quotient -> Node (Quotient,
                                          Node (Diff,
                                            Node (Mult,deriveur exp1,exp2),
                                            Node (Mult,exp2,deriveur exp1)
                                          )
                                          ,Fonction("exp",Node (Mult,Leaf [2.,0],Fonction("ln",(exp2))))
                                        )
                          end
  (*| Pow (k,exp1) -> begin match k with
                    | 0.-> failwith "Ce cas n'est pas encore traîté.."
                    | 1.-> deriveur exp1
                    | k->Node (Mult,Leaf [k,0],(Pow (k-.1.,deriveur exp1))) 
                    end*)
  | Fonction (f,exp1) -> Node (Mult,deriveur exp1,derivee_f f exp1)
  | Leaf p-> Leaf (derivee_P p) in expr |> deriveur |> clean;;

let rec eval f x= match f with
| Node (Sum,exp1,exp2)  -> eval exp1 x +. eval exp2 x
| Node (Diff,exp1,exp2) ->  eval exp1 x -. eval exp2 x
| Node (Mult,exp1,exp2) -> eval exp1 x *. eval exp2 x
| Node (Quotient,exp1,exp2) -> eval exp1 x /. eval exp2 x
| Fonction (f,exp1) -> eval_f f exp1 x
| Leaf p-> eval_P p x
and eval_f f expr x= match f with
| "sin" -> sin (eval expr x)
| "cos" -> cos (eval expr x)
| "tan" -> tan (eval expr x)
| "exp" -> exp (eval expr x)
| _ -> failwith "This function is not generic.";;

let generate a b dx=
    if a>b then failwith "Wrong order" else
    let rec generate_aux acc=function
    | x when x-.dx<=a-> a::acc
    | x -> generate_aux ((x-.dx)::acc) (x-.dx)
    in generate_aux [b] b;; 

let rec integr_up expr a b dx=
    if a>b then -.(integr_up expr b a dx) else
    let x_i=generate a b dx in
    let rec iter acc=function
    | [] | [_] -> acc
    | x_1::x_2::t -> iter ((eval expr x_2)*.(x_2-.x_1)+.acc) (x_2::t)
    in iter 0. x_i;;

let integr_dn expr a b dx=
if a>b then -.(integr_up expr b a dx) else
  let x_i=generate a b dx in
  let rec iter acc=function
  | [] | [_] -> acc
  | x_1::x_2::t -> iter ((eval expr x_1)*.(x_2-.x_1)+.acc) (x_2::t)
  in iter 0. x_i;;

let integr_rect expr a b dx=let u=integr_up expr a b dx and d=integr_dn expr a b dx in ((d+.u)/.2.,(u-.d)/.2.);;

let integr_simpson expr a b dx=
if a>b then -.(integr_up expr b a dx) else
  let x_i=generate a b dx in
  let rec iter acc=function
  | [] | [_] -> acc
  | x_1::x_2::t -> iter (((x_2-.x_1)/.6.)*.((eval expr x_1)+.4.*.(eval expr ((x_1+.x_2)/.2.))+.(eval expr x_2))+.acc) (x_2::t)
  in iter 0. x_i;;

let println_expression expr=expr |> print_expression; Printf.printf "\n";;
