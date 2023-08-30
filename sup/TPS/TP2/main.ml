type 'a suite = {u0 : 'a; f: 'a->'a};;

let f_syracuse = function
| n when n mod 2 = 0 -> n/2
| n -> (3*n)+1;;

let syracuse u0= {u0=u0;f=f_syracuse};;

let rec nth {u0=u0;f=f} = function
| n when n<0 -> failwith "Invalid argument"
| 0 -> u0
| n -> nth {u0=(f u0); f=f} (n-1);;

let ()=print_int (nth {u0=1023;f=f_syracuse} 41);;

let premiers {u0=u0;f=f} n=
    let rec iteration uk = function
    | n when n<0 -> failwith "Invalid argument"
    | 0 -> []
    | k -> uk::iteration (f uk) (k-1)
    in iteration u0 n;;

let ()=List.iter (fun h->print_string "\n"; print_int h) (premiers {u0=5;f=f_syracuse} 70);print_string "\n";;

let rec index el = function
| [] -> -1
| h::t when h=el -> 0
| h::t -> let check v = if v= (-1) then -1 else 1 + v in check (index el t)
;;

let periode {u0=u0;f=f} =
    let rec iteration k = function
    | uk::t -> let check v = 
        if v>(-1) 
        then (v+1,k-v-1) 
        else iteration (k+1) ((f uk)::uk::t) in check (index uk t)
    | _ -> failwith "Oooops"
    in (iteration 1 [(f u0);u0]);;

let print_couple (a,b) = print_string "("; print_int a; print_string "," ; print_int b ; print_string ")\n";;

let () = print_couple (periode {u0=5;f=f_syracuse});print_string "\n";;

let split2 lst k=
    let rec auxi out lst k = match lst, k with
    | (_,k) when  k<0 -> failwith "Oops 1"
    | ([],k) when k>=0 -> failwith "Oops 2"
    | (h::t,0) -> ((fst out),lst) 
    | (h::t,k) -> auxi ((fst out)@[h],[]) t (k-1)
    | _ -> failwith "error"
    in auxi ([],[]) lst k;;

let rec split lst k = match lst, k with
| (lst,0) -> ([],lst)
| ([],k) -> ([],[]) 
| (h::t,k) -> let (l1,l2) = split t (k-1) in (h::l1,l2);;

let decompose suite = 
    let (k,p)=periode suite
    in split (premiers (suite) (k+p)) p;;

let () = let (a,b)= decompose (syracuse 42) in List.iter (fun h->print_string "\n"; print_int h) a; print_string "\nPart 2\n";List.iter (fun h->print_string "\n"; print_int h) b;;

let floyd {u0=u0;f=f} =
    let rec auxiliaire k uk up= match f uk,f (f up) with (*p=2k*)
    | (a,b) when a=b -> k
    | (a,b) -> auxiliaire (k+1) a b
    in auxiliaire 1 u0 (u0);;


(* Fonction imparfaite (retourne k*p, k étant un entier supérieur ou égal à 1 et p la période.)*)
let periode_m u = match floyd u with
    | k -> let rec find_p p = function
           | ua,ub when ua=ub -> (k,p)
           | ua,ub -> find_p (p+1) (u.f ua,u.f ub)
           and uk=(nth u k) in find_p 0 (uk,u.u0);;

let preperiode u p =
    let rec find_k k = function
    | (a,b) when a=b -> k
    | (a,b) -> find_k (k+1) (u.f a,u.f b)
in find_k 0 (u.u0,nth u p);;

let rec f_prabekhar = function
| 0 -> 0
| a -> let digit=a mod 10 in (((digit |> float_of_int)**2.) |> int_of_float) + f_prabekhar (a/10);;

let print_u u n = for i=0 to n do print_int (nth u i); print_string "\n" ; done;;

let s_prabekhar u0 = {u0=u0;f=f_prabekhar};;


(* L'idée est de dire que pour tout n>=999, f_prabekhar n<n. Donc il suffit de montrer que toutes les suites de la forme prabekhar et de u0 dans [0;999] sont 8-périodiques. Ce test est fait à l'aide de la fonction ci-dessous : *)

let demo_prabekhar () =
    let rec test = function
    | 0 when fst (periode (s_prabekhar 0))<=8 -> print_string "La période d'une suite avec une telle relation de récurrence sera toujours inférieure ou égale à 8 !\n"
    | a when fst (periode (s_prabekhar a))<=8 -> test (a-1)
    | a -> print_string "Cette propriété est fausse : marche pas pour u0=";print_int a;print_string ".\n";
in test 999;;

let repr a b =
    let chiffres={u0=(0,a);f=fun (_,a) -> (10*a/b,(10*a) mod b)} in 
    let k,p= (periode chiffres) in
    let rec list_generators (pre,per) = function
    | (0,0) -> (pre,per)
    | (a,0) -> list_generators ((fst (nth chiffres a))::pre,per) (a-1,0)
    | (0,b) -> list_generators (pre,(fst (nth chiffres (p+b)))::per) (0,b-1)
    | (a,b) -> list_generators ((fst (nth chiffres a))::pre,(fst (nth chiffres (p+b)))::per) (a-1,b-1)
in list_generators ([],[]) (p,k);;

let longest k =
    let rec test (max,k_max) = function
    | k' when k'<0 -> failwith "Invalid k value (k<0)"
    | 0 -> (k_max,repr 1 k_max)
    | k' -> let repr_k=repr 1 k' in if (List.length (snd repr_k))>max then test (List.length (snd repr_k),k') (k'-1)
                                                        else test (max,k_max) (k'-1)
    in test (List.length (snd (repr 1 k)),k) k;;  

let gen_f a b (p,q) = ((p*q+1) mod a,(p+q) mod b);;

let cartesian a b = 
    let rec aux acc=function (* Cartesian product for partitions of type [0;a]*[0;b] *)
    | (a,b) when a<0 || b<0 -> failwith "Invalid partitions"
    | (0,0) -> (0,0)::acc
    | (a,0) -> aux ((a,0)::acc) (a-1,b)
    | (a,b) -> aux ((a,b)::acc) (a,b-1)
in aux [] (a,b);;  

let cycles a b =
    let f=gen_f a b in
    let rec aux iter = match iter with
    | [] -> []
    | (p,q)::t -> (periode {u0=(p,q);f=f})::(aux t)
in aux (cartesian a b);;