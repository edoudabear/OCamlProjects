type 'a heap_t = Nil | Node of 'a heap_t*'a*'a heap_t;;

let to_tree heap_arr=
  let n=Array.length heap_arr in
  let rec gen = function
  | k when k>=n -> Nil
  | k -> Node (gen (2*(k+1)-1),heap_arr.(k),gen (2*(k+1)))
in gen 0;;

let to_array h=if h=Nil then [||] else
  let n=
    let rec size=function
    | Nil -> 0
    | Node (a,_,b)->1+size a+size b
    in size h
  and k=match h with | Node (_,a,_)->a | _ -> failwith "not expected" in
  let res=Array.make (n) (k) in
  let rec insert u_h=function
  | i when i>=n -> ()
  | i -> begin match u_h with
        | Nil -> failwith "not expected either"
        | Node (h1,v,h2) -> res.(i)<-v;insert h1 (2*(i+1)-1);insert h2 (2*(i+1))
        end
  in insert h 0;res;;

type 'a binom_heap = Nilb |Nodeb of 'a*'a binom_heap list;;
type 'a heap_arr = { mutable n : int ; data : 'a array};;


let copy k arr= if (k=0) then [||] else
  let res=Array.make (k) (arr.(0)) in
  for i=0 to k-1 do
    res.(i)<-arr.(i)
  done;
  res;;

let to_binom h= if h.data=[||] then Nilb else
  let rec merger = function
  | n when n>=h.n -> Nilb
  | n-> (* invalide *)begin let nc1= merger (2*(n+1)-1) and nc2=merger (2*(n+1)) in
            match nc1,nc2 with
            | Nilb,Nilb -> Nodeb (h.data.(n),[])
            | Nodeb (v1,l1),Nodeb (v2,l2) -> Nodeb (max v1 v2,if v1>v2 then nc2::l1 else nc1::l2)
            | _ -> failwith "invalid"
            end
  in merger 0;;
  
let data={n=16;data=[|37;29;22;10;27;11;4;5;7;25;17;9;8;2;3;1|]};;