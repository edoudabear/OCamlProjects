let memo= Array.init (42+1) (fun _ -> ref (-1))

let rec fib k=
   if k<=1 then k
   else if !(memo.(k)) <> -1 then !(memo.(k))
   else begin memo.(k):= fib (k-1) + fib (k-2); !(memo.(k)) end;;

let rec fib2 n=
  if n=0 then 0 else if n=1 then 1 else
  let rec aux acc k= match k,acc with
  | k,_ when k<0 -> failwith "Invalid index"
  | k,[f1;f2] when k=n-1 -> f1+f2
  | k,[f1;f2] -> aux [f1+f2;f1] (k+1)
  | _ -> failwith "Not expected"
in aux [1;0] (1);;
