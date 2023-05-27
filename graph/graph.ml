type 'a graph=('a,'a list) Hashtbl.t;;


let graph=Hashtbl.create 42;;

Hashtbl.add graph 0 [1;3;4];;
Hashtbl.add graph 1 [2;4;5;6];;
Hashtbl.add graph 2 [6];;
Hashtbl.add graph 3 [4;7];;
Hashtbl.add graph 4 [8];;
Hashtbl.add graph 5 [4;6;8;9];;
Hashtbl.add graph 6 [];;
Hashtbl.add graph 7 [4;8];;
Hashtbl.add graph 8 [9];;
Hashtbl.add graph 9 [6];;

let neighbors v gr=Hashtbl.find gr v;;

let exists_edge_between v w gr=List.mem w (Hashtbl.find gr v);;

let dfs f gr a=
  let dict=Hashtbl.create 42 in
  let rec lookup v=
    Hashtbl.add dict v true;
    f v;
    List.iter (fun h->if Hashtbl.mem dict h then () else lookup h) (neighbors v gr)
in Hashtbl.add dict a true;lookup a;;


let bfs f gr a=
  let dict=Hashtbl.create 42 and q=Queue.create () in
  Queue.push a q; 
  Hashtbl.add dict a true; (* Attention, cette ligne avait été oubliée pendant l'entraînement ! *)
  let rec lookup v=
    f v;
    List.iter (fun h->if not (Hashtbl.mem dict h) then (Hashtbl.add dict h true; Queue.push h q)) (neighbors v gr)
  in while not (Queue.is_empty q) do
    lookup (Queue.pop q)
  done;;

  let sbs f gr a=
  let dict=Hashtbl.create 42 and q=Stack.create () in
  Stack.push a q; 
  Hashtbl.add dict a true; (* Attention, cette ligne avait été oubliée pendant l'entraînement ! *)
  let rec lookup v=
    f v;
    List.iter (fun h->if not (Hashtbl.mem dict h) then (Hashtbl.add dict h true; Stack.push h q)) (List.rev (neighbors v gr))
  in while not (Stack.is_empty q) do
    lookup (Stack.pop q)
  done;;

let topological_sort gr vertices=
  let visited=Hashtbl.create 42 and res=ref [] in
  let rec explorer v=
    Hashtbl.add visited v true;
    List.iter
      (fun w->if not (Hashtbl.mem visited w) then explorer w) (neighbors v gr);
    res:=v::(!res)
  in
  List.iter (fun v->if Hashtbl.mem visited v then () else explorer v) vertices;
  !res;;