type 'a graph=(int*'a) list array;;
type process= Black | Grey | White;;

module Graph : sig
  val create : int->'a graph;;
  val exists : 'a graph->int->int->bool;;
  val neighbors : 'a graph->int->int list;;
  val neighbors_w : 'a graph->int->(int*'a) list;;
  val add_edge : 'a graph->int->int->'a->unit;;
  val add_n_or_edge : 'a graph->int->int->'a->unit;;
end=
struct
let create n = Array.make n ([]);;
let neighbors gr a=List.map (fun (v,_)->v) gr.(a);;
let exists gr a b= List.mem b (neighbors gr a);;
let neighbors_w gr a=gr.(a);;
let add_edge gr a b w =if List.mem b (neighbors gr a) then begin Printf.printf "%d & %d :" a b; failwith "Already added" end else gr.(a)<-(b,w)::gr.(a);;
let add_n_or_edge gr a b w=add_edge gr a b w;add_edge gr b a w;;
end;;

let gr=Graph.create 42;;
let ()=Graph.add_edge gr 0 1 0;
       Graph.add_edge gr 0 3 0;
       Graph.add_edge gr 0 4 0;
       Graph.add_edge gr 1 4 0;
       Graph.add_edge gr 1 5 0;
       Graph.add_edge gr 1 6 0;
       Graph.add_edge gr 1 2 0;
       Graph.add_edge gr 2 6 0;
       Graph.add_edge gr 3 4 0;
       Graph.add_edge gr 3 7 0;
       Graph.add_edge gr 4 8 0;
       Graph.add_edge gr 5 4 0;
       Graph.add_edge gr 5 8 0;
       Graph.add_edge gr 5 9 0;
       Graph.add_edge gr 5 6 0;
       Graph.add_edge gr 7 4 0;
       Graph.add_edge gr 7 8 0;
       Graph.add_edge gr 8 9 0;
       Graph.add_edge gr 9 6 0;;

let gr2=Graph.create 10;;
let ()=
Graph.add_n_or_edge gr2 0 1 0;
Graph.add_n_or_edge gr2 0 3 0;
Graph.add_n_or_edge gr2 0 4 0;
Graph.add_n_or_edge gr2 1 5 0;
Graph.add_n_or_edge gr2 1 6 0;
Graph.add_n_or_edge gr2 1 2 0;
Graph.add_n_or_edge gr2 3 7 0;
Graph.add_n_or_edge gr2 3 8 0;
Graph.add_n_or_edge gr2 5 8 0;
Graph.add_n_or_edge gr2 8 9 0;;

let dfs gr a f=
  let d=Hashtbl.create 42 in
  let rec visit x=
    f x;
    List.iter (fun v-> if (Hashtbl.mem d v |> not) then begin Hashtbl.add d v true; visit v end) (Graph.neighbors gr x)
  in Hashtbl.add d a true;visit a;;

let sort gr=Array.map (List.sort compare) gr

(* ATTENTION :  d -> dernier argument (Hashtbl) | q -> premier argument (Queue)*)
let bfs gr a f=
  let d=Hashtbl.create 42 and q=Queue.create () in
  let rec visit x=
    f x;
    List.iter (fun v->if Hashtbl.mem d v |> not then begin Hashtbl.add d v true;Queue.push v q end) (Graph.neighbors gr x)
  in Hashtbl.add d a true; Queue.push a q;
  while Queue.is_empty q |> not do
    visit (Queue.pop q)
  done;
;;


(* WARNING : List.rev used !*)
let sbs gr a f=
  let d=Hashtbl.create 42 and s=Stack.create () in
  let rec visit x=
    f x;
    List.iter (fun v->if Hashtbl.mem d v |> not then begin Hashtbl.add d v true;Stack.push v s end) (List.rev (Graph.neighbors gr x))
  in Hashtbl.add d a true; Stack.push a s;
  while Stack.is_empty s |> not do
    visit (Stack.pop s)
  done;
;;

let find_cycle gr s=
  let d=Hashtbl.create 50 in
  let rec explore x=
    let rec visit_neighbors=function
    | [] -> None
    | y::t when Hashtbl.mem d y-> if Hashtbl.find d y<>x then begin Hashtbl.add d x y; Some y end else visit_neighbors t
    | y::t -> match Hashtbl.add d x y;explore y with | Some y -> Some y | None -> Hashtbl.remove d x; visit_neighbors t
    in visit_neighbors (Graph.neighbors gr x)
  in Hashtbl.add d s s;
  match explore s with
  | None -> None
  | Some v->let rec rebuild z= match Hashtbl.find d z with
                              | r when r=v -> [z;v]
                              | r -> z::(rebuild r)
   in Some (rebuild v);;

(* ATTENTION *)
let topological_sort gr s_0=
  let d=Hashtbl.create 50 and res=ref [] in
  let rec find_next s= begin
    List.iter (fun v->if Hashtbl.mem d v |> not then begin Hashtbl.add d v Grey; find_next v end else if Hashtbl.find d v=Grey then failwith "Cycle found") (Graph.neighbors gr s);
    res:=s::!res;Hashtbl.replace d s Black end
  in Hashtbl.add d s_0 Grey;find_next s_0;!res;;