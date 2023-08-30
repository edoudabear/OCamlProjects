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

let gr3=Graph.create 10;;

let ()=Graph.add_n_or_edge gr3 1 2 0;
       Graph.add_n_or_edge gr3 4 5 0;
       Graph.add_n_or_edge gr3 2 7 0;
       Graph.add_n_or_edge gr3 8 9 0;
       Graph.add_n_or_edge gr3 7 1 0;;

let dfs gr x_0 f=
    let d=Hashtbl.create 64 in
    let rec explore v=
        Hashtbl.add d v true;
        f v
        List.iter (fun x->if Hashtbl.mem d x |> not then explore x) (Graph.neighbors gr v)
    in explore x_0;;

let bfs gr x_0 f=
    let d=Hashtbl.create 42 and q=Queue.create () in
    let explore v=
        f v;
        List.iter (fun x->if Hashtbl.mem d x |> not then Hashtbl.add d x true; Queue.push x q) (Graph.neighbors gr v)
    in Hashtbl.add d x_0 true;explore x_0; while Queue.is_empty q |> not do
        explore (Queue.pop q)
    done;;

let sbs gr x_0 f=
    let d=Hashtbl.create 42 and q=Stack.create () in
    let explore v=
        f v;
        List.iter (fun x->if Hashtbl.mem d x |> not then Hashtbl.add d x true; Stack.push x q) (List.rev (Graph.neighbors gr v))
    in Hashtbl.add d x_0 true;explore x_0; while Stack.is_empty q |> not do
        explore (Stack.pop q)
    done;;

let find_cycle gr v_0 =
    let d=Hashtbl.create 42 in
    let rec explore v=
        let rec iter =function
        | [] -> None
        | h::t when Hashtbl.mem d h -> if Hashtbl.find d h <> v then begin Hashtbl.add d v h;Some h end else iter t
        | h::t -> Hashtbl.add d v h; match explore h with | Some v->Some v | None ->Hashtbl.remove d v;iter t
        in iter (Graph.neighbors gr v)
    in match explore v_0 with
    | None -> None
    | Some v -> let rec rebuild z=match Hashtbl.find d z with
                                  | r when r=v -> [z;v]
                                  | r -> z::rebuild r
                                  in Some (rebuild v);;

let topological_sort gr v_0=
    let d=Hashtbl.create 42 and res=ref [] in
    let rec explore v=
        Hashtbl.add d v Grey;
        List.iter (fun x->if Hashtbl.mem d x |> not then begin explore x end else if Hashtbl.find d x=Grey then failwith "Error : Cycle found") (Graph.neighbors gr v);
        Hashtbl.add d v Black;
        res:=v::!res
    in explore v_0;!res;;

let partition gr vertices=
    let d=Hashtbl.create 42 in
    let rec explore v=
        Hashtbl.add d v true;
        v::(List.fold_left (fun hr v->if Hashtbl.mem d v |> not then (explore v)@hr else hr) [] (Graph.neighbors gr v))
    in List.fold_left (fun hr v-> if Hashtbl.mem d v |> not then (explore v)::hr else hr ) [] vertices;;
    
let topologic_sort gr vertices=
    let d=Hashtbl.create 16 and res=ref [] in
    let rec explore a=
        List.iter (fun v->if Hashtbl.mem d v |> not then begin Hashtbl.add d v true;explore v; end) (Graph.neighbors gr a);
        res:=a::(!res)
    in List.iter (fun v->if Hashtbl.mem d v |> not then begin Hashtbl.add d v true;explore v;end) vertices;!res;;

let partition2 gr vertices=
    let d=Hashtbl.create 16 in
    let rec explore v=
        Hashtbl.add d v true;
        v::List.fold_left (fun hr e->if Hashtbl.mem d e |> not then (explore e)@hr else hr) [] (Graph.neighbors gr v)
in List.fold_left (fun hr v-> if Hashtbl.mem d v |> not then explore v::hr else hr) [] vertices;;

let find_cycle2 gr a_0=
    let d=Hashtbl.create 16 in
    let rec explore v=
        let rec deal_with_neighbors=function
        | [] -> None
        | h::t when Hashtbl.mem d h -> if Hashtbl.find d h=v then deal_with_neighbors t else begin Hashtbl.add d v h;Some h end
        | h::t -> match Hashtbl.add d v h ;explore h with | None -> Hashtbl.remove d v; deal_with_neighbors t | Some v -> Some v
        in deal_with_neighbors (Graph.neighbors gr v)
    in match explore a_0 with
    | None -> None
    | Some v -> let rec rebuild =function
                | r when Hashtbl.find d r=v -> [r;v]
                | r -> r::rebuild (Hashtbl.find d r)
in Some (rebuild v);;

explore f typ index=
    let d=Hashtbl.create 16 in
    let rec visit x=
        Hashtbl.add d x;