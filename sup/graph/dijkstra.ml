(** Commande pour exécuter : ocamlfind ocamlc -package core,yojson -linkpkg dijkstra.ml
   *)
(** Structure retenue :
 {
       // "recordid": "95a47dfc8c841cd18f6930b78c24bcb2ddb81ca6",
        "fields": {
            "indice_lig": "D",
            "geo_point_2d": [
                48.68101661540746,
                2.533130656571037
            ],
            "mode": "RER",
            "exploitant": "SNCF",
            "nom_gares": "Boussy-Saint-Antoine",
            "id_gares": 105,
        }
    }
  }  
*)

type station= {
  id : int;
  name : string;
  coord : float*float;
  mode : string;
  exploitant : string;
  indice_ligne : string;
  terminus : bool;
}
type graph=(int*float*station) list array;;
open Yojson;;

module Graph : sig
  val create : int->graph;;
  val exists : graph->int->int->bool;;
  val neighbors : graph->int->int list;;
  val neighbors_full : graph->station->(int*float*station) list;;
  val add_edge : graph->station->station->float->unit;;
  val add_n_or_edge : graph->station->station->float->unit;;
  val delete_edge : graph->station->station->unit;;
  val delete_n_or_edge : graph->station->station->unit;;
end=
struct
let create n = Array.make n ([]);;
let neighbors gr a=List.map (fun (v,_,_)->v) gr.(a);;
let exists gr a b= List.mem b (neighbors gr a);;
let neighbors_full gr a=gr.(a.id);;
let add_edge gr a b w =if List.mem b.id (neighbors gr a.id) then () else gr.(a.id)<-(b.id,w,b)::gr.(a.id);;
let add_n_or_edge gr a b w=add_edge gr a b w;add_edge gr b a w;;
let rec remove b=function
| [] -> []
| (h,_,_)::t when b=h -> t
| h::t -> h::remove b t;;
let delete_edge gr a b=gr.(a.id)<-remove b.id gr.(a.id);;
let delete_n_or_edge gr a b=delete_edge gr a b;delete_edge gr b a;;
end;;

let get_bool=function
| "0"->false
|  _ -> true;;

let form el=
let open Yojson.Basic.Util in let fields=el |> member "fields" in {
  id = fields |> member "id_gares" |> to_int;
  name = fields |> member "nom_gares" |> to_string;
  coord = begin match fields |> member "geo_point_2d" |> to_list with | [lo;la] -> (lo |> to_float,la |> to_float) | _ -> failwith "Invalid coordinates" end;
  mode = fields |> member "mode" |> to_string;
  exploitant = fields |> member "exploitant" |> to_string;
  indice_ligne = fields |> member "indice_lig" |> to_string;
  terminus = fields |> member "termetro" |> to_string |> get_bool || fields |> member "terrer" |> to_string |> get_bool || fields |> member "tertram" |> to_string |> get_bool || fields |> member "tertrain" |> to_string |> get_bool
};;

let stations=ref [];;
let max_id=ref (-1);;

let generate_stations=
  let json=Yojson.Basic.from_file "data.json" in
  let open Yojson.Basic.Util in
  List.iter (fun v->stations:=(form v)::(!stations);max_id:=max !max_id (List.hd !stations).id) (json |> to_list)
;;

let print_station el=
Printf.printf "{\n\tid : %d;\n\tnom : %s;\n\tcoord : %f*%f;\n\tmode : %s;\n\texploitant : %s;\n\tindice_ligne : %s;\n\test_terminus : %b;\n}\n" el.id el.name (fst el.coord) (snd el.coord) el.mode el.exploitant el.indice_ligne el.terminus;();;

let ()=List.iter (print_station) !stations;Printf.printf "%d\n" (List.length !stations);;

(*
  Structure du graphe : on accroche toutes les stations qui sont les moins éloignées géographiquement (en espérant que cela corresponde aux stations adjacentes)   
*)

let dist st1 st2=
  let (a,b)=st1.coord and (c,d)=st2.coord in
  sqrt ((a-.c)**2. +. (b-.d)**2.);;

let poke=function
| Some v -> v
| None  -> failwith "Can't poke None";;

let find_nearest_station station=
  let rec iter acc=function
  | [] -> acc
  | st::t when st.id=station.id || st.mode<>station.mode || st.indice_ligne<>station.indice_ligne -> iter acc t
  | st::t -> if acc=None || (acc<>None && dist (poke acc) station > dist st station) then iter (Some st) t else iter acc t
in iter None (!stations);;

let find_two_nearest_stations station=
  let min_a=find_nearest_station station in
  let rec iter acc=function
  | [] -> min_a,acc
  | st::t when st.id=station.id || (min_a<>None && st.id=(poke min_a).id) || st.mode<>station.mode || st.indice_ligne<>station.indice_ligne -> iter acc t
  | st::t -> if acc=None || (acc<>None && dist (poke acc) station > dist st station) then iter (Some st) t else iter acc t
in iter None (!stations);;

let gr=Graph.create (!max_id+1);;

let weight station1 station2=if station1.mode<>station2.mode then 0. else (if station1.mode="RER" || station1.mode="TRAIN" then 1. else 3.)*.dist station1 station2;;
let count=ref 0;;

let rec find_link name = function
| [] -> []
| h::t -> if h.name=name then h::find_link name t else find_link name t;;

let rec link_stations=function
| [] -> ()
| h::t -> List.iter (fun x->Graph.add_n_or_edge gr h x 0.5) (find_link h.name t); link_stations t;;

let rec find station mode l_number=function
| [] -> failwith "not found"
| h::t when h.name=station && h.mode=mode && l_number=h.indice_ligne -> h
| h::t -> find station mode l_number t;;

let gen_graph =
  let rec build station=
  incr count;
  (*Printf.printf "Handling : %d, %dth element\n" station.id !count;*)
  if station.terminus || station.indice_ligne="FUN" then begin
    let s=begin match find_nearest_station station with | Some v -> v | None -> failwith "Corrupt data 1" end in
    Graph.add_n_or_edge gr station s (weight station s);
  end else begin
    let s1,s2=begin match find_two_nearest_stations station with | Some v,Some w->v,w 
    | Some _,None -> failwith "Corrupt data 2" 
    | None, Some _ -> failwith "Corrupt data 3"
    | None,None-> failwith "Corrupt data 4" end in
    Graph.add_n_or_edge gr station s1 (weight station s1);
    Graph.add_n_or_edge gr station s2 (weight station s2);
  end
in List.iter build (!stations);link_stations !stations;Printf.printf "Setup was successful\n";;

type 'a heap={mutable size:int;mutable n:int;mutable data:'a array};;
exception Is_empty;;

let create ()={
  size=16;
  n=0;
  data=Array.make 16 None
};;


let double h=let arr=Array.init (2*h.n) (fun k->if k<h.size then h.data.(k) else None) in
  h.data<-arr;
  h.size<-2*h.size;;

let repair_up pq i =
  let (p, v) = poke pq.data.(i) in
  let rec move_up i =
  let j = (i-1)/2 in
  if i = 0 || fst (poke pq.data.(j)) < p
  then pq.data.(i) <- Some (p, v) (* replace l'élément *)
  else (pq.data.(i) <- pq.data.(j); move_up j)
  in move_up i;;

let push p v h=
  if h.n=h.size then
    double h;
  (h.data).(h.n)<-Some (p,v);
  h.n<-h.n+1;
  
  repair_up h (h.n-1);;

let top h=match h.data.(0) with
| None -> raise Is_empty
| Some v -> v;;

let repair_down h i =
  let elem = poke h.data.(i) in
  let rec move_down i =
  if i >= h.n/2 then h.data.(i) <- Some elem (* plus de fils *)
  else let j = (if h.n = 2*i+2 || h.data.(2*i+1) <= h.data.(2*i+2)
  then 2*i+1
  else 2*i+2)
  in if poke h.data.(j) >= elem
  then h.data.(i) <- Some elem
  else (h.data.(i) <- h.data.(j); move_down j)
  in move_down i;;

let is_empty h=h.n=0;;
let pop h=
  let el=top h in
  h.data.(0) <- h.data.(h.n-1);
  if h.n-1<>0 then h.data.(h.n-1)<-None;
  h.n<-h.n-1;
  repair_down h 0; 
  el;;


let is_secure st=st.mode="METRO" && st.indice_ligne<="6";;

let dijkstra gr d1 d2 secure=
  let dist=Hashtbl.create 42 and q=create () in
  Hashtbl.add dist d1 [];push 0. d1 q;
  while is_empty q |> not && Hashtbl.mem dist d2 |> not do
    let p,s=pop q in let l=Hashtbl.find dist s in
    List.iter (fun (id,w,st)-> if Hashtbl.mem dist st |> not && (secure |> not || is_secure st) then begin Hashtbl.add dist st (st::l); push (p+.w) st q end) (Graph.neighbors_full gr s)
  done; if Hashtbl.mem dist d2 |> not then None else Some (d1::(List.rev (Hashtbl.find dist d2)));;


(*let la_defense_A=find "La Défense" "RER" "A" !stations;;
let odeon=find "Odéon" "METRO" "4" !stations;;
let () = List.iter (fun v->Printf.printf "|%s (%s%s)\n" v.name v.mode v.indice_ligne) (match (dijkstra gr la_defense_A odeon) with | Some v -> v | None -> Printf.printf "no path found\n";[]);;
let () = print_station la_defense_A; print_station odeon;;
let ()=Printf.printf "Neighbors of \"La Défense\" RER A :\n";print_station la_defense_A;List.iter (fun (_,_,st)->print_station st) (Graph.neighbors_full gr la_defense_A);;*)

let rec find_type typ index = function
| [] -> failwith "Unknown line type"
| h::t -> if h.mode=typ && h.indice_ligne=index then h else find_type typ index t;;

let explore f typ index=
    Printf.printf "%s %s:\n" typ index;
    let d=Hashtbl.create 16 in
    let rec visit x=
        f x;
        Hashtbl.add d x.id true;
        List.iter (fun (i,w,st)->if Hashtbl.mem d i |> not && st.mode=typ && st.indice_ligne=index then begin 
            visit (st)
        end) (Graph.neighbors_full gr x);
        Printf.printf "**";
    in find_type typ index !stations |> visit;Printf.printf "(%d stations)\n" (Hashtbl.length d);;

let explore_classic f station=
Printf.printf "From %s :\n" station.name;
let d=Hashtbl.create 16 in
let rec visit x=
    f x;
    Hashtbl.add d x.id true;
    List.iter (fun (i,w,st)->if Hashtbl.mem d i |> not && st.mode=station.mode && st.indice_ligne=station.indice_ligne then begin 
        visit (st)
    end) (Graph.neighbors_full gr x);
    Printf.printf "**";
in station |> visit;Printf.printf "(%d stations)\n" (Hashtbl.length d);;


(* Corrections sur les lignes*)
let ()=
    (* Ligne 1 : aucun pb *)
    (* Ligne 2 : Ternes à replacer correctement *)
    Graph.delete_n_or_edge gr (find "Courcelles" "METRO" "2" !stations) (find "Charles De Gaulle-Étoile" "METRO" "2" !stations) ;
    let s1,s2=(find "Courcelles" "METRO" "2" !stations),(find "Ternes" "METRO" "2" !stations) in
    Graph.add_n_or_edge gr s1 s2 (weight s1 s2) ;
    let s1,s2=(find "Charles De Gaulle-Étoile" "METRO" "2" !stations),(find "Ternes" "METRO" "2" !stations) in
    Graph.add_n_or_edge gr s1 s2 (weight s1 s2) ;
    (* Ligne 3 : aucun pb *)
    (* Ligne 4 : Vavin automatiquement connecté à Saint-Placide*)
    Graph.delete_n_or_edge gr (find "Vavin" "METRO" "4" !stations) (find "Saint-Placide" "METRO" "4" !stations);
    (* Ligne 5 : côté nord, 2 à 3 inversion de liens*)
    Graph.delete_n_or_edge gr (find "Bobigny-Pantin - Raymond Queneau" "METRO" "5" !stations) (find "Hoche" "METRO" "5" !stations);
    Graph.delete_n_or_edge gr (find "Jacques Bonsergent" "METRO" "5" !stations) (find "Gare du Nord" "METRO" "5" !stations);
    (* Ligne 6 : un sans faute*)
    (* Ligne 7 :*)
    
;;

(*let ()=explore_classic print_station (find "République" "METRO" "3" !stations);;
let ()=while true do
  if read_int ()=0 then
    explore (fun v->Printf.printf "| %s (%f,%f)\n" v.name (fst v.coord) (snd v.coord)) (read_line ()) (read_line ())
  else
    List.iter (fun (i,w,st)-> print_station st) (Graph.neighbors_full gr (find (read_line ()) (read_line ()) (read_line ()) !stations))
done;
;;*)

let rec choose_station name secure=function
| [] -> failwith "Station not found"
| h::t when (secure |> not || is_secure h) && h.name=name -> h
| _::t -> choose_station name secure t;;

let ()=while true do
    let secure=Printf.printf "Trajet sur graphe secure ?\n";match read_line () with | "1" -> true | _ -> false in
    let depart=Printf.printf "Entrez la gare de départ\n"; choose_station (read_line ()) secure !stations in
    let arrivee=Printf.printf "Entrez la gare de d'arrivée\n";choose_station (read_line ()) secure !stations in
    print_station depart;
    print_station arrivee;
    match dijkstra gr depart arrivee secure with
  | None -> Printf.printf "Pas de chemin trouvé\n"
  | Some v -> List.iter (fun st -> Printf.printf "|%s (%s %s)\n" st.name st.mode st.indice_ligne) v
done;;