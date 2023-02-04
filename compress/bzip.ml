let read filename = 
  (* On ouvre le fichier en mode binaire *)
  let ic = open_in_bin filename in
  (* On d�termine sa longueur *)
  let length = in_channel_length ic in
  (* On lit une cha�ne de la longueur correspondante *)
  really_input_string ic length;;

let to_array s =
  Array.init (String.length s) (fun i -> int_of_char s.[i]);;

let to_string t =
  String.init (Array.length t) (fun i -> char_of_int t.(i));;

let frequences tab = let n=Array.length tab and res=Array.make 256 0 in
  for i=0 to n-1 do
    res.(tab.(i))<-res.(tab.(i))+1
  done; res;;

let moins_frequent t=let freqs=frequences t in
  let min= ref freqs.(0) and im= ref 0 in (* On détermine la fréquence minimale et son indice associé*)
  for i=1 to 255 do
    if freqs.(i)<(!min) then begin min:=freqs.(i);im:=i end
  done; !im;;

let comptes_successifs t k=let count=ref 1 and charac=t.(k) and n=Array.length t in
  while k+(!count)<n && charac=t.(k+(!count)) do
    count:=!count+1
  done; !count;;

let taille_resultat t=
  let k=moins_frequent t and taille=ref 1 and n=Array.length t and i=ref 0 in
  while (!i)<n do
    if t.(!i)=k then begin taille:=(!taille)+2;i:=!i+1; end
    else match comptes_successifs t (!i) with
    | comptes when comptes<=3 -> taille:=(!taille)+comptes;i:=!i+comptes
    | comptes when comptes<=255 -> taille:=(!taille)+3; i:=!i+comptes
    | comptes -> taille:=(!taille)+3*(comptes/255) + (min (comptes mod 255) 3); i:=!i+comptes
  done; !taille;;

let ratioRLE filename = let stream=read filename in (String.length stream, stream |> to_array |>  taille_resultat);;


let encode_RLE stream =
  let t=stream in let k=moins_frequent t in let res=Array.make (taille_resultat t) (k) and n=Array.length t and i=ref 0 and p=ref 1 in
  while (!i)<n do
    if t.(!i)=k then begin res.(!p)<-k;res.(!p+1)<-0;i:=!i+1; p:=!p+2 end
    else match comptes_successifs t (!i) with
    | comptes when comptes<=3 -> res.(!p)<-t.(!i);p:=!p+1;i:=!i+1;
    | comptes when comptes<=255 -> res.(!p)<-k;res.(!p+1)<-comptes; res.(!p+2)<-t.(!i); i:=!i+comptes;p:=!p+3;
    | comptes -> res.(!p)<-k;res.(!p+1)<-255; res.(!p+2)<-t.(!i); i:=!i+255;p:=!p+3;
  done; res;; 

let taille_decomp stream=let k=stream.(0) and n=Array.length stream and i=ref 1 and count=ref 0 in
while (!i)<n do
  match stream.(!i) with
  | v when v=k -> begin match stream.(!i+1) with
                  | v2 when v2=0 -> count:=!count+1;i:=!i+2;
                  | v2 -> count:=!count+v2;i:=!i+3;
                end
  | v -> count:=!count+1; i:=!i+1;
done;
!count;;



let decode_RLE stream= let k=stream.(0) and n=Array.length stream and res=Array.make (taille_decomp stream) (0) and i=ref 1 and p=ref 0 in
while (!i)<n do
  match stream.(!i) with
  | v when v=k -> begin match stream.(!i+1) with
                  | v2 when v2=0 -> res.(!p)<-k; p:=!p+1;i:=!i+2;
                  | v2 -> let v3=stream.(!i+2) in for j=(!p) to !p+v2-1 do res.(!p)<-v3;p:=!p+1 done;i:=!i+3;
                end
  | v -> res.(!p)<-v; p:=!p+1;i:=!i+1;
done; res;;

let compare_rotations arr a b =
  let count=ref 0 and res=ref 0 and n=Array.length arr in
  while (!count)<n && (!res)=0 do
    if arr.((a+ (!count)) mod (n))>arr.((b+(!count)) mod (n)) then res:=1 else
    if arr.((a+ (!count)) mod (n))<arr.((b+(!count)) mod (n)) then res:=-1
    else incr count;
  done; !res;;

let indexOf arr a=
  let n=Array.length arr and res=ref (-1) and i=ref 0 in
  while (!i)< n && (!res)=(-1) do
    if arr.(!i)=a then res:=!i else incr i
  done; !res;;

let tri_rotations t = (* definir compare_rotations avant *)
  let n = Array.length t in
  let ordre = Array.make n 0 in
  for i = 1 to n-1 do ordre.(i) <- i done;
  Array.sort (compare_rotations t) ordre;
  ordre;;

let encode_BW arr =
  let rots= tri_rotations arr and n= Array.length arr in let res=Array.make n 0 and i=ref 0 in
  while (!i)<n do 
    res.(!i)<-arr.((n-1+rots.(!i)) mod n ); incr i;
  done; (res,indexOf rots 1) ;; (* Pour la clé, on prend la case de rots contenant l'indice n°1 -> signifie que le premier caractère est a la fin de cette permutation *)

let bzip arr= let data,code=encode_BW arr in
  (encode_RLE data,code);;

let nthOcc k v arr=let n = Array.length arr and count=ref k and i=ref 0 and resindex=ref 0 in
while (!i)<n && (!count)>=0 do
  if arr.(!i)=v then begin count:=!count-1; resindex:=!i end else ();
  incr i;
done; if (!count)>=0 then -1 else (!resindex);;

let print_gs () = print_string "\027[42m  \027[0m";;

let trouver_indices arr = let sorted= Array.copy arr and lastAddedSquare=ref 0 and freqs=Array.make 256 0 and n=Array.length arr and i=ref 0 in let res=Array.make n 0 in
Array.sort (fun a b->if a<b then -1 else if a=b then 0 else 1) sorted;
while (!i)<n do
  if ((!i)-(!lastAddedSquare))>n/10 then begin lastAddedSquare:=(!i); print_gs () end;
  res.(!i)<-nthOcc (freqs.(sorted.(!i))) (sorted.(!i)) arr;
  freqs.(sorted.(!i))<-freqs.(sorted.(!i))+1;
incr i;
done; print_endline ""; res ;;

let decodage_BW (arr,key)=
  let indexes=trouver_indices arr and lastAddedSquare=ref 0 and counter=ref 1 and n=Array.length arr in let res=Array.make n (arr.(key)) and pointer=ref key in
  while (!counter)<n do
    pointer:=indexes.(!pointer);
    res.(!counter)<-arr.(!pointer);
    if ((!counter)-(!lastAddedSquare))>n/10 then begin lastAddedSquare:=(!counter); print_gs () end;
    incr counter;
  done;
  print_endline "";
res;;

let bunzip (arr,key)= decodage_BW (arr |> decode_RLE,key);;

let ratioBW filename = let stream=read filename in (String.length stream, stream |> to_array |>  bzip |> fst |> Array.length);;

let data=to_array (read "adn.dat") in
  data= decode_RLE (encode_RLE data);;

let convertFile str=read str |> to_array;;


(* Permet de tester que la conversion et déconversion donnent bien le fichier initial *)
let control str = let file=convertFile str in match file=(file |> bzip |> bunzip) with
| true -> "true !"
| false -> "false";;


(*Test de bzip pour le fichier ocaml.ppm (mis en commentaire car coûteux)
   
let ()= print_string "Running test..\n"; print_string (control "ocaml.ppm");; -> retourne vrai *)

let rec write_data oc data key=
  let oc = oc |> open_out in
  if key>=0 then Printf.fprintf oc "%d\x2A" (key);
  let n=Array.length data and i=ref 0 in
    while ((!i)<n) do begin
      Printf.fprintf oc "%c" (data.(!i));
      incr i;
    end done;
    close_out oc;
;;

let string_of_char c = String.make 1 c;;

let rec read_zip_file ic=
  let ic=ic |> open_in_bin in let i=ref 0 and key= ref "" and n=in_channel_length ic and finished=ref false in
  while (!i)<n && not (!finished)  do 
    try begin
      let bit = input_char ic in
      (*Printf.printf "%c\n" bit;*)
      if bit='\x2A' then begin 
        finished:=true;
      end else begin key :=(!key)^(string_of_char bit) end;
      incr i;
    end with e -> begin
      (* some unexpected exception occurs *)
      Printf.printf "Corrupt file. Check your input.\n";
      close_in_noerr ic;
      (* emergency closing *)
      raise e end
    done;
    (really_input_string ic (n-(!i)),int_of_string (!key));;
  ;;

let generateZip filenameInput filenameOutput=
  let data,key=filenameInput |> read |> to_array |> bzip in
  (* Array.iter (print_int) data; *)
  write_data (filenameOutput) (data |> Array.map  (fun h-> char_of_int h)) key;;

let generateUnzip filenameInput filenameOutput =
  let data,key=read_zip_file filenameInput in
  write_data filenameOutput ((bunzip ((data |> to_array),key)) |> Array.map  (fun h-> char_of_int h)) (-1);;

let print_version () = print_endline "Diskloud ZIP version 0.0.1 alpha (Jan 2023)\n© Edouard Aubert, Diskloud Technologies";;

let print_commands commands = print_endline "Diskloud zip 2023"; List.iter (fun (c,doc) -> Printf.printf "%s -> %s\n" c doc) commands;;

let commands = 
[
("version","Displays information on the programm and its version");
("zip","zip files : takes inputFileName as first argument and outputFileName as 2nd argument");
("compress","zip files : takes inputFileName as first argument and outputFileName as 2nd argument");
("unzip","unzip files : takes inputFileName as first argument and outputFileName as 2nd argument");
("decompress","unzip files : takes inputFileName as first argument and outputFileName as 2nd argument")
]

let () = try match Array.length Sys.argv,Sys.argv.(1) with
  | _,"version" -> print_version ()
  | 4,"zip" | 4,"compress" -> generateZip Sys.argv.(2) Sys.argv.(3)
  | 3,"zip" | 3,"compress" -> generateZip Sys.argv.(2) Sys.argv.(2)
  | _,"zip" | _,"compress" -> print_endline "The command only expects two arguments (inputFilename and outputFilename)"
  | 4,"unzip" | 4,"decompress" -> generateUnzip Sys.argv.(2) Sys.argv.(3)
  | 3,"unzip" | 3,"decompress" -> generateUnzip Sys.argv.(2) Sys.argv.(2)
  | _,"unzip" | _,"decompress" -> print_endline "The command only expects two arguments (inputFilename and outputFilename)"
  | _,_ -> print_endline "We could not recognize your command" ; print_commands commands
with 
| Invalid_argument s -> print_version ();;
