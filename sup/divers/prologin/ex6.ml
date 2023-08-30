let refroidissement n m k a b tuyaux =
  let tuyauxTuples=List.map (fun h -> match h with | [a;b;c] ->(a,b,c) | _-> failwith "invalid output") tuyaux
  and tuyaux_par_dep= Array.init (n) (fun h -> Array.of_list (List.map (fun i-> match i with | [a;_;_] -> if a=h-1 then true else false | _ -> false ) tuyaux))
  and reachable=ref false and
  paths=[ref ([a],0)] in
  (*for i=0 to n-1 do*)
    
tuyaux_par_dep;;

let () =
  let n = read_int () in
  let m = read_int () in
  let k = read_int () in
  let a = read_int () in
  let b = read_int () in
  let tuyaux = List.init m (fun _ -> read_line () |> fun x -> if x = "" then [] else String.split_on_char ' ' x |> List.rev_map int_of_string |> List.rev) in
  refroidissement n m k a b tuyaux