(* 
Edouard Aubert 2022 (https://e.diskloud.fr)
*)

(* Application directe de la formule utilisée dans la méthode de Simpson. La liste de floats correspond aux sigma_k, supposément triés 
   dans l'ordre croissant. f est une application prenant un float et retournant un float, application 'continue' sur l'intervalle décrit par les sigma_k  *)
let integrale_simpson (f : float -> float ) (sigma_k : float list) =
  let rec sum_iterator (acc : float) = function
  | [sk0] -> print_string "WARNING : votre liste de sigma_k doit au moins contenir deux valeurs pour être non-nulle.\n"; 0.
  | [sk;sk'] -> acc +. ((sk'-.sk)/.6.)*.(f sk +. 4.*.(f ((sk'+.sk)/.2.)) +. f sk')
  | sk::sk'::t -> sum_iterator (acc +. ((sk'-.sk)/.6.)*.(f sk +. 4.*.(f ((sk'+.sk)/.2.)) +. f sk')) (sk'::t)
  | [] -> failwith "ERROR : votre liste sigma_k est vide.. il faut au moins donner des bornes !"
in sum_iterator 0. sigma_k;;

(* Cette fonction permet de générer la liste des sigma_k pour un intervalle [a;b] choisi par l'utilisateur 
   et avec p(sigma)<=delta *)
let genere_pas_intervalle (a : float) (b : float) delta = match a with
| a when a=b -> [b]
| a -> let rec generateur_auxiliaire acc = function
       | x when a<b && x<=a -> a::acc
       | x when a<b -> generateur_auxiliaire (x::acc) (x-.delta)
       | x when a>b && x>=a -> a::acc
       | x when a>b -> generateur_auxiliaire (x::acc) (x+.delta)
       | _ -> failwith "Cas dégénéré (pour éviter que le compilateur n'affiche une alerte"
in generateur_auxiliaire [] (b);;


(* Voici une démonstration de l'appel l'intégrale de x^3dx de aux bornes -1 et -4 et où p(sigma)=0.001 *)
genere_pas_intervalle (-.1.) (-.4.) 0.001 |> integrale_simpson (fun x-> x**3.);; (* Retourne une valeur proche de 63.75 *)

