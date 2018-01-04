(*  Autor: Łukasz Zarębski
    Code Review:  *)

(*open PMap;; TODO*)

(*  Zwracany w momencie gdy graf okaże się cykliczny *)
exception Cykliczne;;

(*  Typ grafu, lista sąsiedztwa
    (node n, lista node'ów do których node n ma skierowaną krawędź) *)
type 'a graf = ('a * 'a list) list

(*  Zwraca mapę list node'ów do których dany node ma skierowaną krawędź, 
    kluczowaną node'ami. W każdej takiej liście nie występują duplikaty *)
let kartograf g =
  let mapa = ref empty
  in let f (node, lis) = 
    let lista = 
      if mem node !mapa then 
        lis @ (find node !mapa)
      else lis
    in mapa := add node lista !mapa;
  in 
    List.iter f g;
    let uniq l =
      let tempMapa = ref empty 
      in let rec loop = function
          | [] -> ()
          | h :: t -> 
            tempMapa := add h h !tempMapa;
            loop t
        in loop l;
        fold (fun k a -> k :: a) !tempMapa []
    in map uniq !mapa


(*  Dla danego grafu[(a_1,[a_11;...;a_1n]); ...; (a_m,[a_m1;...;a_mk])] zwraca 
    graf w ktorym kazdy z elementow a_i oraz a_ij wystepuje dokladnie raz i 
    ktory jest uporzadkowany w taki sposob, ze kazdy element a_i jest przed
    kazdym z elementow a_i1 ... a_il *)
let topol g = 42
;;