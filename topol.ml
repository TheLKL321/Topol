(*  Autor: Łukasz Zarębski
    Code Review:  *)

(*open PMap;; TODO*)

(*  Zwracany w momencie gdy graf okaże się cykliczny *)
exception Cykliczne;;

(*  Typ grafu, lista sąsiedztwa
    (node, lista połączeń) *)
type 'a graf = ('a * 'a list) list

(*  Wrzuca dane grafu na mapę *)
let kartograf g =
  let map = empty
  in
  let rec loop mapa = function
    | [] -> mapa
    | (h1, h2) :: t -> 
      loop (add h1 h2 mapa) t
  in loop map g

(*  Dla danego grafu[(a_1,[a_11;...;a_1n]); ...; (a_m,[a_m1;...;a_mk])] zwraca 
    graf w ktorym kazdy z elementow a_i oraz a_ij wystepuje dokladnie raz i 
    ktory jest uporzadkowany w taki sposob, ze kazdy element a_i jest przed
    kazdym z elementow a_i1 ... a_il *)
let topol g = 42
;;