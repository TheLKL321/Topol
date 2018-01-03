(*  Autor: Łukasz Zarębski
    Code Review:  *)

(*open PMap;; TODO*)

(*  Zwracany w momencie gdy graf okaże się cykliczny *)
exception Cykliczne;;

(*  Typ grafu, lista sąsiedztwa
    (node, lista połączeń) *)
type 'a graf = ('a * 'a list) list

(*  Zwraca parę (mapa1, mapa2), gdzie mapa1 to mapa list sąsiadów wierzchołka 
    grafu g kluczowana wierzchołkami, a mapa2 to mapa liczby sąsiadów 
    wierzchołka kluczowana wierzchołkami *)
let kartograf g =
  let rec loop map indegree = function
    | [] -> (map, indegree)
    | (h1, h2) :: t -> 
      let rec loop2 tempIndegree = function
        | [] -> tempIndegree
        | th :: tt -> 
          loop2 (add th (find th tempIndegree + 1) tempIndegree) tt
      in loop (add h1 h2 map) (loop2 indegree h2) t
  in loop (empty) (empty) g

(*  Dla danego grafu[(a_1,[a_11;...;a_1n]); ...; (a_m,[a_m1;...;a_mk])] zwraca 
    graf w ktorym kazdy z elementow a_i oraz a_ij wystepuje dokladnie raz i 
    ktory jest uporzadkowany w taki sposob, ze kazdy element a_i jest przed
    kazdym z elementow a_i1 ... a_il *)
let topol g = 
  let (map, indegree) = kartograf g
  in
;;