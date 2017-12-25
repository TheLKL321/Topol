(*  Autor: Łukasz Zarębski
    Code Review:  *)

(*  Zwracany w momencie gdy graf okaże się cykliczny *)
exception Cykliczne;;

(*  Typ grafu
    (node, lista połączeń) *)
type graf = ('a * 'a list) list

(*  Dla danego grafu[(a_1,[a_11;...;a_1n]); ...; (a_m,[a_m1;...;a_mk])] zwraca 
    graf w ktorym kazdy z elementow a_i oraz a_ij wystepuje dokladnie raz i 
    ktory jest uporzadkowany w taki sposob, ze kazdy element a_i jest przed
    kazdym z elementow a_i1 ... a_il *)
let topol g = 42
;;