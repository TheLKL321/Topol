(*  Autor: Łukasz Zarębski
    Code Review:  *)

(*open PMap;; TODO*)

(*  Zwracany w momencie gdy graf okaże się cykliczny *)
exception Cykliczne;;

(*  Typ grafu, lista sąsiedztwa
    (node n, lista node'ów do których node n ma skierowaną krawędź) *)
type 'a graf = ('a * 'a list) list

(*  Zwraca mapę list node'ów do których dany node ma skierowaną krawędź, 
    kluczowaną node'ami. W żadnej z takich list nie występują duplikaty, lecz 
    niekoniecznie każdy element listy znajduje się w uzyskanej mapie jako node*)
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

(*  Tworzy mapę kluczowaną node'ami, zawierającą liczbę krawędzi skierowanych
    do danego node'a *)
let inDegrees m =
  let indegree = ref empty
  in let f k lis = 
    if not (mem k !indegree) then indegree := add k 0 !indegree;
    let filler x = 
      try
        indegree := add x (find x !indegree + 1) !indegree;
      with
        | Not_found -> indegree := add x 1 !indegree;
        | _ -> failwith "Unknown inDegrees filler"
    in List.iter filler lis;
  in iter f m;
  !indegree

(*  Dla danego grafu[(a_1,[a_11;...;a_1n]); ...; (a_m,[a_m1;...;a_mk])] zwraca 
    graf w ktorym kazdy z elementow a_i oraz a_ij wystepuje dokladnie raz i 
    ktory jest uporzadkowany w taki sposob, ze kazdy element a_i jest przed
    kazdym z elementow a_i1 ... a_il *)
let topol g = 
  let queue = ref []
  and mapa = ref (kartograf g)
  and result = ref []
  in 
    let indegree = ref (inDegrees !mapa)
    (*  Wypełnianie kolejki node'ami do których nie ma skierowanych krawędzi *)
    in 
      let f1 k v = 
        if v = 0 then queue := k :: !queue
      (*  Przenoszenie danego node'a z indegree do listy wynikowej i usuwanie
          jego krawędzi *)
      and f2 x = 
        result := x :: !result;
        indegree := remove x !indegree;
        (*  Usuwanie krawędzi node'a *)
        try
          let f21 y = 
            indegree := add y (find y !indegree - 1) !indegree
          in 
            List.iter f21 (find x !mapa)
        with
          | Not_found -> ()
          | _ -> failwith "Unknown topol f2"
      in 
        while not (is_empty !indegree) do
          iter f1 !indegree;
          if !queue = [] then raise Cykliczne;
          List.iter f2 !queue;
          queue := [];
        done;
        List.rev !result
;;