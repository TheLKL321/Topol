(*  Autor: Łukasz Zarębski
    Code Review: Jakub Kowalski *)

(*open PMap;;*)

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
      if mem node !mapa then 
        mapa := add node (lis @ (find node !mapa)) !mapa
      else mapa := add node lis !mapa
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
  and queueTemp = ref []
  and mapa = ref (kartograf g)
  and result = ref []
  in 
    let indegree = ref (inDegrees !mapa)
    in 
      (*  Wypełnianie kolejki node'ami do których nie ma skierowanych krawędzi*)
      let f1 k v = 
        if v = 0 then queue := k :: !queue
      (*  Przenoszenie danego node'a z indegree do listy wynikowej i usuwanie
          jego krawędzi. Jeśli unsunięta zostanie ostatnia krawędź node'a, jest 
          on dodawany do kolejki tymczasowej *)
      and f2 x = 
        result := x :: !result;
        indegree := remove x !indegree;
        try
          (*  Usuwanie krawędzi/dodawanie do kolejki tymczasowej *)
          let f21 y = 
            let value = find y !indegree - 1
            in
              if value = 0 then queueTemp := y :: !queueTemp
              else indegree := add y value !indegree
          in 
            List.iter f21 (find x !mapa)
        with
          | Not_found -> ()
          | _ -> failwith "Unknown topol f2"
      in 
        iter f1 !indegree;
        while not (is_empty !indegree) do
          if !queue = [] then raise Cykliczne;
          List.iter f2 !queue;
          queue := !queueTemp;
          queueTemp := [];
        done;
        List.rev !result
;;