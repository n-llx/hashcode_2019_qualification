type picture = {
  id : int;
  h : bool;
  nb_tags : int;
  tags : (string, unit) Hashtbl.t}
;;

(*Chemin du fichier a lire*)
let rec just_tags liste =
  
  (*Recupere uniquement les tags d'une liste*)
  
  let q1 = List.tl liste in
  let q2 = List.tl q1 in
  q2
;;
  
let rec list_in_hashtbl liste hashtbl =
  (*Rentre les elements d'une liste dans une hashtbl*)
  
  match liste with
  | [] -> ()
  | tete :: queue ->
    Hashtbl.add hashtbl tete ();
    list_in_hashtbl queue hashtbl
;;

let read_entry path_file =
  (*A partir d'un fichier <path_file>, renvoie un tableau d'image*)
  
  let entry_file = open_in path_file in
  let nb_picture = int_of_string (input_line entry_file) in

  let empty_hashtbl = Hashtbl.create 0 in
  let empty_picture = {id = 0; h = false; nb_tags = 0; tags = empty_hashtbl} in
  let all_pictures_tab = Array.make (nb_picture) empty_picture in 
  for i = 0 to nb_picture -1 do
    let all_line = input_line entry_file in
    let parse_line = String.split_on_char ' ' all_line in
    let horizontal = (List.hd parse_line) = "H" in
    let nb_tags = int_of_string (List.nth parse_line 1) in
    let tags = Hashtbl.create 100 in
    list_in_hashtbl (just_tags parse_line) tags;
    all_pictures_tab.(i) <- {id = i; h = horizontal; nb_tags = nb_tags; tags = tags}
  done;
  all_pictures_tab
;;
    
let print_hashtbl hashtbl =
  Hashtbl.iter (fun k v -> Printf.printf "%s\n" k) hashtbl
;;

let display_information array =
  (*Affiche les informations des images contenues dans un tableau*)
  
  let n = Array.length array in
  for i = 0 to n - 1 do
    let a = array.(i).id in
    let b = if array.(i).h then "oui" else "non" in
    let c = array.(i).nb_tags in
    Printf.printf "id : %d | horizontale : %s | nb_tags : %d \ntags :\n" a b c;
    print_hashtbl (array.(i).tags);
    Printf.printf "\n"
  done;
;;

(* On définit l'image vide avec un id = -1 *)

type slide = {
  mutable content : picture * picture; 
  h : bool (* Un diapo contient ou 2 images verticales soit une image
     horizontale et une image vide *)
};; 

let intersect slide1 slide2 =
  (*retourne la liste des tags commun a 2 slides et leurs nombre sous forme d'un coupe (list_tag, nombre_tag)*)
  let res = ref [] in
  let count = ref 0 in
  if slide1.h && slide2.h then (* cas ou les 2 slides ont des photos horizontales *)
    let (picture1,_) = slide1.content in
    let (picture2,_) =slide2.content in
    begin
    Hashtbl.iter (fun tag () -> if Hashtbl.mem picture2.tags tag then (res := tag::!res;incr count;)) (picture1.tags);
    (!res,!count);
    end

  else if slide1.h then (* cas ou seul le 1er slide à photos horizontales *)
    let (picture1,_) = slide1.content in
    let (picture2,picture3) = slide2.content in
    begin
      Hashtbl.iter (fun tag () -> if (Hashtbl.mem picture2.tags tag) || (Hashtbl.mem picture3.tags tag)  then (res := tag::!res;incr count;)) (picture1.tags);
      (!res,!count);
    end

  else if slide2.h then
    let (picture1,picture2) = slide2.content in
    let (picture3,_) = slide1.content in
    begin
      Hashtbl.iter (fun tag () -> if (Hashtbl.mem picture2.tags tag) || (Hashtbl.mem picture1.tags tag)  then (res := tag::!res;incr count;)) (picture3.tags);
      (!res,!count);
    end

  else (*cas ou les 2 slides ont des photos horizontales*)
  let (picture1,picture2) = slide1.content in
  let (picture3,picture4) = slide2.content in
    begin
      Hashtbl.iter (fun tag () -> if (Hashtbl.mem picture2.tags tag) || (Hashtbl.mem picture1.tags tag)  then (res := tag::!res;incr count;)) (picture3.tags);
      Hashtbl.iter (fun tag () -> if (Hashtbl.mem picture2.tags tag) || (Hashtbl.mem picture1.tags tag)  then (res := tag::!res;incr count;)) (picture4.tags);
      (!res,!count);
    end
;;

let score slide1 slide2 =
  let nb_common = snd (intersect slide1 slide2) in
  let (picture1,picture2) = slide1.content in
  let picture3,picture4 = slide2.content in
  let nb_tag_slide1 = picture1.nb_tags + picture2.nb_tags in
  let nb_tag_slide2 = picture3.nb_tags + picture4.nb_tags in
  min (min (nb_tag_slide1 - nb_common) (nb_common)) (nb_tag_slide2 - nb_common)
;;


let greedy_diapo (pictures : picture array) (n : int) = (* algo en O(n^3)*)
  let used = Array.make n false in (*tableau des images déjà utilisé*)
  let res = Array.make (n + 1) (-1,-1) in
  let empty_picture = {id = -1; nb_tags = 0; h = false; tags = Hashtbl.create 0} in
  used.(0) <- true;
  res.(0) <- (0,-1);
  
  let id = ref 0 in 
  for i = 0 to 0 do
    if not pictures.(0).h then
      while pictures.(!id).h do
        (
          incr id;
        )
      done;
    (
      used.(!id) <- true;
      res.(0) <- (0,!id);
    )
    done;

  let nb_used = ref 1 in
  let nb_diapo = ref 1 in

  while !nb_used < n do
    let maxi = ref (-1) in
    let id_maxi = ref (-1,-1) in
    let slide1 = {
      content = (pictures.(fst res.(!nb_diapo - 1)),empty_picture);
      h = pictures.(fst res.(!nb_diapo - 1)).h;
    } in
    for i = 0 to 0 do
      if not slide1.h then (* me demandez pas pourquoi mais sans cette boucle inutile j'ai une erreur*)
        (slide1.content <- (pictures.(fst res.(!nb_diapo - 1)),pictures.(snd res.(!nb_diapo - 1)));)
      done;
    for i = 0 to n - 1 do
      if not used.(i) && i <> fst res.(!nb_diapo - 1) && i <> snd res.(!nb_diapo -1) && not pictures.(i).h then
        for j = 0 to n - 1 do
          if not pictures.(j).h && not used.(j) && i <> j && j <> fst res.(!nb_diapo - 1) && j <> snd res.(!nb_diapo -1) && (score slide1 {content = (pictures.(i),pictures.(j)); h = false}) > !maxi then
            begin
            maxi := (score slide1 {content = (pictures.(i),pictures.(j)); h = false});
            id_maxi := (i,j);
            end
          done
      else if not used.(i) && i <> fst res.(!nb_diapo - 1) && i <> snd res.(!nb_diapo -1) then
        let slide2 = {content = (pictures.(i),empty_picture); h = true;} in
        if score slide1 slide2 > !maxi then
          begin
            maxi := (score slide1 {content = (pictures.(i),empty_picture); h = false});
            id_maxi := (i,-1);
          end
    done;

    if !nb_used = n - 1 then
      for i = 0 to n - 1 do
        if not used.(i) then
          (id_maxi := (i,-1);)
        done;


    if snd (!id_maxi) = -1 then
      (
      res.(!nb_diapo) <- !id_maxi;
      incr nb_used;
      incr nb_diapo;
      print_int (fst !id_maxi);
      used.(fst !id_maxi) <- true;)
    else
      (
      res.(!nb_diapo) <- !id_maxi;
      nb_used := !nb_used + 2;
      incr nb_diapo;
      used.(fst !id_maxi) <- true;
      used.(snd !id_maxi) <- true;)
  done;
  res,!nb_diapo
;;

let display_res res n =
  print_int n;
  print_newline();
  for i = 0 to n - 1 do
    if snd res.(i) = -1 && (fst res.(i)) <> -1 then
      (print_int (fst res.(i));
      print_newline();)
    else if fst res.(i) <> - 1 then
      (print_int (fst res.(i));
      print_string " ";
      print_int (snd res.(i));
      print_newline();)
    done
;;
