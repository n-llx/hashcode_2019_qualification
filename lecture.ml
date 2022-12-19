type picture = {
  id : int;
  h : bool;
  nb_tags : int;
  tags : (string, unit) Hashtbl.t
}
;;

(*Chemin du fichier a lire*)
let path_file = "a_example.txt";;

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





  
    




    
