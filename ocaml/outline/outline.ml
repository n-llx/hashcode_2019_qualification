(*Quelques fonctions de bases*)
type picture = {
  id : int;
  h : bool;
  nb_tags : int;
  tags : (string, unit) Hashtbl.t
}
;;

type slide =
  | Horizontal of picture
  | Vertical of picture * picture
;;

type diapo = slide list
;;

let rec just_tags liste =  
   let q1 = List.tl liste in
  let q2 = List.tl q1 in
  q2
;;
  
let rec list_in_hashtbl liste hashtbl =
   match liste with
  | [] -> ()
  | tete :: queue ->
    Hashtbl.add hashtbl tete ();
    list_in_hashtbl queue hashtbl
;;

let print_hashtbl hashtbl =
  Hashtbl.iter (fun k v -> Printf.printf "%s\n" k) hashtbl


let read_entry path_file =
  (**Lire un fichier <path_file> (donne en chemin absolu) et renvoyer un tableau d'image**)
  
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
  close_in entry_file;
  all_pictures_tab
  
;;

let display_information array =
  (**Afficher toutes les informations d'un picture array**)
  
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

let write_output diaporama path_file =
  (**Creer le fichier resultat a <path_file> correspondant au diaporama**)
  
  let output_file = open_out path_file in
  let n = List.length diaporama in
  Printf.fprintf output_file "%d\n" n;
  let rec affiche_diapo liste =
    match liste with
    | [] -> ()
    | Horizontal(pic) :: queue ->
      Printf.fprintf output_file "%d\n" pic.id;
      affiche_diapo queue
    | Vertical(pic1,pic2) :: queue ->
      Printf.fprintf output_file "%d %d\n" pic1.id pic2.id;
      affiche_diapo queue
  in affiche_diapo diaporama;
  close_out output_file;
;;

let fst (a,_,_) = a;;
let snd (_,a,_) = a;;
let thd (_,_,a) = a;;

let incr_fst ref_triple = let a,b,c = !ref_triple in ref_triple.contents <- (a+1,b,c);;
let incr_snd ref_triple = let a,b,c = !ref_triple in ref_triple.contents <- (a,b+1,c);;
let incr_thd ref_triple = let a,b,c = !ref_triple in ref_triple.contents <- (a,b,c+1);;
 
let card_intersect ht1 ht2 =
  let count = ref (0,0,0) in
  Hashtbl.iter (fun x y -> if Hashtbl.mem ht2 x then incr_snd count else incr_fst count) ht1;
  Hashtbl.iter(fun x y -> if not (Hashtbl.mem ht1 x) then incr_thd count) ht2;
  !count
;;

let min3 (a,b,c) = min (min a b) c;;

let union ht1 ht2 =
  let n = Hashtbl.length ht1 + Hashtbl.length ht2 in
  let res = Hashtbl.create n in
  Hashtbl.iter (fun x y -> Hashtbl.add res x y) ht1;
  Hashtbl.iter (fun x y -> if Hashtbl.mem res x then () else Hashtbl.add res x y) ht2;
  res
;;

let score_slide s1 s2 =
  let t_tags,q_tags =
        match s1,s2 with
        | Horizontal(pic1),Horizontal(pic2) ->
          pic1.tags, pic2.tags
        | Horizontal(pic1), Vertical(pic2,pic3) ->
          pic1.tags, (union pic2.tags pic3.tags)
        | Vertical(pic1, pic2), Horizontal(pic3) ->
          (union pic1.tags pic2.tags), pic3.tags
        | Vertical(pic1,pic2), Vertical(pic3,pic4) ->
          (union pic1.tags pic2.tags), (union pic3.tags pic4.tags)
      in
      let overall = card_intersect t_tags q_tags in
      min3 overall
;;
  

let score diaporama =
  let rec score_aux diaporama score =
    match diaporama with
    | [] -> score
    | [elt] -> score
    | t :: q :: suite ->
       score_aux (q :: suite) (score + (score_slide t q))
  in score_aux diaporama 0
;;
