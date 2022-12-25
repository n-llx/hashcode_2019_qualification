(*./executable <borne> <fichier_source> <fichier_sortie>*)

(*
commandes utiles uniquement pour le top-level : 
permet d'importer le module definit par le fichier outline.mli
   
#directory "../outline";;
#load "outline.cmo";;
*)
Random.self_init ();;

let nb_iteration = 500;;

let shuffle array =
  (** 'a array -> unit **)
  (** Melanger sur place de maniere aleatoire un tableau **)
  
  let n = Array.length array in
  for i = 0 to n - 1 do
    let new_pos = Random.int n in
    let tmp = array.(i) in
    array.(i) <- array.(new_pos);
    array.(new_pos) <- tmp;
  done;;

let id_h pic_array =
  (** picture array -> int list **)
  (** Recuperer les indices (ie les positions) des "pictures" verticales **)
  
  let id = ref [] in
  let n = Array.length pic_array in
  for i = 0 to n - 1 do
    if not (pic_array.(i).Outline.h) then
      id := i :: !id;
  done;
  !id
;;

let rec delete_elt liste elt =
  (** 'a list -> 'a -> 'a list**)
  
  match liste with
  | [] -> []
  | tete :: queue ->
    if tete = elt then delete_elt queue elt
    else tete :: (delete_elt queue elt)
;;
  
let creer_diapo shuffled_pict_tab =
  (** picture array -> slide list **)
  (** Creer une diapositive a partir d'un tableau de "picture" melange de maniere aleatoire**)
  
  let n = Array.length shuffled_pict_tab in
  let horizontal_id = ref (id_h shuffled_pict_tab) in
  let diapo = ref [] in
  for i = 0 to n - 1 do
    if shuffled_pict_tab.(i).Outline.h then
      begin
        diapo := Outline.Horizontal(shuffled_pict_tab.(i)) :: !diapo
      end
    else if List.mem i !horizontal_id then
      begin
        horizontal_id := (delete_elt !horizontal_id i);
        let other_horizontal_pic_id = List.hd !horizontal_id in
        let other_horizontal_pic = shuffled_pict_tab.(other_horizontal_pic_id) in
        diapo := Outline.Vertical(shuffled_pict_tab.(i), other_horizontal_pic) :: !diapo;
        horizontal_id := List.tl !horizontal_id
      end;
  done;
  List.rev !diapo
;;

let random_diapo pic_tab =
  (** picture array -> slide list * int **)
  (**A partir d'un tableau non melange, renvoyer une diapositive aleatoire ainsi que son score **)
  
  shuffle pic_tab;
  let diapo = creer_diapo pic_tab in
  let score = Outline.score diapo in
  (diapo,score)
;;


exception Found of Outline.slide list * int;;

let algo_probabiliste pict_tab borne file =
  (** picture array -> int -> string -> unit **)
  (** Essayer de trouver une instance du probleme dont le score est superieur a <borne> et l'ecrire dans un fichier <file>**)
  
  try
    for i = 0 to nb_iteration - 1 do
      let diapo,score = random_diapo pict_tab in
      if score >= borne then
        raise (Found (diapo,score))
    done;
    Printf.printf "Aucune solution n'a ete trouvee.\n";
  with
  |Found (res,score) ->
    Outline.write_output res file;
    Printf.printf "Score : %d\n" score;
;;  


let final () =
  let file_in = Sys.argv.(2) in
  let file_out = Sys.argv.(3) in
  let picture_tab_lecture = Outline.read_entry file_in in
  let borne = int_of_string (Sys.argv.(1)) in
  algo_probabiliste picture_tab_lecture borne file_out;
;;

final ();;



        
            
    
