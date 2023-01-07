let compare pic1 pic2 =
  if pic1.Outline.nb_tags < pic2.Outline.nb_tags then
    -1
  else if pic1.Outline.nb_tags > pic2.Outline.nb_tags then
    1
  else 0
;;

let sort_by_number_of_tags tab = Array.sort compare tab;;

let permutation arr =
  let swap i j =
    let tmp = arr.(i) in
    arr.(i) <- arr.(j);
    arr.(j) <- tmp;
  in
  let rec aux i =
    if i = Array.length arr
    then [Array.copy arr]
    else
      let res = ref [] in
      for j = i to Array.length arr - 1 do
        swap i j;
        res := ((aux (i+1)) @ !res);
        swap i j;
      done;
      !res
  in aux 0;;

let rev arr =
  let swap i j =
    let tmp = arr.(i) in
    arr.(i) <- arr.(j);
    arr.(j) <- tmp;
  in
  for i = 0 to ((Array.length arr) / 2) - 1 do
    swap i (Array.length arr - 1 - i)
  done;
  arr
;;
        
let rec delete_rev liste =
  match liste with
  | [] -> []
  | tete :: queue ->
    if List.mem (rev tete) queue
    then delete_rev queue
    else tete :: (delete_rev queue)
;;

let genere_possibilitees tableau =
  (*Termine sur un tableau de taille < 8 en fait 7 optimal*)
  delete_rev (permutation tableau)
;;
  
let semi_force_brut pic_tab verbose accuracy =
  let n = Array.length pic_tab in
  sort_by_number_of_tags pic_tab;
  let slide_tab = Array.map (fun pic -> Outline.Horizontal(pic)) pic_tab in
  let res = ref [Outline.Horizontal(pic_tab.(0))] in
  let i = ref 0 in
  while !i < Array.length slide_tab - 1 do    
    let sub =
      if !i + accuracy <= Array.length slide_tab then
        Array.sub slide_tab !i accuracy
      else
        Array.sub slide_tab !i (Array.length slide_tab - !i)
    in
    let all_possibilities = genere_possibilitees sub in
    let max_score_slide acc slide_tab =
      let slide_list = Array.to_list slide_tab in
      let score = Outline.score slide_list in
      let max_score = fst acc in
      if max_score < score then
        (score, slide_list)
      else
        acc
    in
    let best = List.fold_left max_score_slide (-1,[]) all_possibilities in
    res := !res @ (List.tl (snd best));
    i := !i + (accuracy - 1);
    if verbose
    then
      begin
        print_string ((string_of_int !i) ^ " / " ^ (string_of_int n));
        flush stdout;
        print_string "\r";
        flush stdout;
      end
  done;
  !res
;;
    

let algorithme unit =
  let accuracy = int_of_string Sys.argv.(1) in
  let verbose = Sys.argv.(2) = "-v" in
  let file_in = Sys.argv.(if verbose then 3 else 2) in
  let file_out = Sys.argv.(if verbose then 4 else 3) in
  let pic_tab = Outline.read_entry file_in in
  let diapo_res = semi_force_brut pic_tab verbose accuracy in
  Printf.printf "Score : %d\n" (Outline.score diapo_res);
  Outline.write_output diapo_res file_out
;;

algorithme ();;
