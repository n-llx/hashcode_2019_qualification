let file_in = "example.txt";;
let file_out = "";;

let pict_tab = Outline.read_entry file_in;;

let compare pic1 pic2 =
  if pic1.Outline.nb_tags < pic2.Outline.nb_tags then
    -1
  else if pic1.Outline.nb_tags > pic2.Outline.nb_tags then
    1
  else 0
;;

let sort_by_number_of_tags tab = Array.sort compare pict_tab;;

let print_tab tab = Array.iter (fun x -> Printf.printf "%d\n" x.Outline.id) tab;;

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
  delete_rev (permutation tableau)
;;
  
let a = [|0;1;2;3;4|];;
let b = [|4;3;2;1;0|];;
let c = [|1;2;3|];;
let d = [|3;2;1|];;
let e = [|2|];;


