let best_match pic pictureArray vu =
  let max = ref (-1) in
  let id = ref (-1) in
  for i = 0 to Array.length pictureArray - 1 do
    if not (i = pic.Outline.id || vu.(i)) then
      vu.(i) <- true;
      let score = Outline.score_slide (Outline.Horizontal(pic)) (Outline.Horizontal(pictureArray.(i))) in
      if score > !max then
        begin
          id := i;
          max := score;
        end
  done;
  !id
;;

let genere_slide pictureArray verbose =
  let n = Array.length pictureArray in
  let vu = Array.make n false in
  let res = ref [pictureArray.(0)] in
  for i = 1 to n - 1 do
    let bestMatchI = best_match pictureArray.(i-1) pictureArray vu in
    res := (pictureArray.(bestMatchI)) :: !res;
    if verbose then
      begin
        print_string ((string_of_int i) ^ " / " ^ (string_of_int n));
        flush stdout;
        print_string "\r";
        flush stdout;
      end
  done;
  !res
;; 


(**)
(* let final () = *)
(*   if Sys.argc < 4 then failwith "./glouon [-v] <fichierEntree> <fichierSortie>" *)
(*   else *)
(*     begin *)
(*       let verbose = Sys.argv[1] = "-v" in *)
(*       let add = if verbose then 1 else 0 in *)
(*       let fileIn = Sys.argv[2] in *)
(*       let fileOut = Sys.argv[3] in *)
(**)
(*     end *)
