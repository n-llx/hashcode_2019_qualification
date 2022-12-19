type picture =
{
  id : int;
  h : bool;
  nb_tags : int;
  tags : (string, unit) Hashtbl.t;
}

(* On définit l'image vide avec un id = -1 *)

type slide = {
  mutable content : picture * picture; 
  h : bool (* Un diapo contient ou 2 images verticales soit une image
     horizontale et une image vide *)
};; 
  
(*fonctions de test *)

let create_picture id h tags =
  (* cette fonction retourne une image une image, tags est une string list *)
  let nb_tags = List.length tags in
  let t = Hashtbl.create nb_tags in
  let rec aux tags =
    match tags with
    |[] -> ()
    |tag::q -> Hashtbl.add t tag ();
    aux q; in
  aux tags;
  let res = {
    id = id;
    h = h;
    nb_tags = nb_tags;
    tags = t;
  } in
  res;;

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
    end;;


let score slide1 slide2 =
  let nb_common = snd (intersect slide1 slide2) in
  let (picture1,picture2) = slide1.content in
  let picture3,picture4 = slide2.content in
  let nb_tag_slide1 = picture1.nb_tags + picture2.nb_tags in
  let nb_tag_slide2 = picture3.nb_tags + picture4.nb_tags in
  min (min (nb_tag_slide1 - nb_common) (nb_common)) (nb_tag_slide2 - nb_common);;

let greedy_diapo pictures n = (* algo en O(n^4)*)
  let used = Array.make n false in (*tableau des images déjà utilisé*)
  let res = Array.make (n + 1) (-1,-1) in
  let empty_picture = create_picture (-1) false [] in
  used.(0) <- true;
  res.(0) <- (0,-1);

  let nb_used = ref 1 in
  let nb_diapo = ref 1 in

  while !nb_used < n do

    let maxi = ref (-1) in
    let id_maxi = ref (-1,-1) in
    print_int pictures.(fst res.(!nb_diapo - 1)).id;
    let slide1 = {
      content = (pictures.(fst res.(!nb_diapo - 1)),empty_picture);
      h = pictures.(fst res.(!nb_diapo - 1)).h;
    } in

    for i = 0 to 1 do
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
    if snd (!id_maxi) = -1 then
      (
      res.(!nb_diapo) <- !id_maxi;
      incr nb_used;
      incr nb_diapo;
      used.(fst !id_maxi) <- true;)
    else
      (
      res.(!nb_diapo) <- !id_maxi;
      nb_used := !nb_used + 2;
      incr nb_diapo;
      used.(fst !id_maxi) <- true;
      used.(snd !id_maxi) <- true;)
  done;
  res,!nb_diapo;;

let res = greedy_diapo test 6;;

let display_res res n =
  print_int n;
  print_newline();
  for i = 0 to n - 1 do
    if snd res.(i) = -1 then
      (print_int (fst res.(i));
      print_newline();)
    else if fst res.(i) <> - 1 then
      (print_int (fst res.(i));
      print_string " ";
      print_int (snd res.(i));
      print_newline();)
    done;;
    

(*fonctions de test *)

let picture1 = create_picture 1 true ["cat";"dog";"selfi";"garden";"tomato"];;
let picture2 = create_picture 2 true ["selfi";"garden";"horse";"computer"];;
let picture3 = create_picture 3 false ["selfi";"garden";"horse";"computer"];;
let picture4 = create_picture 4 false ["sport";"flower";"horse";"food"];;
let picture5 = create_picture 5 false ["desk";"sun";"horse";"nature"];;
let picture6 = create_picture 6 false ["sun";"garden";"food";"computer"];;
let empty_picture = create_picture (-1) false [];; (*image vide pour mettre avec une image horizontale
   ou avec une ilage verticale seul dans une slide*)


let slide1 =
  {
    content = (picture1, empty_picture);
    h = true;
  };;

let slide2 =
  {
    content = (picture2, empty_picture);
    h = true;
  };;

let slide3 =
  {
    content = (picture3, picture4);
    h = false;
  };;

let slide4 =
  {
    content = (picture5,picture6);
    h = false;
  };;

let test = [|picture1;picture2;picture3;picture4;picture5;picture6|];;

let res = greedy_diapo test 6;;

display_res (fst res) (snd res);;
