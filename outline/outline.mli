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
val read_entry : string -> picture array;;
val display_information : picture array -> unit;;
val write_output : diapo -> string -> unit;;
val score : diapo -> int;;
