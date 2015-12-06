open Items

type attributes = {level:int ref;
                   experience:int ref;
                   hp:(int*int) ref;
                   c_hp:int ref;
                   att:(int*int) ref;
                   def:(int*int) ref;}

type move = {name1:string;
             description1:string;
             accuracy:int;
             damage:int;}

type pokeML = {name:string;
               description:string;
               moves: move list;
               element:string;
               attributes: attributes;
               quantity: int ref;
               special: string;}

let use_ability pokeML item =
  match pokeML.special with
  | "fire" -> cook item
  | "water" -> if (item.name = "water") then Some "clean water" else None
  | "earth" -> None
  | "air" -> None
  | _ -> None

let level attr =
  attr.level:=(!(attr.level)+1);
   attr.experience:=0;
   attr.hp:=(((fst !(attr.hp))+(snd !(attr.hp))),(snd !(attr.hp)));
   attr.c_hp:=(fst !(attr.hp));
   attr.att:=(((fst !(attr.att))+(snd !(attr.att))),(snd !(attr.att)));
   attr.def:=(((fst !(attr.def))+(snd !(attr.def))),(snd !(attr.def)));()

let gain_xp attr xp =
  attr.experience:=(!(attr.experience)+xp);
  if !(attr.experience)>=(!(attr.level)*100)
  then (level attr)
  else ()

let rec gain_xp_lst lst xp =
  match lst with
  | h::t -> (gain_xp h.attributes xp); gain_xp_lst t xp
  | [] -> ()

let rec find_move lst str =
  match lst with
  | h::t -> if (h.name1 = str) then Some h else find_move t str
  | [] -> None

let find_ran_move lst =
  List.nth lst (Random.int (List.length lst))

let rec current_pokeML lst =
  match lst with
  | h::t -> if (!(h.quantity)>0) then h::(current_pokeML t) else current_pokeML t
  | [] -> []

let rec rem_pokeML lst pokeML=
  match lst with
  | h::t -> if (h = pokeML) then (h.quantity:=0;()) else (rem_pokeML t pokeML)
  | [] -> ()

let rec print_move_lst lst =
  match lst with
  | h::t -> print_string h.name1; print_move_lst t
  | [] -> ()