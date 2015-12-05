(*open Global
open Inv*)
open Items
open PokeML

(*Creates a new player type, containing the exp, inventory, and pokeML*)
type player = {
  (*faction: Tile.faction;*)
  hunger: int ref;
  thirst: int ref;
  xp: int ref;
  level: int ref;
  inventory: item list;
  pokeML: pokeML list;}

let change_hunger player num =
  player.hunger := (!(player.hunger) + num);()

let change_thirst player num =
  player.thirst := (!(player.thirst) + num);()

(*Takes in the experience of the player and computes the level*)
let rec level_calc xp lvl: int =
  if (xp < (lvl*100)) then lvl else level_calc (xp-(lvl*100)) (lvl+1)

(*Increases the experience of a player*)
let gain_exp player xp : unit =
  (player.xp:=!(player.xp)+xp);
  (player.level:=(level_calc !(player.xp) 1));()

(*Add item
  Precondition: item must be valid*)
let rec add_item inv item =
  match inv with
  | h::t -> if (Items.(h.name) = item)
            then ((Items.(h.quantity) := !Items.(h.quantity) + 1);())
            else add_item t item
  | [] -> ()

(*Add pokeML
  Precondition: pokeML must be valid*)
let rec add_pokeML inv item =
  match inv with
  | h::t -> if (h.name = item)
            then (if (!(h.quantity)>0)
                  then (print_string "You already have this pokeML!";())
                  else ((h.quantity := 1);()))
            else add_pokeML t item
  | [] -> ()

(*Remove item*)
let rec remove_item inv item =
  match inv with
  | h::t -> if (Items.(h.name) = item)
            then if (!(Items.(h.quantity))>0)
                 then ((Items.(h.quantity) := !Items.(h.quantity) - 1);())
                 else (print_string "You do not have this item!"; ())
            else remove_item t item
  | [] -> ()

(*Checks if an item is in the inventory*)
let rec in_inv inv item =
  match inv with
  | h::t -> if (Items.(h.name) = item)
            then if (!(Items.(h.quantity))>0) then true else false
            else in_inv t item
  | [] -> false

(*Checks whether or not an item is in the pokeML inventory*)
let rec in_poke_inv inv item =
  match inv with
  | h::t -> if (h.name = item)
            then if (!(h.quantity)>0) then true else false
            else in_poke_inv t item
  | [] -> false

(*Selects a pokeML from a list*)
let rec sel_pokeML inv str =
  match inv with
  | h::t -> if (h.name = str) then h else sel_pokeML t str
  | [] -> {name="";
           description="";
           moves=[{name1="";description1="";accuracy=0;damage=0;}];
           element="";
           attributes={level=ref 0;experience=ref 0;hp=ref (0,0);
           c_hp=ref 0;att=ref (0,0);def=ref (0,0);};
           quantity=ref 0;
           special="";}