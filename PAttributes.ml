open Global
open Inv
open Items

(*Creates a new player type, containing the exp, inventory, and pokeML*)
type player = {
  faction: Tile.faction;
  hunger: int ref;
  thirst: int ref;
  xp: int;
  level: int;
  inventory: item list;
  pokeML: pokeML list}

(*Increases the experience of a player*)
let gain_xp player xp : player =
  {faction=player.faction; hunger= player.hunger; thirst= player.thirst;
  xp=(player.xp + xp); level=(level_calc (player.xp + xp));
  inventory=player.inventory; pokeML=player.pokeML}

(*Takes in the experience of the player and computes the level*)
let rec level_calc xp lvl: int =
  if (xp < (lvl*100)) then lvl else level_calc (xp-(lvl*100)) (lvl+1)

(*Add item
  Precondition: item must be valid*)
let rec add_item inv item =
  match inv with
  | h::t -> if (h.name = item)
            then (h.quantity := !h.quantity + 1);
            else add_item t item
  | [] -> ()

(*Remove item*)
let rec remove_item inv item =
  match inv with
  | h::t -> if (h.name = item)
            then if (!(h.quantity)>0) then (h.quantity := !h.quantity - 1);
                 else print_string "You do not have this item!"; ()
            else remove_item t item
  | [] -> ()

(*Checks if an item is in the inventory*)
let rec in_inv inv item =
  match inv with
  | h::t -> if (h.name = item)
            then if (!(h.quantity)>0) then true else false
            else in_inv t item
  | [] -> false

(*Calculates the list of abilities a player has access to in order to interact
with the environment*)
(*let skills player : string list =*)