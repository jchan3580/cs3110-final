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
  {hunger= !player.hunger; thirst= !player.thirst; xp=(player.xp + xp);
  level=(level_calc (player.xp + xp)); inventory=player.inventory; pokeML=player.pokeML}

(*Takes in the experience of the player and computes the level*)
let level_calc xp : int =
  if (xp / 50) / (level * (level + 1)) = 0 then level else level + 1

let rec rem_helper lst item =
  match lst with
  | h::t -> if (h.name = item)
            then if (!(h.quantity)>0) then (h.quantity := !h.quantity - 1)
                 else print_string "You do not have this item!"; ()
            else rem_helper t item
  | [] -> ()

(*Remove item*)
let remove_item player item =
  rem_helper player.inventory item


(*Changes the inventory of the player*)
(*let use_item player : player =*)

(*Calculates the list of abilities a player has access to in order to interact
with the environment*)
(*let skills player : string list =*)