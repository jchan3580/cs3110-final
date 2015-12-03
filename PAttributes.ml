(*Creates a new player type, containing the exp, inventory, and pokeML*)
type player = {hunger: int; thirst: int; xp: int; level: int;
  inventory: item list; pokeML: pokeML list}

(*Increases the experience of a player*)
let gain_xp player xp : player =
  {hunger=player.hunger; thirst=player.thirst; xp=(player.xp + xp);
  level=(level_calc (player.xp + xp)); inventory=player.inventory; pokeML=player.pokeML}

(*Takes in the experience of the player and computes the level*)
let level_calc xp : int =
  xp / 100 + 1

(*Changes the inventory of the player*)
(*let use_item player : player =*)

(*Calculates the list of abilities a player has access to in order to interact
with the environment*)
(*let skills player : string list =*)