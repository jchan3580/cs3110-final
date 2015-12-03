(*Creates a new player type, containing the exp, inventory, and pokeML*)
type player

(*Increases the experience of a player*)
val gain_xp : player -> int -> player

(*Takes in the experience of the player and computes the level*)
val level_calc : player -> player

(*Changes the inventory of the player*)
val use_item : player -> player

(*Calculates the list of abilities a player has access to in order to interact
with the environment*)
val skills : player -> string list