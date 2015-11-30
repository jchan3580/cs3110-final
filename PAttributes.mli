(*Creates a new player type, containing the exp, inventory, and pokeML*)
type 'a player

(*Takes in the experience of the player and computes the level*)
val level_up : 'a player -> int -> int

(*Changes the inventory of the player*)
val use_item : 'a player -> 'b player

(*Calculates the list of abilities a player has access to in order to interact
with the environment*)
val skills : 'a player -> int -> string list