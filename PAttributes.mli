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
  pokeML: pokeML list}

val change_hunger : player -> int -> unit

val change_thirst : player -> int -> unit

(*Increases the experience of a player*)
val gain_exp : player -> int -> unit

(*Takes in the experience of the player and computes the level*)
val level_calc : int -> int -> int

(*Remove helper function*)
val add_item : item list -> string -> unit

val add_pokeML : pokeML list -> string -> unit

(*Remove an item from the player inventory*)
val remove_item : item list -> string -> unit

(*Checks whether or not an item is in the inventory*)
val in_inv : item list -> string -> bool

(*Checks whether or not an item is in the pokeML inventory*)
val in_poke_inv : pokeML list -> string -> bool

(*Selects a pokeML from a list*)
val sel_pokeML : pokeML list -> string -> pokeML