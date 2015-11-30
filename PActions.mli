open PAttributes
open PokeML
open Battle
open Items

(*consumes an item to increase the player's hunger bar*)
val eat : 'a item -> 'a player

(*uses an item on an pokeML*)
val use_item : 'a player -> 'a pokeML

(*initiates a battle with another player or pokeML*)
val battle : 'a player -> 'b player -> unit
