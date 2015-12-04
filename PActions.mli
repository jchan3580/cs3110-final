open PAttributes
open PokeML
open Battle
open Items

(*Takes an item and computers its hunger boost*)
val consume_hunger_helper : item -> int

(*Takes an item and computers its thirst boost*)
val consume_thirst_helper : item -> int

(*consumes an item to increase the player's hunger bar*)
val consume : item -> player

(*uses an item on an pokeML*)
val use_item : 'a player -> 'a pokeML

(*initiates a battle with another player or pokeML*)
val battle : 'a player -> 'b player -> unit
