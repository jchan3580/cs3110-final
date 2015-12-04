open PAttributes
open PokeML
open Battle
open Items

(*Takes an item and computers its hunger boost*)
val consume_hunger_helper : item -> int

(*Takes an item and computers its thirst boost*)
val consume_thirst_helper : item -> int

(*consumes an item to increase the player's hunger bar*)
val consume : player -> item -> unit

(*uses an item on an pokeML*)
val use_item : player -> pokeML -> unit

(*Drops an item*)
val drop_item : player -> item -> unit

(*Pickup an item*)
val pickup_item : player -> item -> unit

(*initiates a battle with another player or pokeML*)
val battle : player -> player -> unit

(*Moves the player*)
val move : player -> string -> unit
