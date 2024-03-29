open PAttributes
open PActions
open Battle
open Items
open PokeML

type room = {items: item list; pokeML: pokeML list}

val get_items : item list -> string -> string

val get_pokeML : pokeML list -> string -> string

val room_description : room -> string

val gen_room_items : item list -> pokeML list -> room

(*Parses commands*)
val parse : player -> string -> room ref-> unit

(*Calls all the necessary components to start the game*)
val main : player -> room ref -> 'a