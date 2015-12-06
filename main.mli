open PAttributes
open PActions
open Battle
open Items
open PokeML

(*Parses commands*)
val parse : player -> string -> unit

(*Calls all the necessary components to start the game*)
val main : player -> unit