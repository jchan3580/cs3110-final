open PAttributes
open PActions
open Battle
open Items
open PokeML

(*Parses commands*)
val parse : string -> unit

(*Calls all the necessary components to start the game*)
val main : 'a player -> items -> 'a battle -> unit

(*repl that interacts with the player*)
val repl : string -> string