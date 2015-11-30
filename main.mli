open MLGame
open PAttributes
open pActions
open Battle
open Items
open PokeML

(*Calls all the necessary components to start the game*)
val main : mlgame -> 'a player -> 'a items -> 'a battle -> unit

(*repl that interacts with the player*)
val repl : string -> string