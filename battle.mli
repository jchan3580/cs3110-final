open MLGame
open PokeML

(*pokeML attributes that factor into how a battle will turn out*)
type 'a battle

(*the players involved in the game*)
type 'a player

(*translates the command to something that can be used by the main method*)
val commands : string -> list

(*checks to see whether health is 0 *)
val end_condition : 'a battle  -> bool

(*has the methods that modify the attributes according to the specific command
parsed *)
val main : 'a battle -> 'b battle -> 'a player -> 'b player ->
  ('c battle * 'c player)