open PAttributes
open PokeML

(*translates the command to something that can be used by the main method*)
val commands : string -> unit

(*checks to see whether health is 0 *)
val end_condition : player  -> bool

(*has the methods that modify the attributes according to the specific command
parsed *)
val main : player -> player -> player