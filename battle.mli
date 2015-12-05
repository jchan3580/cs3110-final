open PAttributes
open PokeML

type result = | Flee | Switch | Dead of pokeML

(*checks to see whether health is 0 *)
val dead : pokeML  -> bool

val attack : pokeML -> move -> pokeML -> unit

val battle : pokeML -> pokeML -> result

val fight : pokeML list -> pokeML -> bool

val refreshHP: pokeML list -> unit

val main : player -> pokeML -> unit