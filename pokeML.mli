(*the attributes a pokeML has *)
type attributes

(*the moves a pokeML knows*)
type move

(*the type of the pokeML*)
type pokeML

(*uses a move on another pokeML*)
val use_move : move -> pokeML

(*increases a pokeML's abilities*)
val level : attributes -> attributes

