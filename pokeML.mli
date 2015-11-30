(*the type of the pokeML*)
type 'a pokeML

(*the moves a pokeML knows*)
type 'a moves

(*the attributes of a pokeML*)
type 'a attributes

(*uses a move on another pokeML*)
val use_move : 'a move -> 'a pokeML

(*increases a pokeML's abilities*)
val level : 'a attributes -> 'b attributes

