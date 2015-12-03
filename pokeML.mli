(*the attributes a pokeML has *)
type attributes

(*the moves a pokeML knows*)
type move

(*the type of the pokeML*)
type pokeML

(*uses a pokeML ability*)
val use_ability : pokeML -> unit

(*increases a pokeML's abilities*)
val level : attributes -> attributes

