open Items
(*the attributes a pokeML has *)
type attributes = {level:int ref;
                   experience:int ref;
                   hp:(int*int) ref;
                   c_hp:int ref;
                   att:(int*int) ref;
                   def:(int*int) ref;}

(*the moves a pokeML knows*)
type move = {name1:string;
             description1:string;
             accuracy:int;
             damage:int;}

(*the type of the pokeML*)
type pokeML = {name:string;
               description:string;
               moves: move list;
               element:string;
               attributes: attributes;
               quantity: int ref;
               special: string;}

(*uses a pokeML ability*)
val use_ability : pokeML -> item -> string option

val gain_xp : attributes -> int -> unit

val gain_xp_lst : pokeML list -> int -> unit

(*increases a pokeML's abilities*)
val level : attributes -> unit

val find_move : move list -> string -> move

val find_ran_move : move list -> move

val current_pokeML : pokeML list -> pokeML list

val rem_pokeML : pokeML list -> pokeML -> unit
