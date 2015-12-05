(*the attributes a pokeML has *)
type attributes = {level:int;
                   experience:int;
                   hp:(int*int);
                   c_hp:int;
                   att:(int*int);
                   def:(int*int);}

(*the moves a pokeML knows*)
type move = {name:string;
             description:string;
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

(*increases a pokeML's abilities*)
val level : attributes -> attributes

