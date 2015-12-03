type attributes = {level:int;
                   experience:int;
                   hp:(int*int);
                   att:(int*int);
                   def:(int*int);}

type move = {name:string;
             description:string;
             accuracy:int;
             damage:int;}

type pokeML = {name:string;
               description:string;
               moves: move list;
               element:string;
               attributes: attributes;
               special: string;}

let use_ability poke =
  match poke.special with
  | "fire" -> ()
  | "water" -> ()
  | "earth" -> ()
  | "air" -> ()

let level attr =
  {level=attr.level+1;
   experience=0;
   hp=(((fst attr.hp)+(snd attr.hp)),(snd attr.hp));
   att=(((fst attr.att)+(snd attr.att)),(snd attr.att));
   def=(((fst attr.def)+(snd attr.def)),(snd attr.def));}