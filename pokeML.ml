open Items

type attributes = {level:int;
                   experience:int;
                   hp:(int*int);
                   c_hp:int;
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
               quantity: int ref;
               special: string;}

let use_ability pokeML item =
  match pokeML.special with
  | "fire" -> cook item
  | "water" -> if (item.name = "Water") then Some "CleanWater" else None
  | "earth" -> None
  | "air" -> None
  | _ -> None

let level attr =
  {level=attr.level+1;
   experience=0;
   hp=(((fst attr.hp)+(snd attr.hp)),(snd attr.hp));
   c_hp=((fst attr.hp)+(snd attr.hp));
   att=(((fst attr.att)+(snd attr.att)),(snd attr.att));
   def=(((fst attr.def)+(snd attr.def)),(snd attr.def));}