open PAttributes
open PActions
open Battle
open Items
open PokeML

type room = {items: item list; pokeML: pokeML list}

let rec get_items (items : item list) (accum : string) =
  match items with
  | [] -> if (String.length accum >= 2)
  then String.sub accum 0 ((String.length accum) - 2) else accum
  | h::t -> if (!(h.quantity)>0)
  then get_items t (accum ^ (string_of_int !(h.quantity)) ^ (h.name) ^ ", ")
  else get_items t accum

let rec get_pokeML (pokeML : pokeML list) (accum : string) =
  match pokeML with
  | [] -> (if (String.length accum >= 2)
    then ("Also, you see " ^
            (String.sub accum 0 ((String.length accum) - 2) ^ "!"))
    else accum)
  | h::t -> if (!(h.quantity)>0)
  then get_pokeML t (accum ^ (string_of_int !(h.quantity)) ^ (h.name) ^ ", ")
  else get_pokeML t accum

let room_description (room : room) : string =
  "
   / \\    (  (    )     \\       \\       \\ \n
   / \\  ( (     )   )  / \\     / \\     / \\ \n
  \\ |   ( (         )  / \\     / \\     / \\
 / \\    (           / ) |    \\  |  /    |        \\ \n
 / \\    ( ( \\       )      / \\   / \\      \\    / \\ \n
  |        ( |  // )    \\   / \\   / \\     / \\   / \\ \n
       ,'',   |   | /   / \\   |     |/     / \\    | \n
    /``   ''\\     / \\  / \\    /    / \\     |      \\ \n
 {``         ''}  / \\   |/   / \\   / \\     \\     / \\ \n
{               }  |    / \\  / \\    |     / \\    / \\   \n
 `'`  \\ //  `'`    \\    / \\   |        /  / \\     |    \n
       | |     /  / \\  \\ |   \\    \\   / \\  |     \\   \n
      // \\   / \\ / \\ / \\   / \\  / \\  / \\       / \\ \n
 / \\          / \\  |  / \\   / \\  / \\   |        / \\ \n
 / \\     \\     |   /   |     |    |       /      | \n
  |     / \\       / \\      \\             / \\ \n
        / \\       / \\     / \\            / \\ \n
         |         |      / \\             | \n
                           | \n

  You come to a clearing in the forest.  On the ground you see " ^
  (get_items room.items "") ^ "." ^ (get_pokeML room.pokeML "")

let parse player str room=
  if (String.length str >= 4 &&
      String.sub (String.lowercase str) 0 4 = "eat ") then
    (let command = String.sub (String.lowercase str) 4
                    ((String.length (String.lowercase str))-4) in
        match (sel_item player.inventory command) with
        | Some x -> consume player x
        | None -> print_string "\nThat is not a valid item!";())
  else if (String.length str >= 5 &&
      String.sub (String.lowercase str) 0 5 = "drop ") then
    (let command = String.sub (String.lowercase str) 5
                    ((String.length (String.lowercase str))-5) in
        match (sel_item player.inventory command) with
        | Some x -> drop_item player x
        | None -> print_string "\nThat is not a valid item!";())
  else if (String.length str >= 8 &&
      String.sub (String.lowercase str) 0 8 = "pick up ") then
    (let command = String.sub (String.lowercase str) 8
                    ((String.length (String.lowercase str))-8) in
        match (sel_item room.items command) with
        | Some x -> pickup_item player x
        | None -> print_string "\nThat is not a valid item!";())
  else if (String.length str >= 4 &&
      String.sub (String.lowercase str) 0 4 = "use ") then
    (let command = String.sub (String.lowercase str) 4
                    ((String.length (String.lowercase str))-4) in
        print_string "\nWhat would you like to use this on?";
        let input = read_line () in
        if (in_poke_inv player.pokeML command)
        then
        (match ((sel_pokeML player.pokeML command),
                      (sel_item player.inventory input)) with
             | Some x, Some y -> use_pokeML player x y
             | None, Some x -> (print_string "\nThat is not a valid pokeML!";())
             | _ -> (print_string "\nThat is not a valid item!";()))
        else
        (match ((sel_item player.inventory command),
                      (sel_item player.inventory input)) with
             | Some x, Some y -> use_item player x y
             | _ -> (print_string "\nThose are not valid items!";())))
  else if (String.length str >= 5 &&
      String.sub (String.lowercase str) 0 5 = "move ") then
    (let command = String.sub (String.lowercase str) 5
                    ((String.length (String.lowercase str))-5) in
        move player command)
  else if (String.length str >= 7 &&
      String.sub (String.lowercase str) 0 7 = "battle ") then
    (let command = String.sub (String.lowercase str) 7
                    ((String.length (String.lowercase str))-7) in
        match (sel_pokeML room.pokeML command) with
        | Some x -> PActions.battle player x
        | None -> (print_string "\nThat is not a valid pokeML!";()))
  else if (String.length str >= 8 &&
      String.sub (String.lowercase str) 0 8 = "examine ") then
    (let command = String.sub (String.lowercase str) 8
                    ((String.length (String.lowercase str))-8) in
        if (in_poke_inv player.pokeML command)
        then
        (match (sel_pokeML player.pokeML command) with
             | Some x -> (print_newline(); print_string x.name;)
             | None -> (print_string "\nThat is not a valid pokeML!";()))
        else if (in_inv player.inventory command)
        then
        (match (sel_item player.inventory command) with
             | Some x -> (print_newline(); print_string Items.(x.name);)
             | _ -> (print_string "\nThat is not a valid item!";()))
        else if (in_poke_inv room.pokeML command)
        then
        (match (sel_pokeML room.pokeML command) with
             | Some x -> (print_newline(); print_string x.name;)
             | None -> (print_string "\nThat is not a valid pokeML!";()))
        else if (in_inv room.items command)
        then
        (match (sel_item room.items command) with
             | Some x -> (print_newline(); print_string Items.(x.name);)
             | _ -> (print_string "\nThat is not a valid item!";()))
        else (print_string "\nCannot examine this."; ()))
  else if (String.lowercase str)="look"
       then ((print_newline(); print_string (room_description room));())
  else if (String.lowercase str)="quit"
       then exit 0
  else if (String.lowercase str)="stats"
       then (print_string "\nPlayer's hunger: ";
             print_int !(player.hunger);
             print_string "\nPlayer's thirst: ";
             print_int !(player.thirst);
             print_string "\nPlayer's level: ";
             print_int !(player.level);
             print_string "\nPlayer's total experience: ";
             print_int !(player.xp);())
  else if (String.lowercase str)="help"
       then (print_string "\nThis is the help menu. Here are the possible
commands: eat _, drop _, pick up _, use _, move _, battle _, look, stats, quit,
help, inventory, and pokeml. Possible directions to move in include forwards,
backwards, right, or left. In order to eat or drop something, it must be in your
inventory. In order to pick up something, it must be in the room. The use
command allows you to select two items and combine them for special effects! The
battle command must be used on a pokeML in the room, and you will try to take it
down in order to gain experience and levels. The stats command allows you to
view your current statistics such as level, total experience, hunger, and thirst.
Inventory allows you to view your inventory, and pokeML allows you to view your
current pokeML. Look restates the description of the room, and quit ends the
game. In order to beat the game, you must reach level 5, which can be
accomplished by doing various tasks. Every command you enter will slowly
decrease your hunger and thirst levels - should they reach 0, you will die and
lose the game. Good luck!"; print_newline ())

  else if (String.lowercase str)="inventory"
       then (print_newline(); print_string (get_items player.inventory "");())
  else if (String.lowercase str)="pokeML"
       then (print_newline();print_string (get_pokeML player.pokeML "");())
  else (print_string "\nThat is not a valid command!";())



(*items*)
let wood = {name = "wood";
description = "A piece of wood.  You could probably make a fire with this.";
quantity = ref 0; consumable = None}

let flint = {name = "flint";
description = "A flint for starting a fire.";
quantity = ref 0; consumable = None}

let campfire = {name = "campfire";
description = "It's hot!"; quantity = ref 0; consumable = None}

let rawMeat = {name = "raw meat";
description = "It's not especially appetizing, but probably won't kill you.";
quantity = ref 0; consumable = Some RawMeat}

let cookedMeat = {name = "cooked meat";
description = "At this rate you'll get your cooking skillcape in no time.";
quantity = ref 0; consumable = Some CookedMeat}

let burntMeat = {name = "burnt meat";
description = "Well well done.";
quantity = ref 0; consumable = Some BurntMeat}

let salad = {name = "salad";
description = "Wish I had some ranch.";
quantity = ref 0; consumable = Some Salad}

let coconut = {name = "coconut";
description = "Not as good without the chocolate and almond.";
quantity = ref 0; consumable = Some Coconut}

let stirFry = {name = "stir fry";
description = "Jk it's actually just hot salad.";
quantity = ref 0; consumable = Some StirFry}

let water = {name = "water";
description = "Definitely palatable, but it's not Perrier.";
quantity = ref 0; consumable = Some Water}

let cleanWater = {name = "clean water";
description = "https://www.youtube.com/watch?v=rg3Mr6e1KMo";
quantity = ref 0; consumable = Some CleanWater}

(*pokeML*)
let fire1_attributes = {level = ref 1; experience = ref 0;
hp = ref (100,50); c_hp = ref 100; att = ref (2,2); def = ref (50,10)}

let fire1_move1 = {name1 = "claw";
description1 = "Swipe the enemy with your claw.";
accuracy = 70; damage = 30}

let fire1_move2 = {name1 = "fire breath";
description1 = "Burninate your enemy.";
accuracy = 50; damage = 40}

let fire1_moves = [fire1_move1; fire1_move2]

let fire1 = {name = "Voldrag"; description = "A fire type pokeML";
moves = fire1_moves; element = "fire"; attributes = fire1_attributes;
quantity = ref 0; special = "fire"}

let fire2_attributes = {level = ref 1; experience = ref 0;
hp = ref (150,100); c_hp = ref 150; att = ref (2,1); def = ref (50,20)}

let fire2_move1 = {name1 = "fireball";
description1 = "Shoot a fireball at your enemy.";
accuracy = 60; damage = 30}

let fire2_move2 = {name1 = "firestream";
description1 = "Shoot a stream of fire at your enemy.";
accuracy = 30; damage = 60}

let fire2_moves = [fire2_move1; fire2_move2]

let fire2 = {name = "Hotstuff"; description = "A fire type pokeML";
moves = fire2_moves; element = "fire"; attributes = fire2_attributes;
quantity = ref 0; special = "fire"}

let fire3_attributes = {level = ref 1; experience = ref 0;
hp = ref (100,75); c_hp = ref 100; att = ref (3,1); def = ref (40,20)}

let fire3_move1 = {name1 = "punch";
description1 = "Punch your enemy.";
accuracy = 80; damage = 30}

let fire3_move2 = {name1 = "kick";
description1 = "Kick your enemy.";
accuracy = 60; damage = 50}

let fire3_moves = [fire3_move1; fire3_move2]

let fire3 = {name = "Mixtape"; description = "A fire type pokeML";
moves = fire3_moves; element = "fire"; attributes = fire3_attributes;
quantity = ref 0; special = "fire"}

let water1_attributes = {level = ref 1; experience = ref 0;
hp = ref (100,50); c_hp = ref 100; att = ref (2,2); def = ref (50,10)}

let water1_move1 = {name1 = "tail";
description1 = "Hit the enemy with your tail.";
accuracy = 70; damage = 30}

let water1_move2 = {name1 = "water spout";
description1 = "Blast your enemy with some water.";
accuracy = 50; damage = 40}

let water1_moves = [water1_move1; water1_move2]

let water1 = {name = "Seadrag"; description = "A water type pokeML";
moves = water1_moves; element = "water"; attributes = water1_attributes;
quantity = ref 0; special = "water"}

let water2_attributes = {level = ref 1; experience = ref 0;
hp = ref (150,100); c_hp = ref 150; att = ref (2,1); def = ref (50,20)}

let water2_move1 = {name1 = "water shot";
description1 = "Smack your enemy with a shot of water.";
accuracy = 60; damage = 30}

let water2_move2 = {name1 = "water stream";
description1 = "Drown your enemy in a stream of water.";
accuracy = 30; damage = 60}

let water2_moves = [water2_move1; water2_move2]

let water2 = {name = "Snoop Drizzle"; description = "A water type pokeML";
moves = water2_moves; element = "water"; attributes = water2_attributes;
quantity = ref 0; special = "water"}

let water3_attributes = {level = ref 1; experience = ref 0;
hp = ref (100,75); c_hp = ref 100; att = ref (3,1); def = ref (40,20)}

let water3_move1 = {name1 = "punch";
description1 = "Punch your enemy.";
accuracy = 80; damage = 30}

let water3_move2 = {name1 = "kick";
description1 = "Kick your enemy.";
accuracy = 60; damage = 50}


let water3_moves = [water3_move1; water3_move2]

let water3 = {name = "The Mussel"; description = "A water type pokeML";
moves = water3_moves; element = "water"; attributes = water3_attributes;
quantity = ref 0; special = "water"}

let initial_pokeML_list = [fire1; fire2; fire3; water1; water2; water3]

let initial_item_list = [wood; flint; campfire; rawMeat; cookedMeat; burntMeat;
salad; coconut; stirFry; water; cleanWater]

let input_list (input : string) : string list =
  let has_space = String.contains input ' ' in
  if has_space then
    let space_ind = String.index input ' ' in
    let sublen = (String.length input - space_ind) in
    [String.sub input 0 space_ind; String.sub input (space_ind+1) (sublen-1)]
  else [input]

let gen_room_items item_lst p_lst =
  let ilst = ref [] in
  let plst = ref [] in
  let x = Random.int (List.length item_lst) in
  let y = Random.int (List.length p_lst) in
  for i = 0 to 10 do
    let itms = List.nth item_lst ((i*x) mod (List.length item_lst))  in
    let pkmls = List.nth p_lst ((i*y) mod (List.length p_lst))  in
    ilst := List.append !ilst [itms];
    plst := List.append !plst [pkmls];
  done;
  {items= !ilst; pokeML= !plst}


  let rec main player room =
  let input = read_line() in
  (player.hunger:=!(player.hunger) - 1);
  (player.thirst:=!(player.thirst) - 1);
  (parse player input room);
  if ((!(player.hunger) <= 0) || (!(player.thirst) <= 0))
  then (print_string "\nOh no, you do
not have enough energy to continue, you died of hunger and thirst :'("; exit 0)
else if (!(player.level)>=5)
  then (print_string "\nCONGRATULATIONS!! You have become
the PokeML Champion! You Win!!!"; exit 0)
else main player room

(*
let player = {hunger= ref 100;
  thirst= ref 100;
  xp= ref 0;
  level= ref 1;
  inventory= initial_item_list;
  pokeML=
  } in
*)