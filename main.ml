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
  "You come to a clearing in the forest.  On the ground you see " ^
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
lose the game. Good luck!";())
  else if (String.lowercase str)="inventory"
       then (print_newline(); print_string (get_items player.inventory "");())
  else if (String.lowercase str)="pokeML"
       then (print_newline();print_string (get_pokeML player.pokeML "");())
  else (print_string "\nThat is not a valid command!";())




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

let initial_item_list = [wood; flint; campfire; rawMeat; cookedMeat; burntMeat;
salad; coconut; stirFry; water; cleanWater]

let main player room = failwith "TODO"