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
        | None -> print_string "That is not a valid item!";())
  else if (String.length str >= 5 &&
      String.sub (String.lowercase str) 0 5 = "drop ") then
    (let command = String.sub (String.lowercase str) 5
                    ((String.length (String.lowercase str))-5) in
        match (sel_item player.inventory command) with
        | Some x -> drop_item player x
        | None -> print_string "That is not a valid item!";())
  else if (String.length str >= 8 &&
      String.sub (String.lowercase str) 0 8 = "pick up ") then
    (let command = String.sub (String.lowercase str) 8
                    ((String.length (String.lowercase str))-8) in
        match (sel_item room.items command) with
        | Some x -> pickup_item player x
        | None -> print_string "That is not a valid item!";())
  else if (String.length str >= 4 &&
      String.sub (String.lowercase str) 0 4 = "use ") then
    (let command = String.sub (String.lowercase str) 4
                    ((String.length (String.lowercase str))-4) in
        print_string "What would you like to use this on?";
        let input = read_line () in
        if (in_poke_inv player.pokeML command)
        then
        (match ((sel_pokeML player.pokeML command),
                      (sel_item player.inventory input)) with
             | Some x, Some y -> use_pokeML player x y
             | None, Some x -> (print_string "That is not a valid pokeML!";())
             | _ -> (print_string "That is not a valid item!";()))
        else
        (match ((sel_item player.inventory command),
                      (sel_item player.inventory input)) with
             | Some x, Some y -> use_item player x y
             | _ -> (print_string "Those are not valid items!";())))
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
        | None -> (print_string "That is not a valid pokeML!";()))
  else if (String.lowercase str)="look"
       then ((print_string (room_description room));())
  else if (String.lowercase str)="quit"
       then (*Quit the game*)()
  else if (String.lowercase str)="help"
       then (*Print help*)()
  else if (String.lowercase str)="inventory"
       then (*Print inventory*)()
  else (print_string "That is not a valid command!";())




let wood = {name = "wood";
description = "A piece of wood.  You could probably make a fire with this.";
quantity = ref 0; consumable = None}

let flint = {name = "flint";
description = "A flint for starting a fire.";
quantity = ref 0; consumable = None}

let campfire = {name = "campfire";
description = "It's hot!"; quantity = ref 0; consumable = None}

let RawMeat = {name = "raw meat";
description = "It's not especially appetizing, but probably won't kill you.";
quantity = ref 0; consumable = Some RawMeat}

let CookedMeat = {name = "cooked meat";
description = "At this rate you'll get your cooking skillcape in no time.";
quantity = ref 0; consumable = Some CookedMeat}

let BurntMeat = {name = "burnt meat";
description = "Well well done.";
quantity = ref 0; consumable = Some BurntMeat}

let Salad = {name = "salad";
description = "Wish I had some ranch.";
quantity = ref 0; consumable = Some Salad}

let Coconut = {name = "coconut";
description = "Not as good without the chocolate and almond.";
quantity = ref 0; consumable = Some Coconut}

let StirFry = {name = "stir fry";
description = "Jk it's actually just hot salad.";
quantity = ref 0; consumable = Some StirFry}

let Water = {name = "water";
description = "Definitely palatable, but it's not Perrier.";
quantity = ref 0; consumable = Some Water}

let CleanWater = {name = "clean water";
description = "https://www.youtube.com/watch?v=rg3Mr6e1KMo";
quantity = ref 0; consumable = Some CleanWater}

let initial_item_list = [wood; flint; campfire; RawMeat; CookedMeat; BurntMeat;
Salad; Coconut; StirFry; Water; CleanWater]







let main player = failwith "TODO"