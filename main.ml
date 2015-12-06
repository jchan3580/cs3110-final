open PAttributes
open PActions
open Battle
open Items
open PokeML

type room = {items: item list; pokeML: pokeML list}

let rec get_items (items : item list) (accum : string) =
  match items with
  | [] -> if (String.length accum >= 2)
  then String.sub 0 ((String.length accum) - 2) else accum
  | h::t -> if (!(h.quantity)>0)
  then get_items t (accum ^ (string_of_int !(h.quantity)) ^ (h.name) ^ ", ")
  else get_items t accum

let rec get_pokeML (pokeML : pokeML list) (accum : string) =
  match pokeML with
  | [] -> if (String.length accum >= 2)
  then "Also, you see " ^ (String.sub 0 ((String.length accum) - 2) ^ "!"
  else accum)
  | h::t -> if (!(h.quantity)>0)
  then get_pokeML t (accum ^ (string_of_int !(h.quantity)) ^ (h.name) ^ ", ")
  else get_pokeML t accum

let room_description (room : room) : string =
  "You come to a clearing in the forest.  On the ground you see " ^
  (get_items room.items "") ^ "."

let parse player str =
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
        match (sel_item player.inventory command) with
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
  else if (String.lowercase str)="look"
       then (*Print description of room*)()
  else if (String.lowercase str)="quit"
       then (*Quit the game*)()
  else if (String.lowercase str)="help"
       then (*Print help*)()
  else if (String.lowercase str)="inventory"
       then (*Print inventory*)()
  else (print_string "That is not a valid command!";())

let main player = failwith "TODO"