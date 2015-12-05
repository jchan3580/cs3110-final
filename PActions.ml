open PAttributes
open PokeML
open Battle
open Items

(*Takes an item and computes its hunger boost*)
let consume_hunger_helper item =
  match item.consumable with
  |None -> 0
  |Some x ->
  match x with
  |RawMeat -> 0
  |CookedMeat -> 50
  |BurntMeat -> 5
  |Salad -> 20
  |Coconut -> 20
  |StirFry -> 30
  |Water -> 0
  |CleanWater -> 5

(*Takes an item and computes its thirst boost*)
let consume_thirst_helper item =
  match item.consumable with
  |None -> 0
  |Some x ->
  match x with
  |RawMeat -> 0
  |CookedMeat -> 0
  |BurntMeat -> 0
  |Salad -> 5
  |Coconut -> 10
  |StirFry -> 0
  |Water -> 40
  |CleanWater -> 70


(*consumes an item to increase the player's hunger bar*)
let consume (player : player) (i : item) : unit =
  if ((in_inv (player.inventory) (i.name))=true)&& (i.consumable <> None)
  then (change_hunger player (consume_hunger_helper i);
        change_thirst player (consume_thirst_helper i);
        remove_item player.inventory i.name;
        gain_exp player 5;())
  else (print_string "You do not have that item!"; ())

(*uses two items together*)
let use_item player item1 item2 =
  if ((in_inv (player.inventory) (item1.name))=true&&
    ((in_inv (player.inventory) (item2.name))=true))
  then (match (combine item1 item2) with
    | Some x -> (remove_item player.inventory item1.name;
                remove_item player.inventory item2.name;
                add_item player.inventory x;
                gain_exp player 5; ())
    | None -> ())
  else (print_string "You do not have those items!"; ())

(*uses an item on an pokeML*)
let use_pokeML player pokeML item =
  if ((in_inv (player.inventory) (item.name))=true)
  then (if((in_poke_inv (player.pokeML) (PokeML.(pokeML.name)))=true)
    then (match (use_ability pokeML item) with
      | Some x -> (remove_item player.inventory item.name;
                   add_item player.inventory x;
                   gain_exp player 10; ())
      | None -> ())
    else (print_string "You do not have that pokeML!"; ()))
  else (print_string "You do not have that item!"; ())

(*Drops an item*)
let drop_item player item =
  if ((in_inv (player.inventory) (item.name))=true) then
  ((*SOMEHOW DROP THE ITEM ON THE GROUND*)
      remove_item player.inventory item.name;
      gain_exp player 1; ())
  else (print_string "You do not have that item!"; ())

(*Pickup an item*)
let pickup_item player item =
  if (*CHECK THAT ITEM IS ON THE GROUND*) true then
  ((*REMOVE ITEM FROM GROUND*)
      add_item player.inventory item.name;
      gain_exp player 1; ())
  else (print_string "That item is not on the ground!"; ())

(*initiates a battle with another player or pokeML*)
let battle player pokeML=
  main player pokeML

(*Moves the player*)
let move : player -> string -> unit= failwith "TODO"
