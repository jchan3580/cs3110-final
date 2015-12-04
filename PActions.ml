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
  if ((in_inv (player.inventory) (i.name))=true) then
  (player.hunger := (!(player.hunger) + (consume_hunger_helper i));
      player.thirst := (!(player.thirst) + (consume_thirst_helper i));
      remove_item player.inventory i.name;
      ())
  else (print_string "You do not have that item!"; ())

(*uses an item on an pokeML*)
let use_item player pokeML = failwith "TODO"

(*Drops an item*)
let drop_item player item =
  if ((in_inv (player.inventory) (item.name))=true) then
  ((*SOMEHOW DROP THE ITEM ON THE GROUND*)
      remove_item player.inventory item.name;
    ())
  else (print_string "You do not have that item!"; ())

(*Pickup an item*)
let pickup_item player item =
  if (*CHECK THAT ITEM IS ON THE GROUND*) true then
  ((*REMOVE ITEM FROM GROUND*)
      add_item player.inventory item.name;
      ())
  else (print_string "That item is not on the ground!"; ())

(*initiates a battle with another player or pokeML*)
let battle : player -> player -> unit = failwith "TODO"

(*Moves the player*)
let move : player -> string -> unit= failwith "TODO"
