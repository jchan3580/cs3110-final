open PAttributes
open PokeML
open Battle
open Items

(*Takes an item and computes its hunger boost*)
let consume_hunger_helper item =
  match (get_consume item) with
  |None -> 0
  |Some x ->
  match x with
  |Raw Meat -> 0
  |Cooked Meat -> 50
  |Burnt Meat -> 5
  |Salad -> 20
  |Coconut -> 20
  |Stir Fry -> 30
  |Water -> 0
  |Clean Water -> 5

(*Takes an item and computes its thirst boost*)
let consume_thirst_helper item =
  match item.consumable with
  |None -> 0
  |Some x ->
  match x with
  |Raw Meat -> 0
  |Cooked Meat -> 0
  |Burnt Meat -> 0
  |Salad -> 5
  |Coconut -> 10
  |Stir Fry -> 0
  |Water -> 40
  |Clean Water -> 70


(*consumes an item to increase the player's hunger bar*)
let consume (player : player) (i : item) : player =
  if ((in_inv (player.inventory) (i.name))=true) then
  (hunger := (!player.hunger + (consume_hunger_helper i));
    thirst := (!player.thirst + (consume_thirst_helper i));
    player)
  else print_string "You do not have that item!"; player

(*uses an item on an pokeML*)
let use_item : player -> pokeML = failwith "TODO"

(*initiates a battle with another player or pokeML*)
let battle : player -> player -> unit = failwith "TODO"
