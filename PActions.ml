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
  |Raw Meat -> 0
  |Cooked Meat -> 50
  |Burnt Meat -> 5
  |Salad -> 20
  |Coconut -> 20
  |Stir Fry -> 30
  |Water -> 0
  |Clean Water -> 5

(*Takes an item and computers its thirst boost*)
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
let consume (item : item) player : player =
  {faction = player.faction;
  hunger = (!player.hunger + consume_hunger_helper item);
  thirst = (!player.thirst + consume_thirst_helper item); xp = player.xp;
  level = player.level; inventory = player.inventory; pokeML = player.pokeML}

(*uses an item on an pokeML*)
let use_item : 'a player -> 'a pokeML

(*initiates a battle with another player or pokeML*)
let battle : 'a player -> 'b player -> unit
