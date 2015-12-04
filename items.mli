(*Items that can be consumed*)
type food = | RawMeat | CookedMeat | BurntMeat
| Salad | Coconut | StirFry | Water | CleanWater

(*The items used by the player*)
type item = {name: string; description: string; quantity: int ref;
             consumable: food option}

(*Turn one item into food*)
val cook : item -> item

(*Combine items to make a more advanced item*)
val combine : item -> item -> item

