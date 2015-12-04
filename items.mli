(*Items that can be consumed*)
type food

(*The items used by the player*)
type item

(*get consumable items*)
val get_consume : item -> food option

(*Turn one item into food*)
val cook : item -> item

(*Combine items to make a more advanced item*)
val combine : item -> item -> item

