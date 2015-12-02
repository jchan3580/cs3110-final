(*The items used by the player*)
type item

(*Turn one item into food*)
val cook : item -> item

(*Combine items to make a more advanced item*)
val combine : item -> item -> item

