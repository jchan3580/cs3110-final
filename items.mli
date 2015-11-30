(*The items used by the player*)
type 'a item
type 'a resource
type 'a food

(*Turn one item into food*)
val cook : 'a item -> 'a food

(*Combine items to make a more advanced item*)
val craft : 'a item -> 'b item -> 'c item

(*Turn resources into items*)
val turn : 'a resource -> 'a item

