open PAttributes
open PActions
open Battle
open Items
open PokeML

let parse str =
  if (String.length str >= 4) && String.sub (String.lowercase str) 0 4 = "eat "
  then let item = String.sub (String.lowercase str) 4 ((String.length str)-4) in
      ()(*eat item*)
  else
  if (String.length str >= 5) && String.sub (String.lowercase str) 0 5 = "drop "
  then let item = String.sub (String.lowercase str) 5 ((String.length str)-5) in
      ()(*drop item*)
  else
  if (String.length str >= 8) &&
            String.sub (String.lowercase str) 0 8 = "pick up "
  then let item = String.sub (String.lowercase str) 8 ((String.length str)-8) in
      ()(*pick up item*)
  else
  if (String.length str >= 4) && String.sub (String.lowercase str) 0 4 = "use "
  then let item = String.sub (String.lowercase str) 4 ((String.length str)-4) in
      ()(*use item*)
  else
  if (String.length str >= 5) && String.sub (String.lowercase str) 0 5 = "move "
  then let dir = String.sub (String.lowercase str) 5 ((String.length str)-5) in
      ()(*move*)

(*Calls all the necessary components to start the game*)
let main a s d: 'a player -> items -> 'a battle -> unit = failwith "TODO"

(*repl that interacts with the player*)
let repl a: string -> string = failwith "TODO"