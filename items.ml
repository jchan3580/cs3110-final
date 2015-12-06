type food = | RawMeat | CookedMeat | BurntMeat
| Salad | Coconut | StirFry | Water | CleanWater

type item = {name: string; description: string; quantity: int ref;
             consumable: food option}

let cook item =
  match item.name with
  | "raw meat" -> print_string "Successfully cooked a piece of meat!";
      Some "cooked meat"
  | "cooked meat" -> print_string "Unfortunately, you burned a piece of meat";
      Some "burnt meat"
  | "salad" -> print_string "You made stir fry"; Some "stir fry"
  | _ -> print_string "This item can\'t be cooked!"; None

let combine item1 item2 =
  match item1.name, item2.name with
  | "flint", "wood" | "wood", "flint" ->
            print_string "You have made a campfire!";
            Some "campfire"
  | _, _ -> print_string "These items cannot be combined!";
            None