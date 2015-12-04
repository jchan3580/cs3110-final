type food = | Raw Meat | Cooked Meat | Burnt Meat | Salad | Coconut | Stir Fry

type item = {name: string; description: string; quantity: int ref;
             consumable: food option}

let cook item =
  match item.name with
  | "raw meat" -> print_string "Successfully cooked a piece of meat!";
      {name = "cooked meat";
      description = "This piece of meat looks delicious and edible!";
      quantity = 1;
      consumable = Some Cooked Meat;}
  | "cooked meat" -> print_string "Unfortunately, you burned a piece of meat";
      {name = "burnt meat";
      description = "This piece of meat is slightly overcooked";
      quantity = 1;
      consumable =  Some Burnt Meat;}
  | "salad" -> print_string "You made stir fry";
      {name = "stir fry";
      description = "A delicious medly of cooked veggies";
      quantity = 1;
      consumable =  Some Stir Fry;}
  | _ -> print_string "This item can\'t be cooked!"; item

let combine item1 item2 =
  match item1.name, item2.name with
  | "flint", "wood" | "wood", "flint" ->
            print_string "You have made a campfire!";
            (*PLACE A FIRE ON THE GROUND SOMEHOW*)
            {name = "campfire";
             description ="Ouch it's hot, you can probably cook things with it";
             quantity = 1;
             consumable = None;}
  | _, _ -> print_string "These items cannot be combined!";
            {name = "null";
             description = "broken";
             quantity = 0;
             consumable = None;}