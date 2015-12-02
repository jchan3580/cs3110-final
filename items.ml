type item = {name:string;
             description:string;
             quantity:int;
             consumable:bool;
             hunger:float;
             thirst:float;}

let cook item =
  match item.name with
  | "raw meat" -> print_string "Successfully cooked a piece of meat!";
                  {name = "cooked meat"; description =
                  "This piece of meat looks delicious and edible!";
                  quantity = 1;
                  consumable = true;
                  hunger = 30.0;
                  thirst = 5.0;}
  | "cooked meat" -> print_string "Unfortunately, you burned a piece of meat";
                  {name = "burnt meat"; description =
                  "This piece of meat is slightly overcooked";
                  quantity = 1;
                  consumable = true;
                  hunger = 5.0;
                  thirst = 0.0;}
  | _ -> print_string "This food is not cookable!"; item

let combine item1 item2 =
  match item1.name, item2.name with
  | "flint", "wood" | "wood", "flint" ->
            print_string "You have made a fire!";
            (*PLACE A FIRE ON THE GROUND SOMEHOW*)
            {name = "null";
             description = "broken";
             quantity = 0;
             consumable = true;
             hunger = 0.0;
             thirst = 0.0;}
  | _, _ -> print_string "These items cannot be combined!";
            {name = "null";
             description = "broken";
             quantity = 0;
             consumable = true;
             hunger = 0.0;
             thirst = 0.0;}