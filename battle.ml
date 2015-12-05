open PAttributes
open PokeML

type result = | Flee | Switch | Dead of pokeML

(*checks to see whether health is 0 *)
let dead pokeML =
  (!(pokeML.attributes.c_hp)<0)

let attack poke1 move poke2 =
  if ((Random.int 100)<move.accuracy)
  then (let damage = ((move.damage * (fst !(poke1.attributes.att))) -
                    (fst !(poke2.attributes.def))) in
          poke2.attributes.c_hp:=!(poke2.attributes.c_hp) -damage;())
  else ()

(*two pokeML fight each other*)
let rec battle poke1 poke2 =
  print_string "What would you like to do?";
  (*Choices are attack, switch, flee, player input is x*)
  match "attack" with
  | "attack" -> (print_string "Which move would you like to select?";
                  (*Prints list of names of the poke1.moves*)
                  (*Player input is y*)
                  let move1 = (find_move poke1.moves "") in
                  let move2 = (find_ran_move poke2.moves) in
                  attack poke1 move1 poke2;
                  if (dead poke2) then Dead poke2
                  else ((attack poke2 move2 poke1);
                        if (dead poke1) then Dead poke1
                        else battle poke1 poke2))
  | "switch" -> Switch
  | "flee" -> Flee
  | _ -> Flee

(*Your list of pokeML fight a wild pokeML*)
let rec fight lst pokeML =
  (*Print list of your pokeML, lst1*)
  print_string "Which pokeML would you like to use?";
  (*Have player input a string of pokeML name, x*)
  (*Print = poke1.name versus poke2.name*)
  match (battle (sel_pokeML lst "x") pokeML) with
  | Flee -> false
  | Switch -> fight lst pokeML
  | Dead x -> if (x=pokeML) then true
              else (rem_pokeML lst x;
                    let new_lst = (current_pokeML lst) in
                    if (new_lst = []) then false else fight new_lst pokeML)

(*Sets all the current HP to max HP*)
let rec refreshHP lst =
  match lst with
  | h::t -> (h.attributes.c_hp := (fst !(h.attributes.hp))); refreshHP t
  | _ -> ()

(*has the methods that modify the attributes according to the specific command
parsed *)
let main player opp =
  (refreshHP (opp::player.pokeML));
  if (fight player.pokeML opp)
  then ((gain_exp player 100);
         (gain_xp_lst player.pokeML 50); ())
  else ((gain_exp player (-50)); ())

