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
  let command = read_line () in
  match command with
  | "attack" -> (print_string "Which move would you like to select?";
                  print_move_lst poke1.moves;
                  let input = read_line () in
                  match (find_move poke1.moves input) with
                  | Some x -> let move2 = (find_ran_move poke2.moves) in
                              attack poke1 x poke2;
                              if (dead poke2) then Dead poke2
                              else ((attack poke2 move2 poke1);
                                    if (dead poke1) then Dead poke1
                                    else battle poke1 poke2)
                  | None -> (print_string "That is not a valid move.";
                             battle poke1 poke2))

  | "switch" -> Switch
  | "flee" -> Flee
  | _ -> (print_string "That is not a valid command, please try again.";
          battle poke1 poke2)

(*Your list of pokeML fight a wild pokeML*)
let rec fight lst pokeML =
  print_poke_lst lst;
  print_string "Which pokeML would you like to use?";
  let poke = read_line () in
  match (sel_pokeML lst poke) with
  | Some x ->print_string x.name;
             print_string " versus ";
             print_string pokeML.name;
            (match (battle x pokeML) with
            | Flee -> false
            | Switch -> fight lst pokeML
            | Dead y -> if (y=pokeML) then true
                else (rem_pokeML lst y;
                      let new_lst = (current_pokeML lst) in
                      if (new_lst = []) then false else fight new_lst pokeML))
  | None -> (print_string "That is not a valid pokeML! Please try again.";
              fight lst pokeML)


(*Sets all the current HP to max HP*)
let rec refreshHP lst =
  match lst with
  | h::t -> (h.attributes.c_hp := (fst !(h.attributes.hp))); refreshHP t
  | _ -> ()

let rec catchPoke inv poke=
  print_string "Would you like to catch this pokeML?";
         let input = read_line () in
        match (String.lowercase input) with
        | "yes" -> add_pokeML inv poke
        | "no" -> ()
        | _ -> (print_string "Invalid command, please try again.";
               catchPoke inv poke)

(*has the methods that modify the attributes according to the specific command
parsed *)
let main player opp =
  (refreshHP (opp::player.pokeML));
  if (fight player.pokeML opp)
  then (catchPoke player.pokeML pokeML.name;
       (gain_exp player 100);
       (gain_xp_lst player.pokeML 50);())
  else ((gain_exp player (-50)); ())

