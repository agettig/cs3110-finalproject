open Players
open Tiles
open Cards
open Bank
open Board

type gamestate = {
  current_player : player;
  players : player list;
  tiles : tiles list;
  deck : cards list;
}

let print_payday (num : int) =
  match num with
  | 0 ->
      ANSITerminal.print_string [ ANSITerminal.green ]
        "$$$$$$$$$$$$$$$$$$$                  $$$$$$$$$$$              \
         $$$$$             $$$$$      \
         $$$$$$$$$$$$$$$$$$$                       \
         $$$$$$$$$$$              $$$$$             $$$$$"
  | 1 ->
      ANSITerminal.print_string [ ANSITerminal.green ]
        "$$$$$$$$$$$$$$$$$$$$$               \
         $$$$$$$$$$$$$              $$$$$           $$$$$       \
         $$$$$$$$$$$$$$$$$$$$$                    \
         $$$$$$$$$$$$$              $$$$$           $$$$$ "
  | 2 ->
      ANSITerminal.print_string [ ANSITerminal.green ]
        "$$$$$$$$$$$$$$$$$$$$$$             \
         $$$$$$$$$$$$$$$              $$$$$         $$$$$        \
         $$$$$$$$$$$$$$$$$$$$$$                  \
         $$$$$$$$$$$$$$$              $$$$$         $$$$$  "
  | 3 ->
      ANSITerminal.print_string [ ANSITerminal.green ]
        "$$$$$             $$$$$           $$$$$       \
         $$$$$              $$$$$       $$$$$         $$$$$            \
         $$$$$$                $$$$$       $$$$$              \
         $$$$$       $$$$$   "
  | 4 ->
      ANSITerminal.print_string [ ANSITerminal.green ]
        "$$$$$              $$$$          $$$$$         \
         $$$$$              $$$$$     $$$$$          $$$$$             \
         $$$$$$              $$$$$         $$$$$              \
         $$$$$     $$$$$    "
  | 5 ->
      ANSITerminal.print_string [ ANSITerminal.green ]
        "$$$$$              $$$$         $$$$$           \
         $$$$$              $$$$$   $$$$$           $$$$$              \
         $$$$$             $$$$$           $$$$$              $$$$$   \
         $$$$$     "
  | 6 ->
      ANSITerminal.print_string [ ANSITerminal.green ]
        "$$$$$             $$$$$        \
         $$$$$$$$$$$$$$$$$$$$$$$              $$$$$ $$$$$            \
         $$$$$              $$$$$            \
         $$$$$$$$$$$$$$$$$$$$$$$              $$$$$ $$$$$      "
  | 7 ->
      ANSITerminal.print_string [ ANSITerminal.green ]
        "$$$$$$$$$$$$$$$$$$$$$         \
         $$$$$$$$$$$$$$$$$$$$$$$$$              $$$$$$$$$             \
         $$$$$              $$$$$           \
         $$$$$$$$$$$$$$$$$$$$$$$$$              $$$$$$$$$       "
  | 8 ->
      ANSITerminal.print_string [ ANSITerminal.green ]
        "$$$$$$$$$$$$$$$$$$$$$        \
         $$$$$$$$$$$$$$$$$$$$$$$$$$$              $$$$$$$              \
         $$$$$              $$$$$          \
         $$$$$$$$$$$$$$$$$$$$$$$$$$$              $$$$$$$        "
  | 9 ->
      ANSITerminal.print_string [ ANSITerminal.green ]
        "$$$$$$$$$$$$$$$$$           $$$$$                   \
         $$$$$              $$$$$               $$$$$              \
         $$$$$         $$$$$                   $$$$$              \
         $$$$$         "
  | 10 ->
      ANSITerminal.print_string [ ANSITerminal.green ]
        "$$$$$                      $$$$$                     \
         $$$$$             $$$$$               $$$$$             \
         $$$$$$        $$$$$                     $$$$$             \
         $$$$$       "
  | 11 ->
      ANSITerminal.print_string [ ANSITerminal.green ]
        "$$$$$                     $$$$$                       \
         $$$$$            $$$$$               $$$$$            \
         $$$$$$        $$$$$                       $$$$$            \
         $$$$$       "
  | 12 ->
      ANSITerminal.print_string [ ANSITerminal.green ]
        "$$$$$                    $$$$$                         \
         $$$$$           $$$$$               \
         $$$$$$$$$$$$$$$$$$$$$$        $$$$$                         \
         $$$$$           $$$$$       "
  | 13 ->
      ANSITerminal.print_string [ ANSITerminal.green ]
        "$$$$$                   $$$$$                           \
         $$$$$          $$$$$               \
         $$$$$$$$$$$$$$$$$$$$$        $$$$$                           \
         $$$$$          $$$$$       "
  | 14 ->
      ANSITerminal.print_string [ ANSITerminal.green ]
        "$$$$$                  $$$$$                             \
         $$$$$         $$$$$               $$$$$$$$$$$$$$$$$$$         \
         $$$$$                             $$$$$         $$$$$       \n"
  | _ -> print_endline ""

let print_wedding (num : int) =
  match num with
  | 0 ->
      ANSITerminal.print_string [ ANSITerminal.white ]
        "                         --."
  | 1 ->
      ANSITerminal.print_string [ ANSITerminal.white ]
        "          @@@           /  ))"
  | 2 ->
      ANSITerminal.print_string [ ANSITerminal.white ]
        "         (( }           7_ /"
  | 3 ->
      ANSITerminal.print_string [ ANSITerminal.white ]
        "         ``)             / \\"
  | 4 ->
      ANSITerminal.print_string [ ANSITerminal.white ]
        "        ``( \\           |<| |"
  | 5 ->
      ANSITerminal.print_string [ ANSITerminal.white ]
        "       ```\\`.\\__     __/|/| |"
  | 6 ->
      ANSITerminal.print_string [ ANSITerminal.white ]
        "      ```` )|---~o_)|___| | |"
  | 7 ->
      ANSITerminal.print_string [ ANSITerminal.white ]
        "     ~~~~~/ \\`          | |_|"
  | 8 ->
      ANSITerminal.print_string [ ANSITerminal.white ]
        "         / ' \\`         |__>)"
  | 9 ->
      ANSITerminal.print_string [ ANSITerminal.white ]
        "        /  '  \\`        || |"
  | 10 ->
      ANSITerminal.print_string [ ANSITerminal.white ]
        "       /   '    `       |\\ \\"
  | 11 ->
      ANSITerminal.print_string [ ANSITerminal.white ]
        "      /   _!__.-._`     | \\ \\"
  | 12 ->
      ANSITerminal.print_string [ ANSITerminal.white ]
        "     /_.-'  ( |         | |\\ \\"
  | 13 ->
      ANSITerminal.print_string [ ANSITerminal.white ]
        "        /    `|         |_| \\_\\"
  | 14 ->
      ANSITerminal.print_string [ ANSITerminal.white ]
        "       |`_   |`_      __'_)__.-'"
  | _ -> print_endline ""

let print_house (num : int) =
  match num with
  | 0 -> ANSITerminal.print_string [ ANSITerminal.yellow ] "       _"
  | 1 ->
      ANSITerminal.print_string [ ANSITerminal.yellow ]
        "     _|=|__________"
  | 2 ->
      ANSITerminal.print_string [ ANSITerminal.yellow ]
        "    /              \\"
  | 3 ->
      ANSITerminal.print_string [ ANSITerminal.yellow ]
        "   /                \\"
  | 4 ->
      ANSITerminal.print_string [ ANSITerminal.yellow ]
        "  /__________________\\"
  | 5 ->
      ANSITerminal.print_string [ ANSITerminal.yellow ]
        "   ||  || /--\\ ||  ||"
  | 6 ->
      ANSITerminal.print_string [ ANSITerminal.yellow ]
        "   ||[]|| | .| ||[]||"
  | 7 ->
      ANSITerminal.print_string [ ANSITerminal.yellow ]
        " ()||__||_|__|_||__||()"
  | 8 ->
      ANSITerminal.print_string [ ANSITerminal.yellow ]
        "( )|-|-|-|====|-|-|-|( ) "
  | 9 ->
      ANSITerminal.print_string [ ANSITerminal.yellow ]
        "^^^^^^^^^^====^^^^^^^^^^^"
  | _ -> print_endline ""

let rec print_iter pfun acc cap : unit =
  if acc <= cap then
    let () = pfun acc in
    let () = Unix.sleepf 0.08 in
    let () = print_endline "" in
    print_iter pfun (acc + 1) cap

(** [normalize_text] returns s with the whitespace trimed and in all
    lowercase*)
let normalize_text s = String.(s |> trim |> lowercase_ascii)

(** [current_player gamestate] is the identifier of the player whose
    turn it is in which the in gamestate [st]. *)
let current_player st = st.current_player

(** [final_tile_index] is the last tile on the path of life which
    signifies retirement*)
let final_tile_index = 130

(** [index_in_list_helper player lst c] returns an int that represents
    the index of [player] in list [lst]. Raises: Failswith "Not found"
    if [player] is not in [lst].*)
let rec index_in_list_helper player lst c =
  match lst with
  | [] -> failwith "Not found"
  | h :: t ->
      if h.name = player.name then c
      else index_in_list_helper player t (c + 1)

(** [index_in_list_next] returns the index of the next player after
    [player]. Requires [lst] contains at least two elements.
    Postcondition: [index_in_lst] returns an int between 0 and (list
    length -1)*)
let ( married_index,
      starter_home_index,
      house_index,
      retirement,
      elope,
      college_career,
      career ) =
  (25, 33, 97, 130, 20, 10, 11)

let index_in_list_next (current_player : player) (lst : player list) :
    int =
  let x =
    if
      current_player.index_on_board = married_index
      || current_player.index_on_board = starter_home_index
      || current_player.index_on_board = house_index
      || current_player.index_on_board = college_career
      || current_player.index_on_board = career
    then 0
    else 1
  in
  (index_in_list_helper current_player lst 0 + x) mod List.length lst

(** [next_player current_player players] returns the player whose turn
    is after [current_player]*)
let next_player current_player players =
  List.nth players (index_in_list_next current_player players)

(** [finished player] returns true if the player has reached the end of
    the board and has retired and returns false if player is still
    playing.*)
let finished player = player.index_on_board >= final_tile_index

(** [get_tile] returns the tile in [tiles] at given index [index].
    Raises : Failure if list is too short and Invalid Argument if n is
    negative. *)
let get_tile index tiles : tiles = List.nth tiles index

let rec has_investment (numSpun : int) (cards : cards list) : bool =
  match cards with
  | [] -> false
  | h :: t -> (
      match h with
      | Long_Term_Investment x ->
          if x = numSpun then true else has_investment numSpun t
      | _ -> has_investment numSpun t)

let rec check_investments
    (numSpun : int)
    (players : player list)
    (acc : player list) : player list =
  match players with
  | [] -> acc
  | h :: t ->
      if has_investment numSpun h.deck then
        check_investments numSpun t (acc @ [ add_balance h 5000 ])
      else check_investments numSpun t (acc @ [ h ])

let rec possible_career_choices
    (isCollege : bool)
    (deck : cards list)
    (acc : cards list) =
  match deck with
  | [] -> acc
  | h :: t -> (
      match h with
      | Career x ->
          if x.college_career = isCollege then
            possible_career_choices isCollege t (h :: acc)
          else possible_career_choices isCollege t acc
      | _ -> possible_career_choices isCollege t acc)

let has_house (player : player) =
  player.index_on_board > starter_home_index

let rec possible_house_choices
    (player : player)
    (hasHouse : bool)
    (deck : cards list)
    (acc : cards list) =
  match deck with
  | [] -> acc
  | h :: t -> (
      match h with
      | House y ->
          if
            (not hasHouse)
            && y.price <= player.account_balance
            && y.starter
          then possible_house_choices player false t (h :: acc)
          else if hasHouse && y.price <= player.account_balance then
            possible_house_choices player true t (h :: acc)
          else possible_house_choices player hasHouse t acc
      | _ -> possible_house_choices player hasHouse t acc)

let print_career_card (card : cards) =
  match card with
  | Career x ->
      print_endline
        ("Name: " ^ x.name ^ " " ^ "Salary: " ^ string_of_int x.salary
       ^ " " ^ "Salary Max: "
        ^ string_of_int x.salary_max
        ^ " " ^ "Taxes Due: "
        ^ string_of_int x.taxes_due)
  | _ -> failwith "passed in card that isn't a career"

let get_house_or_career_name (card : cards) =
  match card with
  | Career x -> x.name
  | House x -> x.name
  | _ -> failwith "passed in card that isn't a career"

let rec print_houses houses : unit =
  match houses with
  | [] -> print_endline ""
  | h :: t -> (
      print_string "{";
      match h with
      | House house ->
          print_endline ("Name: " ^ house.name);
          print_endline ("Price: " ^ string_of_int house.price);
          print_endline
            ("Selling Price: " ^ string_of_int house.selling_price ^ "}");
          print_houses t
      | _ -> print_houses t)

(** [string_equal s1 s2] returns true if s1 is equivalent to s2 after
    both strings have been trimmed and changed to all lowercase else
    false is returned*)
let string_equal s1 s2 = normalize_text s1 = normalize_text s2

let rec match_card_by_name (name : string) (cards : cards list) : cards
    =
  match cards with
  | [] -> failwith "no card found by that name"
  | h :: t -> (
      match h with
      | Career x ->
          if name = x.name then h else match_card_by_name name t
      | House x ->
          if name = x.name then h else match_card_by_name name t
      | _ -> match_card_by_name name t)

let choose_career (player : player) (deck : cards list) : cards =
  let possible_careers =
    possible_career_choices player.college deck []
  in
  let first_career =
    List.nth possible_careers
      (Random.int (List.length possible_careers))
  in
  let new_possible =
    remove_from_deck possible_careers first_career []
  in
  let second_career =
    List.nth new_possible (Random.int (List.length new_possible))
  in
  let print_careers () =
    print_career_card first_career;
    print_career_card second_career;
    print_string "Enter Desired Career Name: "
  in
  let career1_name = get_house_or_career_name first_career in
  let career2_name = get_house_or_career_name second_career in
  let rec career_string () =
    print_careers ();
    match read_line () with
    | x ->
        if string_equal x career1_name then career1_name
        else if string_equal x career2_name then career2_name
        else (
          print_endline "\nInvalid input";
          career_string ())
  in

  match_card_by_name (career_string ()) possible_careers

let rec bought_house
    (player : player)
    (house_name : string)
    (deck : cards list) =
  match deck with
  | [] -> add_balance player 0
  | h :: t -> (
      match h with
      | House house ->
          if house.name = house_name then
            add_balance (add_card h player) (-house.price)
          else bought_house player house_name t
      | _ -> bought_house player house_name t)

let choose_houses (player : player) (deck : cards list) =
  let possible_houses =
    possible_house_choices player (has_house player) deck []
  in
  if possible_houses <> [] then
    let print_houses () =
      print_houses possible_houses;
      print_string "Enter which house you'd like to buy: "
    in
    let rec house_name () =
      print_houses ();
      match read_line () with
      | chosen_house -> (
          match
            List.find_opt
              (fun a ->
                string_equal chosen_house (get_house_or_career_name a))
              possible_houses
          with
          | Some x ->
              print_iter print_house 0 9;
              get_house_or_career_name x
          | None -> house_name ())
    in
    match_card_by_name (house_name ()) possible_houses
  else match_card_by_name "None" possible_houses

let rec print_players = function
  | [] -> print_endline ""
  | h :: t ->
      Printf.printf "Player: %s\n" h.name;
      print_players t

let rec has_exemption_card (deck : cards list) =
  match deck with
  | [] -> None
  | h :: t -> (
      match h with
      | Exemption_Card -> Some h
      | _ -> has_exemption_card t)

let print_lawsuit_players players plaintiff : unit =
  let lawsuit_players =
    List.filter (fun x -> plaintiff.name <> x.name) players
  in
  print_players lawsuit_players

let rec lawsuit_player players plaintiff =
  print_lawsuit_players players plaintiff;
  print_endline "Enter Player's name who you would like to sue";
  let player_name = read_line () in
  match player_name with
  | player -> (
      match
        List.find_opt
          (fun x ->
            player <> plaintiff.name && string_equal player x.name)
          players
      with
      | None ->
          print_endline "Invalid Input";
          lawsuit_player players plaintiff
      | Some x -> x)

let change_index_board (player : player) : player * int =
  let current_index = player.index_on_board in
  let spinner = spinner () in
  (* player position before adjustment*)
  let player_index_spinner = current_index + spinner in

  let new_index =
    (* makes college player stop at tile 10 to get college career*)
    if player.college && current_index < 10 then
      if player_index_spinner > 10 then 10
      else player_index_spinner
        (* maintains order for college player jumping from tile 10 to
           tile 15 to stay on main path *)
    else if player.college && current_index = 10 then
      4 + player_index_spinner
    else if (not player.college) && current_index = 10 then 11
      (* makes each player stop at the marriage tile *)
    else if current_index < married_index then
      if player_index_spinner > married_index then married_index
      else player_index_spinner
        (* makes each player stop to buy a starter home *)
    else if
      current_index >= married_index
      && current_index < starter_home_index
    then
      if player_index_spinner > starter_home_index then
        starter_home_index
      else player_index_spinner
        (* makes each player stop to buy a house *)
    else if
      current_index >= starter_home_index && current_index < house_index
    then
      if player_index_spinner > house_index then house_index
      else player_index_spinner
        (* makes sure player cannot make illegal moves past final
           tile *)
    else if player_index_spinner > 130 then 130
    else player_index_spinner
  in
  print_endline "";
  print_endline ("Spinner: " ^ string_of_int spinner ^ "\n");
  print_endline
    ("You have moved to tile " ^ string_of_int (new_index + 1) ^ ".");
  print_color_tile new_index make_board;
  print_endline "";
  print_endline "------------------------------------------";
  let () =
    if new_index = married_index then print_iter print_wedding 0 14
  in
  ({ player with index_on_board = new_index }, spinner)

(** [new_players_lst] returns an updated player with the current players
    record updated*)
let rec new_players_list
    (current_lst : player list)
    (updated_player : player list) =
  match updated_player with
  | [] -> current_lst
  | h :: t ->
      new_players_list
        (List.map
           (fun x -> if h.name = x.name then h else x)
           current_lst)
        t

(** [gameover players] returns true if all players in the game have
    retired and returns false if anyone is still playing.*)
let rec gameover players =
  match players with
  | [] -> true
  | h :: t -> if not (finished h) then false else gameover t

(** [player_winner player_lst player] returns the player who has won the
    game with the largest account balance at the end of the game.*)
let rec player_winner player_lst player =
  match player_lst with
  | [] -> player
  | h :: t ->
      if final_balance h > final_balance player then player_winner t h
      else player_winner t player

let winner player = Printf.printf "Winner %s!\n\n" player.name

let rec has_career (deck : cards list) =
  match deck with
  | [] -> None
  | h :: t -> (
      match h with
      | Career _ -> Some h
      | _ -> has_career t)

let rec has_spin_card (deck : cards list) : cards option =
  match deck with
  | [] -> None
  | h :: t -> (
      match h with
      | SpinToWin_Card _ -> Some h
      | _ -> has_spin_card t)

let rec guess_lst (num : int) lst player =
  let rec spin_number player =
    Printf.printf "%s please enter your guess (1-10): " player.name;
    match int_of_string_opt (String.trim (read_line ())) with
    | Some x ->
        if x > 0 && x < 11 && List.mem x lst <> true then x
        else (
          print_endline "\nInvalid input ";
          spin_number player)
    | None ->
        print_endline " \nInvalid input ";
        spin_number player
  in
  match num with
  | 0 -> lst
  | _ -> guess_lst (num - 1) (spin_number player :: lst) player

let player_spintowin (player : player) : player =
  let spin_card = has_spin_card player.deck in
  let player_removed_card =
    match spin_card with
    | None -> player
    | Some x -> remove_card x player
  in

  let num_of_guesses =
    match spin_card with
    | None -> 1
    | Some x -> (
        match x with
        | SpinToWin_Card x -> x
        | _ -> failwith "Illegal")
  in

  let player_guesses =
    guess_lst num_of_guesses [] player_removed_card
  in
  let rec invest_help () =
    print_endline
      "How much would you like to invest? (Input 5000 - 50000): ";
    match int_of_string_opt (String.trim (read_line ())) with
    | Some x ->
        if x > 4999 && x < 50001 then x
        else (
          print_endline "\nInvalid input ";
          invest_help ())
    | None ->
        print_endline " \nInvalid input ";
        invest_help ()
  in
  let invest = invest_help () in

  let spin = spinner () in
  Printf.printf "Spinner Value: %i \n" spin;
  if List.mem spin player_guesses then
    add_balance player_removed_card (10 * invest)
  else player_removed_card

let start_turn () =
  (* Printf.printf ("%s ^ 's Turn /n Please enter any key to start"); *)
  let x = read_line () in
  match x with
  | _ -> print_string ""

let rec get_players lst =
  match lst with
  | [] -> []
  | h :: t ->
      ( pos_to_tuple h.index_on_board,
        " | " ^ String.sub h.name 0 3 ^ " | " )
      :: get_players t

let rec turn gamestate : unit =
  print_players gamestate.players;
  print_endline "";
  Printf.printf "%s's Turn \n \nPlease enter any key to start: "
    gamestate.current_player.name;
  (* makes current player type in anything into terminal to start their
     turn*)
  start_turn ();
  (*print current player information*)
  player_to_string gamestate.current_player;
  if gameover gamestate.players then
    winner
      (player_winner gamestate.players (List.nth gamestate.players 0))
  else if gamestate.current_player.index_on_board = 130 then
    turn
      {
        gamestate with
        current_player =
          next_player gamestate.current_player gamestate.players;
      }
  else
    (*player with new index*)
    let change_ind_tup = change_index_board gamestate.current_player in
    let player_moved = fst change_ind_tup in
    let numSpun = snd change_ind_tup in
    let player_index = player_moved.index_on_board in

    let payraise_player =
      if
        gamestate.current_player.index_on_board < 39
        && 39 <= player_index
      then payraise player_moved
      else if
        gamestate.current_player.index_on_board < 71
        && 71 <= player_index
      then payraise player_moved
      else if
        gamestate.current_player.index_on_board < 99
        && 99 <= player_index
      then payraise player_moved
      else if
        gamestate.current_player.index_on_board < 114
        && 114 <= player_index
      then payraise player_moved
      else player_moved
    in

    let pay_player =
      if
        gamestate.current_player.index_on_board < 12
        && 12 <= player_index
      then
        let () = print_iter print_payday 0 14 in
        payday payraise_player
      else if
        gamestate.current_player.index_on_board < 15
        && 15 <= player_index
      then
        let () = print_iter print_payday 0 14 in
        payday payraise_player
      else if
        gamestate.current_player.index_on_board < 23
        && 23 <= player_index
      then
        let () = print_iter print_payday 0 14 in
        payday payraise_player
      else if
        gamestate.current_player.index_on_board < 32
        && 32 <= player_index
      then
        let () = print_iter print_payday 0 14 in
        payday payraise_player
      else if
        gamestate.current_player.index_on_board < 48
        && 48 <= player_index
      then
        let () = print_iter print_payday 0 14 in
        payday payraise_player
      else if
        gamestate.current_player.index_on_board < 57
        && 57 <= player_index
      then
        let () = print_iter print_payday 0 14 in
        payday payraise_player
      else if
        gamestate.current_player.index_on_board < 64
        && 64 <= player_index
      then
        let () = print_iter print_payday 0 14 in
        payday payraise_player
      else if
        gamestate.current_player.index_on_board < 79
        && 79 <= player_index
      then
        let () = print_iter print_payday 0 14 in
        payday payraise_player
      else if
        gamestate.current_player.index_on_board < 86
        && 86 <= player_index
      then
        let () = print_iter print_payday 0 14 in
        payday payraise_player
      else if
        gamestate.current_player.index_on_board < 92
        && 92 <= player_index
      then
        let () = print_iter print_payday 0 14 in
        payday payraise_player
      else if
        gamestate.current_player.index_on_board < 105
        && 105 <= player_index
      then
        let () = print_iter print_payday 0 14 in
        payday payraise_player
      else if
        gamestate.current_player.index_on_board < 109
        && 109 <= player_index
      then
        let () = print_iter print_payday 0 14 in
        payday payraise_player
      else if
        gamestate.current_player.index_on_board < 120
        && 120 <= player_index
      then
        let () = print_iter print_payday 0 14 in
        payday payraise_player
      else if
        gamestate.current_player.index_on_board < 127
        && 127 <= player_index
      then
        let () = print_iter print_payday 0 14 in
        payday payraise_player
      else payraise_player
    in

    (*tile on which [paid_player] is on*)
    let tile = get_tile pay_player.index_on_board gamestate.tiles in
    print_tiles tile;

    (* [new_player] returns a tuple with an updated player and None if
       the game deck does not need to be altered and Some card if card
       has to be removed from the game deck*)
    let new_player : player list * (cards option * cards option) =
      match tile with
      | PayTile c ->
          ([ add_balance pay_player c.account_change ], (None, None))
      | TaxesTile _ ->
          ( [ add_balance pay_player (-1 * get_taxes pay_player) ],
            (None, None) )
      | LifeTile _ ->
          let rand_lf_tile =
            List.nth life_tiles (Random.int (List.length life_tiles))
          in
          ( [ add_card rand_lf_tile pay_player ],
            (Some rand_lf_tile, None) )
      | CareerTile _ -> (
          let career_chosen = choose_career pay_player gamestate.deck in
          let had_career = has_career pay_player.deck in
          match had_career with
          | None ->
              ( [ add_card career_chosen pay_player ],
                (Some career_chosen, None) )
          | Some h ->
              ( [ exchange_card pay_player career_chosen h ],
                (Some career_chosen, Some h) ))
      | FamilyTile c ->
          if c.index_tile = married_index then
            ([ { pay_player with so = true } ], (None, None))
          else if c.index_tile = elope then
            ( [
                {
                  pay_player with
                  so = true;
                  index_on_board = married_index;
                };
              ],
              (None, None) )
          else
            ( [
                {
                  pay_player with
                  children = pay_player.children + c.children;
                };
              ],
              (None, None) )
      | HouseTile _ -> (
          let chosen_house =
            choose_houses player_moved gamestate.deck
          in
          let house_name =
            match chosen_house with
            | House h -> Some h.name
            | _ -> None
          in
          match house_name with
          | Some x ->
              if x = "None" then
                ( [ bought_house player_moved x gamestate.deck ],
                  (None, None) )
              else
                ( [ bought_house player_moved x gamestate.deck ],
                  (Some chosen_house, None) )
          | None -> ([ player_moved ], (None, None)))
      | TakeTile _ ->
          ([ player_moved ], (None, None)) (* not implemented in ms1*)
      | ActionTile _ -> ([ player_moved ], (None, None))
      | SpinToWinTile _ ->
          let new_players =
            List.map player_spintowin gamestate.players
          in
          (new_players, (None, None))
      | LawsuitTile _ -> (
          let player_sued =
            lawsuit_player gamestate.players pay_player
          in
          let exemption_card = has_exemption_card player_sued.deck in
          match exemption_card with
          | None ->
              Printf.printf "%s's Current Balance: %i \n"
                player_sued.name player_sued.account_balance;
              Printf.printf "%s has sued %s for $100,000 \n"
                pay_player.name player_sued.name;
              let new_balance_player =
                add_balance player_sued ~-100000
              in
              Printf.printf "%s's Current Balance: %i \n"
                new_balance_player.name
                new_balance_player.account_balance;
              ([ pay_player; new_balance_player ], (None, None))
          | Some x ->
              Printf.printf "%s used an Exemption Card!"
                player_sued.name;
              ([ pay_player; remove_card x player_sued ], (None, Some x))
          )
    in
    (*[new_play_list] is the updated player list after the current
      player's turn*)
    let new_play_list =
      new_players_list gamestate.players (fst new_player)
    in
    let new_pos_lst = get_players new_play_list in
    let updated_board = update_board make_board new_pos_lst in
    print_board updated_board;
    (* [new_deck] is the new game deck*)
    let new_deck =
      match snd new_player with
      | None, None -> gamestate.deck
      | Some x, None -> remove_from_deck gamestate.deck x []
      | Some x, Some y -> remove_from_deck (y :: gamestate.deck) x []
      | None, Some y -> y :: gamestate.deck
    in
    (* ANSITerminal.erase Screen; *)
    (* returns new gamestate with updated records*)
    turn
      {
        gamestate with
        current_player = next_player pay_player new_play_list;
        players = check_investments numSpun new_play_list [];
        deck = new_deck;
      }

(** [gameover players] returns true if all players in the game have
    retired and returns false if anyone is still playing.*)
let rec gameover players =
  match players with
  | [] -> true
  | h :: t -> if not (finished h) then false else gameover t

(** [player_winner player_lst player] returns the player who has won the
    game with the largest account balance at the end of the game.*)
let rec player_winner player_lst player =
  match player_lst with
  | [] -> player
  | h :: t ->
      if final_balance h > final_balance player then player_winner t h
      else player_winner t player
