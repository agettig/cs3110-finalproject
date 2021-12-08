open Players
open Tiles
open Cards
open Bank
open Board
open Printing
open IntHashtbl

type gamestate = {
  current_player : player;
  players : player list;
  tiles : tiles list;
  deck : cards list;
  graphics : bool;
}

let rec print_iter pfun acc cap graphics : unit =
  if graphics then (
    if acc <= cap then
      let () = pfun acc in
      let () = Unix.sleepf 0.06 in
      let () = print_endline "" in
      print_iter pfun (acc + 1) cap graphics)
  else print_endline ""

let normalize_text s = String.(s |> trim |> lowercase_ascii)

let rand i = Random.int i

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

(* indexes of stop tiles *)
let ( married_index,
      starter_home_index,
      house_index,
      elope,
      college_career,
      career ) =
  (25, 33, 97, 20, 10, 11)

(** [index_in_list_next] returns the index of the next player after
    [player]. Requires [lst] contains at least two elements.
    Postcondition: [index_in_lst] returns an int between 0 and (list
    length -1)*)
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

(** [next_player current_player players] returns player whose turn is
    after [current_player] in [players] *)
let next_player current_player players =
  List.nth players (index_in_list_next current_player players)

(** [finished player] returns true if the index of [player] is greater
    than or equal to the final tile index*)
let finished player = player.index_on_board >= final_tile_index

(** [get_tile i tiles] returns the tile with index [i]. Raises Failure
    if [tiles] is shorter than [i]. Raises Invalid Argument if [i] < 0 *)
let get_tile i = IntHashtbl.find int_tile i

(** [has_investment numSpun cards] returns true if [numSpun] matches a
    long term investment card within [cards], else false*)
let rec has_investment (numSpun : int) (cards : cards list) : bool =
  match cards with
  | [] -> false
  | h :: t -> (
      match h with
      | Long_Term_Investment x ->
          if x = numSpun then true else has_investment numSpun t
      | _ -> has_investment numSpun t)

(** [check_investments numSpun players acc] returns [players] with
    updated balances based on long term investment cards and [numSpun]*)
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

(** [possible_career_choices isCollege deck acc] returns [acc] with all
    remaining careers in [deck] that match [isCollege]*)
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

(** [share_cards_in_deck acc deck] returns [acc] with all SpinToWin and
    Expection cards in [deck] added*)
let rec share_cards_in_deck (acc : cards list) (deck : cards list) =
  match deck with
  | [] -> acc
  | h :: t -> (
      match h with
      | SpinToWin_Card _
      | Exemption_Card ->
          share_cards_in_deck (h :: acc) t
      | _ -> share_cards_in_deck acc t)

(** [random_share_card deck] returns a random card from [deck]*)
let random_share_card deck : cards option =
  let share_cards = share_cards_in_deck deck [] in
  List.nth_opt share_cards (rand (List.length share_cards))

(** [has_house players] returns a true if a player has a house, else
    false*)
let has_house (player : player) =
  player.index_on_board > starter_home_index

(** [possible_house_choices player hasHouse deck acc] returns [acc] with
    all house choices the [player] can purchase*)
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

(** [print_career_card card] prints out the information of a career
    card. Requires [card] is a career card. *)
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

(** [get_house_or_career_name card] returns a string containing the name
    of the card. Requires [card] is a career or house card. *)
let get_house_or_career_name (card : cards) =
  match card with
  | Career x -> x.name
  | House x -> x.name
  | _ -> failwith "passed in card that isn't a career"

(** [print_houses houses] prints out the information of all house cards
    in [houses] *)
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

(** [match_card_by_name name cards] returns a career or house card in
    [cards] that has the same name as [name]. Requires that a card in
    [cards] has name [name]. *)
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

(** [choose_career player deck] returns the career card that [player]
    chooses out of two choices *)
let choose_career (player : player) (deck : cards list) : cards =
  let possible_careers =
    possible_career_choices player.college deck []
  in
  let first_career =
    List.nth possible_careers (rand (List.length possible_careers))
  in
  let new_possible =
    remove_from_deck possible_careers first_career []
  in
  let second_career =
    List.nth new_possible (rand (List.length new_possible))
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

(** [bought_house player house_name deck] returns [player] with the
    house of [house_name] added to their deck and house price removed
    from their balance *)
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

(** [choose_houses player deck] returns the house card that [player]
    chooses out of all possible choices *)
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
          | Some x -> get_house_or_career_name x
          | None -> house_name ())
    in
    match_card_by_name (house_name ()) possible_houses
  else match_card_by_name "None" possible_houses

(** [print_players] prints out all the names of all players in the
    players list passed in *)
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

(** [print_lawsuit_players players planintiff] prints out the names of
    all players in [players] that does not share the same name as
    [plaintiff] *)
let print_lawsuit_players players plaintiff : unit =
  let lawsuit_players =
    List.filter (fun x -> plaintiff.name <> x.name) players
  in
  print_players lawsuit_players

(** [lawsuit_player players planintiff] returns a player who the
    plaintiff chooses to sue *)
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

(** [change_index_board player] returns a tuple of [player] with a new
    index and the number they spun *)
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
  print_color_tile new_index make_board player.colorblind;
  print_endline "";
  print_endline "------------------------------------------";
  ({ player with index_on_board = new_index }, spinner)

(** [new_players_lst] returns an updated players list by swapping
    [updated_player] players with players of the same name in
    [current_lst] to update all players after a turn *)
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

(** [gameover players] returns true if all players have reached the
    final tile else returns false *)
let rec gameover players =
  match players with
  | [] -> true
  | h :: t -> if not (finished h) then false else gameover t

(** [player_winner players_lst player] returns the player with the
    highest final balance *)
let rec player_winner player_lst player =
  match player_lst with
  | [] -> player
  | h :: t ->
      if final_balance h > final_balance player then player_winner t h
      else player_winner t player

(** [winner player] prints the name of [player] as the winner and their
    final balance *)
let winner player =
  Printf.printf
    "%s has won The Game of Life with a final balance of %i!\n \n"
    player.name (final_balance player)

(** [has_career deck] returns Some career card if there is a career card
    in [deck], else None*)
let rec has_career (deck : cards list) =
  match deck with
  | [] -> None
  | h :: t -> (
      match h with
      | Career _ -> Some h
      | _ -> has_career t)

(** [has_career deck] returns Some SpinToWin card if there is a
    SpinToWin card in [deck], else None*)
let rec has_spin_card (deck : cards list) : cards option =
  match deck with
  | [] -> None
  | h :: t -> (
      match h with
      | SpinToWin_Card _ -> Some h
      | _ -> has_spin_card t)

(** [guess_lst num lst player] returns a list of integer guesses from
    each player*)
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

let num_of_guesses_helper card_opt =
  match card_opt with
  | None -> 1
  | Some x -> (
      match x with
      | SpinToWin_Card x -> x
      | _ -> failwith "Illegal")

(** [player_spintowin player] returns [player] with money added or
    subtract to balance depending on how much they invested and if they
    won the spin to win. *)
let player_spintowin (player : player) : player =
  let spin_card = has_spin_card player.deck in
  let player_removed_card =
    match spin_card with
    | None -> player
    | Some x -> remove_card x player
  in

  let num_of_guesses = num_of_guesses_helper spin_card in

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

(** [start_turn] waits for any user input to begin the next turn *)
let start_turn () =
  (* Printf.printf ("%s ^ 's Turn /n Please enter any key to start"); *)
  let x = read_line () in
  match x with
  | _ -> print_string ""

(** [get_players lst] returns a list of (IntTilesTupl.t * string) to
    represent the indexes of players in [list] on the board and a string
    to represent them on the board *)
let rec get_players lst =
  match lst with
  | [] -> []
  | h :: t ->
      let h_new_name = h.name ^ "   " in
      ( pos_to_tuple h.index_on_board,
        " | " ^ String.sub h_new_name 0 3 ^ " | " )
      :: get_players t

(** [payraise current_player player_moved player_index] increases the
    pay raise of the [player_moved] by $10,000 if a payraise tile is
    passed *)
let payraise current_player player_moved player_index =
  if current_player.index_on_board < 39 && 39 <= player_index then
    payraise player_moved
  else if current_player.index_on_board < 71 && 71 <= player_index then
    payraise player_moved
  else if current_player.index_on_board < 99 && 99 <= player_index then
    payraise player_moved
  else if current_player.index_on_board < 114 && 114 <= player_index
  then payraise player_moved
  else player_moved

(** [pay current_player payraise_player player_index] increases the
    account balance of [payraise_player] by their salary + payraises if
    a pay tile is passed *)
let pay gamestate payraise_player player_index =
  if gamestate.current_player.index_on_board < 12 && 12 <= player_index
  then
    let () = print_iter print_payday 0 14 gamestate.graphics in
    payday payraise_player
  else if
    gamestate.current_player.index_on_board < 15 && 15 <= player_index
  then
    let () = print_iter print_payday 0 14 gamestate.graphics in
    payday payraise_player
  else if
    gamestate.current_player.index_on_board < 23 && 23 <= player_index
  then
    let () = print_iter print_payday 0 14 gamestate.graphics in
    payday payraise_player
  else if
    gamestate.current_player.index_on_board < 32 && 32 <= player_index
  then
    let () = print_iter print_payday 0 14 gamestate.graphics in
    payday payraise_player
  else if
    gamestate.current_player.index_on_board < 48 && 48 <= player_index
  then
    let () = print_iter print_payday 0 14 gamestate.graphics in
    payday payraise_player
  else if
    gamestate.current_player.index_on_board < 57 && 57 <= player_index
  then
    let () = print_iter print_payday 0 14 gamestate.graphics in
    payday payraise_player
  else if
    gamestate.current_player.index_on_board < 64 && 64 <= player_index
  then
    let () = print_iter print_payday 0 14 gamestate.graphics in
    payday payraise_player
  else if
    gamestate.current_player.index_on_board < 79 && 79 <= player_index
  then
    let () = print_iter print_payday 0 14 gamestate.graphics in
    payday payraise_player
  else if
    gamestate.current_player.index_on_board < 86 && 86 <= player_index
  then
    let () = print_iter print_payday 0 14 gamestate.graphics in
    payday payraise_player
  else if
    gamestate.current_player.index_on_board < 92 && 92 <= player_index
  then
    let () = print_iter print_payday 0 14 gamestate.graphics in
    payday payraise_player
  else if
    gamestate.current_player.index_on_board < 105 && 105 <= player_index
  then
    let () = print_iter print_payday 0 14 gamestate.graphics in
    payday payraise_player
  else if
    gamestate.current_player.index_on_board < 109 && 109 <= player_index
  then
    let () = print_iter print_payday 0 14 gamestate.graphics in
    payday payraise_player
  else if
    gamestate.current_player.index_on_board < 120 && 120 <= player_index
  then
    let () = print_iter print_payday 0 14 gamestate.graphics in
    payday payraise_player
  else if
    gamestate.current_player.index_on_board < 127 && 127 <= player_index
  then
    let () = print_iter print_payday 0 14 gamestate.graphics in
    payday payraise_player
  else payraise_player

let new_deck_helper cards_options gamestate =
  match cards_options with
  | None, None -> gamestate.deck
  | Some x, None -> remove_first_instance x gamestate.deck []
  | Some x, Some y -> remove_first_instance x (y :: gamestate.deck) []
  | None, Some y -> y :: gamestate.deck

(** [lawsuit_tile_helper gamestate pay_player] returns an updated player
    list after a lawsuit occurs and a tuple of cards that needs to be
    added to or removed from the deck. *)
let lawsuit_tile_helper gamestate pay_player =
  print_iter print_lawsuit 0 7 gamestate.graphics;
  print_endline "";
  let player_sued = lawsuit_player gamestate.players pay_player in
  let exemption_card = has_exemption_card player_sued.deck in
  match exemption_card with
  | None ->
      Printf.printf "%s's Current Balance: %i \n" player_sued.name
        player_sued.account_balance;
      Printf.printf "%s has sued %s for $100,000 \n" pay_player.name
        player_sued.name;
      let new_balance_player = add_balance player_sued ~-100000 in
      Printf.printf "%s's Current Balance: %i \n"
        new_balance_player.name new_balance_player.account_balance;
      ([ pay_player; new_balance_player ], (None, None))
  | Some x ->
      Printf.printf "%s used an Exemption Card!" player_sued.name;
      ([ pay_player; remove_card x player_sued ], (None, Some x))

(** [take_tile_helper gamestate pay_player] returns an updated player
    list after pay_player lands on a take tile and a tuple of cards that
    needs to be added to or removed from the deck. *)
let take_tile_helper (gamestate : gamestate) (pay_player : player) =
  let spin_card_chosen = random_share_card gamestate.deck in
  match spin_card_chosen with
  | None -> ([ pay_player ], (None, None))
  | Some x ->
      ( [ { pay_player with deck = x :: pay_player.deck } ],
        (Some x, None) )

(** [house_tile_helper gamestate pay_player] returns an updated player
    list after pay_player lands on a house tile and a tuple of cards
    that needs to be added to or removed from the deck. *)
let house_tile_helper gamestate pay_player =
  let chosen_house = choose_houses pay_player gamestate.deck in
  print_iter print_house 0 9 gamestate.graphics;
  let house_name =
    match chosen_house with
    | House h -> Some h.name
    | _ -> None
  in
  match house_name with
  | Some x ->
      if x = "None" then
        ([ bought_house pay_player x gamestate.deck ], (None, None))
      else
        ( [ bought_house pay_player x gamestate.deck ],
          (Some chosen_house, None) )
  | None -> ([ pay_player ], (None, None))

(** [family_tile_helper pay_player index children] returns an updated
    player list after pay_player lands on a family tile and a tuple of
    cards that needs to be added to or removed from the deck. *)
let family_tile_helper pay_player index children graphics =
  if index = married_index then
    ([ { pay_player with so = true } ], (None, None))
  else if index = elope then
    ( [ { pay_player with so = true; index_on_board = married_index } ],
      (None, None) )
  else (
    print_iter print_children 0 5 graphics;
    ( [ { pay_player with children = pay_player.children + children } ],
      (None, None) ))

(** [career_tile_helper gamestate pay_player] returns an updated player
    list after pay_player lands on a career tile and a tuple of cards
    that needs to be added to or removed from the deck. *)
let career_tile_helper gamestate pay_player =
  let career_chosen = choose_career pay_player gamestate.deck in
  let had_career = has_career pay_player.deck in
  match had_career with
  | None ->
      ([ add_card career_chosen pay_player ], (Some career_chosen, None))
  | Some h ->
      ( [ exchange_card pay_player career_chosen h ],
        (Some career_chosen, Some h) )

let rec life_cards_in_deck (acc : cards list) (deck : cards list) =
  match deck with
  | [] -> acc
  | h :: t -> (
      match h with
      | Life_Tiles _ -> life_cards_in_deck (h :: acc) t
      | _ -> life_cards_in_deck acc t)

(** [career_tile_helper gamestate pay_player] returns an updated player
    list after pay_player lands on a life tile and a tuple of cards that
    needs to be added to or removed from the deck. *)
let life_tile_helper gamestate pay_player =
  print_iter Printing.print_life 0 11 gamestate.graphics;
  let life_tiles_in_deck = life_cards_in_deck [] gamestate.deck in
  let rand_lf_tile =
    List.nth_opt life_tiles (rand (List.length life_tiles_in_deck))
  in
  match rand_lf_tile with
  | None -> ([ pay_player ], (None, None))
  | Some v -> ([ add_card v pay_player ], (Some v, None))

let new_player_helper tile pay_player gamestate =
  match tile with
  | PayTile c ->
      ([ add_balance pay_player c.account_change ], (None, None))
  | TaxesTile _ -> ([ tax pay_player ], (None, None))
  | LifeTile _ -> life_tile_helper gamestate pay_player
  | CareerTile _ -> career_tile_helper gamestate pay_player
  | FamilyTile { name; account_change; index_tile; children } ->
      family_tile_helper pay_player index_tile children
        gamestate.graphics
  | HouseTile _ -> house_tile_helper gamestate pay_player
  | TakeTile _ -> take_tile_helper gamestate pay_player
  | ActionTile _ -> ([ pay_player ], (None, None))
  | SpinToWinTile _ ->
      let new_players = List.map player_spintowin gamestate.players in
      (new_players, (None, None))
  | LawsuitTile _ -> lawsuit_tile_helper gamestate pay_player

let rec turn gamestate : unit =
  print_endline "";
  Printf.printf "%s's Turn \n \nPlease enter any key to start: "
    gamestate.current_player.name;
  (* makes current player type in anything into terminal to start their
     turn *)
  start_turn ();
  (* print current player information *)
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
    let change_ind_tup = change_index_board gamestate.current_player in
    (*player with new index*)
    let player_moved = fst change_ind_tup in
    (* value of spinner *)
    let numSpun = snd change_ind_tup in
    let player_index = player_moved.index_on_board in
    (* prints wedding graphic if index of player is marriage tile*)
    let () =
      if player_index = married_index then
        print_iter print_wedding 0 14 gamestate.graphics
    in
    (* player with update payraise if a payraise tile is passed *)
    let payraise_player =
      payraise gamestate.current_player player_moved player_index
    in

    (* player with updated account balance if a payraise tile is
       passed *)
    let pay_player = pay gamestate payraise_player player_index in

    (* tile on which [paid_player] is on *)
    let tile = get_tile pay_player.index_on_board in
    print_tiles tile;

    let new_player = new_player_helper tile pay_player gamestate in

    (*[new_play_list] is the updated player list after the current
      player's turn*)
    let new_play_list =
      new_players_list gamestate.players (fst new_player)
    in
    let new_pos_lst = get_players new_play_list in
    let updated_board = update_board make_board new_pos_lst in

    print_board updated_board gamestate.current_player.colorblind;

    (* [new_deck] is the new game deck*)
    let new_deck = new_deck_helper (snd new_player) gamestate in

    (* returns new gamestate with updated records*)
    turn
      {
        gamestate with
        current_player = next_player pay_player new_play_list;
        players = check_investments numSpun new_play_list [];
        deck = new_deck;
      }
